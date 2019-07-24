#lang racket

;;; Get input file
;;; --------------------------------------------------------------------------------

;; inputs : List-of string?
(define *inputs*
  (with-input-from-file "input7.txt" port->lines))

(define INSTRUCTION-RE
  (pregexp "Step ([[:upper:]]) must be finished before step ([[:upper:]]) can begin."))

(define (parse-instruction s)
  (let ([pr (cdr (regexp-match INSTRUCTION-RE s))])
    (cons (car (string->list (car pr)))
          (car (string->list (cadr pr))))))

;; goals are pairs (from . to) where from -> to
;; We add a fake terminal, #f, for convenience
(define *goals*
  (let* ([goals (map parse-instruction *inputs*)]
         [antecedents (remove-duplicates (map car goals))]
         [subsequents (remove-duplicates (map cdr goals))]
         [terminals (remove* antecedents subsequents)])
    (append goals (map (λ (x) (cons x #f)) terminals))))

(sort (remove-duplicates (filter-map car *goals*)) char<?)   ; No Q
(sort (remove-duplicates (filter-map cdr *goals*)) char<?)   ; No B, F, K, V


;;; Part 1
;;; --------------------------------------------------------------------------------

;; open-steps : hasheq? -> List-of [step]
;; An open step x is one which has no immediate antecedent; ie, there is no y
;; such that (x, y) is in the hash
(define (open-steps goals)
  (remove-duplicates (filter (curry open-step? goals) (map car goals))))

(define (open-step? goals step)
  (null? (immediate-antecedents goals step)))

;; immediate-antecedents : [List-of goals] -> [List-of step]
;; The immediate antecedents of x are those y such that (y . x) is in goals
(define (immediate-antecedents goals step)
  (map car
       (filter (compose1 (curry equal? step) cdr) goals)))

;; Remove all goals that have step as an antecedent
(define (remove-antecedents goals step)
  (filter-not (λ (g) (char=? step (car g))) goals))

;; To compute the sequence of steps:
;; 1. Find the open-steps of goals
;; 2. Choose the alphabetically first
;; 3. Accumulate that
;; 4. Start again with that step removed as a precedent

(define (goals->order goals)
  (let ([opens (open-steps goals)])
    (if (null? opens)
        null
        (let ([first-open (step-smallest opens)])
          (cons first-open (goals->order (remove-antecedents goals first-open)))))))

(define (step-smallest steps)
  (car (sort steps char<?)))

(list->string (goals->order *goals*))

;;; Part 2
;;; --------------------------------------------------------------------------------

(struct World (workers clock) #:transparent)
(struct Job (step duration) #:transparent)

;; Set-up for the example input
(define DURATION-OFFSET (- 61 (char->integer #\A)))

;; A world-state is a pair (workers . clock)
;; workers is a vector-of (or/c job #f), where #f means that this worker is available.
;; A job is a pair (step . time-remaining)
(define world-state-init (World (vector #f #f #f #f #f) 0))

(define (step-duration step)
  (+ DURATION-OFFSET (char->integer step)))


;; Here's the plan.
;; 1. Start with a World state consisting of free workers and goals.

;; Update:
;; 1. If there are no free workers, then tick.
;; 2. If there are any open goals that are not being worked on, then tock.
;; 3. If there are any assigned workers, then tick.
;; 4. Otherwise, there are no workers and no goals; so return state.

;; Tick:
;; 1. Find the worker with the quickest job remaining. Finish that job, release
;;    the worker; update the clock; remove that job from the goals; and go back
;;    to update.

;; Tock:
;; 1. Pull the next goal; assign it to a free worker; then back to update.

(define (world-update state goals)
  (printf "world-update: ~v\n" state)
  (let ([worker (free-worker (World-workers state))])
    (if (not worker)                 ; No worker is free ...
        (world-tick state goals)     ; ... so just move time forward
        (let ([goal (open-goal (World-workers state) goals)])
          (cond [goal                                  ; There is an open goal ... 
                 (world-tock worker goal state goals)] ; ... so assign it
                [(> (vector-count values (World-workers state)) 0)
                 ; At least one worker is engaged ...
                 (world-tick state goals)]   ; ... so move time forward
                [else state])))))

;; Execute the quickest job
(define (world-tick state goals)
  (let* ([workers      (World-workers state)]
         [quickest-job (find-quickest workers)])
    (match-let ([(Job step duration) quickest-job])
      (world-update
       (World
        (vector-map (curry deduct-time-and-free duration) workers)
        (+ (World-clock state) duration))
       (remove-antecedents goals step)))))

;; Assign an available job to an available worker
(define (world-tock worker step state goals)
  (vector-set! (World-workers state) worker (Job step (step-duration step)))
  (world-update state goals))

;; Find the next available goal that is not being worked on, or #f if there are
;; none.
(define (open-goal workers goals)
  (let* ([nexts      (open-steps goals)]
         [opens      (remove* (busy-steps workers) nexts)])
    (if (null? opens)
        #f
        (step-smallest opens))))

;; busy-steps
;; List the steps currently being worked on
(define (busy-steps workers)
  (filter-map (λ (maybe-job)
                (match maybe-job
                  [#f #f]
                  [(Job step duration) step]))
              (vector->list workers)))


;; Find the indices of the free workers (or #f if there are non free)
(define (free-worker workers)
  (vector-memq #f workers))

(define (goals-empty? goals)
  (null? goals))

;; finish-quickest-job : World? -> World?
;; If any job has more than 0 time remaining, find the job with the least time
;; remaining and execute that, updating the time remaiing of all the other jobs,
;; and incrementing the world clock.
(define (Xcomplete-quickest-job state)
  (let* ([workers      (World-workers state)]
         [quickest-job (find-quickest workers)]  ;; errors if all workers are free
         [duration     (Job-duration quickest-job)])
    (World
     (vector-map (curry deduct-time-and-free duration) workers)
     (+ (World-clock state) duration))))

;; Return the job with least time remaining, or #f if all workers are free.
(define (find-quickest workers)
  (for/fold ([quickest #f])
            ([job (in-vector workers)])
    (cond
      [(not job) quickest]
      [(not quickest) job]
      [(< (Job-duration quickest) (Job-duration job)) quickest]
      [else job])))

;; Delete duration time from each job, assuming that each job has at least
;; duration left. For any jobs that result in 0 time remaining, free up that worker
(define (deduct-time-and-free time job)
  (if (not job)
      #f
      (match-let ([(Job step remaining) job])
        (let ([result (- remaining time)])
          (if (= 0 result)
              #f
              (Job step result))))))


(world-update world-state-init *goals*)
