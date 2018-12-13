#lang racket

;; A Date is a year, a month, and a day
(struct Date (year month day) #:transparent)

(define (Date->string dt)
  (string-join
   (list  (number->string (Date-year dt))
          (two-up         (Date-month dt))
          (two-up         (Date-day dt)))
   "-"))

(define (two-up N)
  (let ([N$ (number->string N)])
    (if (< N 10)
        (string-append "0" N$)
        N$)))

;; Note that 1518 was not a leap year
(define MONTH-LENGTH '(31 28 31 30 31 30 31 31 30 31 30 31))

;; Add one day to a date
(define (date-add1 dt)
  (match-define (Date year month day) dt)
  (define month-length (list-ref MONTH-LENGTH (- month 1)))
  (define-values (month-inc new-day)
    (quotient/remainder day month-length))
  (define-values (year-inc new-month)
    (quotient/remainder (+ (- month 1) month-inc) 12))
  (define new-year (+ year year-inc))
  (Date new-year
        (+ new-month 1)
        (+ new-day 1)))

;; Test for stricty ordering of dates 
(define (Date<? dt1 dt2)
  (or (< (Date-year dt1) (Date-year dt2))
      (and (= (Date-year dt1) (Date-year dt2))
           (or (< (Date-month dt1) (Date-month dt2))
               (and (= (Date-month dt1) (Date-month dt2))
                    (< (Date-day dt1) (Date-day dt2)))))))

;; An Oclock is an hour and a minute
(struct Oclock (hour minute) #:transparent)

(define (Oclock->string oc)
  (string-join
   (list (two-up (Oclock-hour oc)) (two-up (Oclock-minute oc)))
   "h"))

(define (Oclock<? oc1 oc2)
  (or (< (Oclock-hour oc1) (Oclock-hour oc2))
      (and (= (Oclock-hour oc1) (Oclock-hour oc2))
           (< (Oclock-minute oc1) (Oclock-minute oc2)))))

;; An Event is either a guard coming on duty, a falling-asleep or a waking-up
(struct Event (dt oc) #:transparent)  ; dt : Date?, oc : Oclock?
(struct Event/duty Event (guard) #:transparent)
(struct Event/asleep Event () #:transparent)
(struct Event/awaken Event () #:transparent)

(define (Event<? ev1 ev2)
  (or (Date<? (Event-dt ev1) (Event-dt ev2))
      (and (equal? (Event-dt ev1) (Event-dt ev2))
           (Oclock<? (Event-oc ev1) (Event-oc ev2)))))

;; A Nap is an interval (closed on the left, open on the right),
;; such that stop > start, or #f
(struct Nap (start stop) #:transparent)

(define (Nap-length nap)
  (if nap
      (- (Nap-stop nap) (Nap-start nap))
      0))

;; Nap-intersect : Nap? Nap? -> List-of Nap?
(define (Nap-intersect nap1 nap2)
  (cond
    [(<= (Nap-stop nap1) (Nap-start nap2)) null]
    [(<= (Nap-stop nap2) (Nap-start nap1)) null]
    [else (list (Nap (max (Nap-start nap1) (Nap-start nap2))
                     (min (Nap-stop  nap1) (Nap-stop  nap2))))]))

;; Nap-diff : Nap? Nap? -> List-of Nap?
;; Computes nap1 - nap2. Returns a list of zero, one, or two naps
(define (Nap-diff nap1 nap2)
  (filter-map values
              (list (make-nap (Nap-start nap1) (min (Nap-start nap2) (Nap-stop nap1)))
                    (make-nap (max (Nap-start nap1) (Nap-stop nap2)) (Nap-stop nap1)))))

(define (make-nap left right)
  (if (< left right)
      (Nap left right)
      #f))

;; Nap-disjoint? : List-of Nap? -> Bool?
(define (Nap-disjoint? naps)
  (if (null? naps)
      #t
      (error "Nap-disjoint not implemented")))

;; A Nod is a list of non-overlapping Naps
(define (Nod? naps)
  (and (andmap Nap? naps)
       (Nap-disjoint? naps)))

(define (Nod-compress naps)
  (foldr
   Nod-compress/aux
   '()
   (sort naps < #:key Nap-start)))

(define (Nod-compress/aux nap total)
  (if (null? total)
      (list nap)
      (if (= (Nap-stop nap) (Nap-start (car total)))
          (cons (Nap (Nap-start nap) (Nap-stop (car total))) (cdr total))
          (cons nap total))))

;; nod-union : List-of Nap? -> List-of [Pair Nap? Number?]
;; A weighted Nod is a list of pairs of naps and counts
(define (nod-union naps)
  (wnod-add/all (map (make-wnapper 1) naps) null))

;; -> (Nap? -> Pair Nap? Number?)
(define (make-wnapper N)
  (λ (nap) (cons nap N)))

;; wnod-add/one : wnod wnap -> wnod
(define (wnod-add/one wnaps wnap)
  (if (null? wnaps)
      (list wnap)
      (match-let ([(cons this-nap this-count) wnap]
                  [(cons next-nap next-count) (car wnaps)])
        (let ([left  (map (make-wnapper this-count)
                          (Nap-diff this-nap next-nap))] 
              [right (map (make-wnapper next-count)
                          (Nap-diff next-nap this-nap))] 
              [both  (map (make-wnapper (+ this-count next-count))
                          (Nap-intersect this-nap next-nap))])
          (append right both (wnod-add/all left (cdr wnaps)))))))

;; wnod-add/aux : wnod wnod -> wnod
(define (wnod-add/all todo done)
  (if (null? todo)
      done
      (wnod-add/all (cdr todo) (wnod-add/one done (car todo)))))

;;; Get input file
;;; --------------------------------------------------------------------------------

;; inputs : List-of string?
(define *inputs*
  (with-input-from-file "input4.txt" port->lines))

(define EVENT-RE
  (pregexp
   (string-join
    '("\\[([[:digit:]]{4})-([[:digit:]]{2})-([[:digit:]]{2})"
      "([[:digit:]]{2}):([[:digit:]]{2})\\]"
      "(Guard #([[:digit:]]+) begins shift|falls asleep|wakes up)"))))

(define (parse-event s)
  (match-define (list yr mn dy hr mi descr guard)
    (cdr (regexp-match EVENT-RE s)))
  (let ([dt (Date (string->number yr) (string->number mn) (string->number dy))]
        [tm (Oclock (string->number hr) (string->number mi))])
    (cond
      [(string=? descr "wakes up")     (Event/awaken dt tm)]
      [(string=? descr "falls asleep") (Event/asleep dt tm)]
      [else                            (Event/duty dt tm (string->number guard))])))

;; events : List-of Event?
(define *events* (map parse-event *inputs*))


;;; Part 1
;;; --------------------------------------------------------------------------------

;; Guards only come on duty the hour of, or the hour before, 00h.
;; > (remove-duplicates (map Event-hour events))
;; '(0 23)

;; Step 1. Figure out which guard is on duty on a particular day. When a guard
;; comes on duty in the 23rd hour, their duty date is the next day.

(define (maybe-inc-date dt oc)
  (if (= (Oclock-hour oc) 23)
      (date-add1 dt)
      dt))

;; roster : List-of (Date . number?), where number is the guard's id
;; An association map, mapping dates to guards
(define *roster*
  (map (λ (ev) (cons (maybe-inc-date (Event-dt ev) (Event-oc ev)) (Event/duty-guard ev)))
       (filter Event/duty? *events*)))

;; Step 2. For each day, put the events in order and compute the times at which
;; guards were asleep or awake

;; Construct a hash table from a list in the following way:
;; For each element of the map
;; 1. Extract a key from the element with extract-key
;; 2. Functionally update the hash table using update-value applied to the
;;    existing value and extract-value applied to the element; or
;; 3. If there is not a value, use set-value applied to the result of 

(define (list->hash ls extract-key extract-value update-value initial)
  (for/fold ([ht (hash)])
            ([elem ls])
    (hash-update ht
                 (extract-key elem)
                 (curry update-value (extract-value elem))
                 initial)))

(define *cal*
  (list->hash (filter (compose1 not Event/duty?) *events*)
              Event-dt ; key extractor
              values   ; identity function is value extractor
              (λ (cur val) (cons cur val)) ; just make a list of the events
              null))

;; The calendar is an association list of dates and Nods.
(define (events->nod evs)
  (Nod-compress
   (let add-next-nap ([nod '()] [es (sort evs Event<?)])
     (if (null? es)
         nod
         (if (not (and (Event/asleep? (car es))
                       (Event/awaken? (cadr es))))
             (error "que?" es)
             (add-next-nap
              (cons (Nap (Oclock-minute (Event-oc (car es)))
                         (Oclock-minute (Event-oc (cadr  es))))
                    nod)
              (cddr es)))))))

(define *calendar*
  (hash-map *cal*
            (λ (day evs) (cons day (events->nod evs)))))

;; Nod? -> Number?
;; Given a Nod, count the minutes asleep. 
(define (count-mins naps)
  (foldl (λ (nap total) (+ (Nap-length nap) total)) 0 naps))

(map (λ (ps) (foldl (λ (p total) (cons (car p) (+ (cdr total) (cdr p)))) '(#f . 0) ps))
 (group-by car ;; group by guard ids
           (for/list ([day (in-list *calendar*)])
             ;; day is a pair, consisting of a Date and a Nod? 
             (cons (cdr (assoc (car day) *roster*))
                   (count-mins (cdr day))))))

;; '((337 . 460)
;;  (3019 . 180)
;;  (1367 . 191)
;;  (2341 . 162)
;;   (439 . 146)
;;  (2617 . 325)
;;  (1973 . 386)
;;   (631 . 197)
;;   (499 . 471)
;;  (2089 . 181)
;;    (89 . 374)
;;   (787 . 294)
;;  (1951 . 202)
;;  (2243 . 211)
;;  (3449 . 355)
;;  (3119 . 151)
;;  (2791 . 302)
;;   (947 . 455)
;;   (809 . 303)
;;  (1481 . 285))
;;

;; Looks like Guard 499!

;; *schedule* is like *calendar* except the dates have been replaced by guard ids
(define *schedule*
  (map
   (λ (day)
     (cons (cdr (assoc (car day) *roster*)) (cdr day)))
   *calendar*))

(nod-union
 (flatten (map cdr (filter (λ (day) (= (car day) 499)) *schedule*))))


;;; Part 2
;;; --------------------------------------------------------------------------------

(map
 (λ (scheds)
   (cons (caar scheds) (argmax cdr (nod-union (flatten (map cdr scheds))))))
 (group-by car *schedule*)) 
