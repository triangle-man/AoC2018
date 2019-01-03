#lang racket

;;; Answers
;;; --------------------------------------------------------------------------------

(module+ main
  (define *initial*
    (new-state
     "##.###.......#..#.##..#####...#...#######....##.##.##.##..#.#.##########...##.##..##.##...####..####"))
  (define *rules*
    (map parse-rule (with-input-from-file "rules.txt" port->lines)))
  (displayln (state-value (state-step-N *initial* *rules* 20)))
  (displayln (+ (state-value (state-step-N *initial* *rules* 400))
                (* (- 50000000000 400) 23))))

(module+ test
  (define *initial*
    (new-state
     "#..#.#..##......###...###"))
  (define *rules*
    (map parse-rule (with-input-from-file "test-rules.txt" port->lines)))
  (state-value (state-step-N *initial* *rules* 20)))

;;; Get input rules
;;; --------------------------------------------------------------------------------

(define RULE-RE #px"([#\\.]{5}) => ([#\\.])")

(define (parse-rule s)
  (let ([pots (cdr (regexp-match RULE-RE s))])
    (cons (list->vector (map char->pot (string->list (car pots))))
          (list->vector (map char->pot (string->list (cadr pots)))))))

;;; Types
;;; --------------------------------------------------------------------------------

;; A State is:
;; - a List-of of pots and
;; - an offset, indicating the numerical index of
;; the first element of the vector relative to the original element 0

(struct State (pots offset) #:transparent)

(define PLANT #\#)
(define NOPLANT #\.)

;; state-print : State? -> string?
(define (state->string st)
  (match-let ([(State pots offset) st])
    (let ([N (length pots)])
      (cond
        [(> offset 0)
         (string-append "|-" (number->string offset) "- "
                        (pots->string pots))]
        [(< offset (- N))
         (string-append (pots->string pots)
                        " -" (number->string (- 0 N offset)) "-|")]
        [else
         (let-values ([(left right) (split-at pots (- offset))])
           (string-append (pots->string left) "|" (pots->string right)))]))))

(define (pots->string pots)
  (list->string (map pot->char pots)))

(define (pot->char pot)
  (if pot
      PLANT
      NOPLANT))

(define (char->pot c)
  (char=? c PLANT))

;; new-state : string? -> State?
(define (new-state s)
  (State
   (map char->pot (string->list s))
   0))

;; Remove leading and trailing empty pots
(define (state-trim st)
  #f)

;; Remove leading empty pots
(define (state-trim/left st)
  (match-let ([(State pots offset) st])
    (if (or (null? pots) (car pots))
        st
        (state-trim/left (State (cdr pots) (+ offset 1))))))


;; A Rule is a pair of:
;; - a vector of pots of length 5; and
;; - a pot

;; A Rules? is a List-of Rule?, noting that it is also an association list

;;; Definitions
;;; --------------------------------------------------------------------------------

;; state-step-N : State? Rules? N -> State?
;; Move N timesteps
(define (state-step-N cur rules N)
  (for/fold ([st cur])
            ([i (in-range N)])
    (state-step st rules)))

;; state-step : State? Rules? -> State?
;; Move one timestep. Pad left and right by 5; start at position 2 and end at
;; the antepenultimate pot.
(define (state-step cur rules)
  (match-let ([(State pots offset) cur])
    (State (state-step/left-padded
            (list* #f #f #f #f #f pots)
            rules)
           (- offset 3))))

(define (state-step/left-padded pots rules)
  (if (null? pots)
      null
      (cons (match-rule (list->vector (take/padded pots 5 #f)) rules)
            (state-step/left-padded (cdr pots) rules))))

;; Return the left-most N elements of ls, padding with fill if necessary 
(define (take/padded ls N fill)
  (cond
    [(= N 0)    null]
    [(null? ls) (cons fill (take/padded null (- N 1) fill))]
    [else       (cons (car ls) (take/padded (cdr ls) (- N 1) fill))]))

;; match-rule : [List-of pot?] Rules -> pot?
(define (match-rule ps rules)
  (car (vector->list (cdr (assoc ps rules)))))

(define (string-match-rule s rules)
  (match-rule (list->vector (string->list s)) rules))

;; Counting values

(define (state-value st)
  (match-let ([(State pots offset) st])
    (for/sum ([i (in-range offset (+ offset (length pots)))]
              [p pots])
      (if p i 0))))
