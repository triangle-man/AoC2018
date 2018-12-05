#lang racket

;; A Date is a year, a month, and a day
(struct Date (year month day) #:transparent)

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

;; An Oclock is an hour and a minute
(struct Oclock (hour minute) #:transparent)

;; An Event is either a guard coming on duty, a falling-asleep or a waking-up
(struct Event (dt oc) #:transparent)
(struct Event/duty Event (guard) #:transparent)
(struct Event/asleep Event () #:transparent)
(struct Event/awaken Event () #:transparent)


;;; Get input file
;;; --------------------------------------------------------------------------------

;; inputs : List-of string?
(define inputs
  (with-input-from-file "input4.txt" port->lines))

(define EVENT-RE
  (pregexp
   (string-join
    '("\\[([[:digit:]]{4})-([[:digit:]]{2})-([[:digit:]]{2})"
      "([[:digit:]]{2}):([[:digit:]]{2})\\]"
      "(Guard #([[:digit:]]+) begins shift|falls asleep|wakes up)"))))

;; events : List-of Event?
(define (parse-event s)
  (match-define (list yr mn dy hr mi descr guard)
    (cdr (regexp-match EVENT-RE s)))
  (let ([dt (Date (string->number yr) (string->number mn) (string->number dy))]
        [tm (Oclock (string->number hr) (string->number mi))])
    (cond
      [(string=? descr "wakes up") (Event/awaken dt tm)]
      [(string=? descr "falls asleep") (Event/asleep dt tm)]
      [else (Event/duty dt tm (string->number guard))])))

(define events (map parse-event inputs))


;;; Part 1
;;; --------------------------------------------------------------------------------

;; Guards only come on duty the hour of, or the hour before, 00h.
;; four.rkt> (remove-duplicates (map Event-hour events))
;; '(0 23)

;; Step 1. Figure out which guard is on duty on a particular day. When a guard
;; comes on duty in the 23hr hour, their duty date is the next day.

(define (maybe-inc-date dt oc)
  (if (= (Oclock-hour oc) 23)
      (date-add1 dt)
      dt))

;; roster : List-of (Date . number?), where number is the guard's id
(define roster
  (map (λ (ev) (cons (maybe-inc-date (Event-dt ev) (Event-oc ev)) (Event/duty-guard ev)))
       (filter Event/duty? events)))

;; Step 2. For each day, put the events in order and compute the times at which
;; guards were asleep or awake

;;; Part 2
;;; --------------------------------------------------------------------------------
