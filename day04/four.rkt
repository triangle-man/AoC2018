#lang racket

;; A Date is a year, a month, and a day
(struct Date (year month day) #:transparent)

;; Note that 1518 was not a leap year
(define MONTH-LENGTH '(31 28 31 30 31 30 31 31 30 31 30 31))

;; Add one day to a date
(define (date-bump dt)
  (match-define (Date year month day) dt)
  (define month-length (list-ref MONTH-LENGTH month))
  (Date year
        month
        (modulo (+ day 1) (list-ref MONTH-LENGTH)))

  ;; An Oclock is an hour and a minute
  (struct Oclock (hour minute) #:transparent))

;; An Event is either a guard coming on duty, a falling-asleep or a waking-up
(struct Event (Date Oclock) #:transparent)
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
  (let ([year   (string->number yr)]
        [month  (string->number mn)]
        [day    (string->number dy)]
        [hour   (string->number hr)]
        [minute (string->number mi)])
    (cond
      [(string=? descr "wakes up") (Event/awaken year month day hour minute)]
      [(string=? descr "falls asleep") (Event/asleep year month day hour minute)]
      [else (Event/duty year month day hour minute (string->number guard))])))

(define events (map parse-event inputs))


;;; Part 1
;;; --------------------------------------------------------------------------------

;; Guards only come on duty the hour of, or the hour before, 00h.
;; four.rkt> (remove-duplicates (map Event-hour events))
;; '(0 23)

;; Step 1. Figure out 


;;; Part 2
;;; --------------------------------------------------------------------------------
