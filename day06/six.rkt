#lang racket

(struct Pos (x y) #:transparent)


;;; Get input file
;;; --------------------------------------------------------------------------------

;; inputs : List-of string?
(define *inputs*
  (with-input-from-file "input6.txt" port->lines))

(define COORD-RE
  (pregexp "([[:digit:]]+), ([[:digit:]]+)"))

(define (parse-coord s)
  (match-let ([(list x y) (cdr (regexp-match COORD-RE s))])
    (Pos (string->number x) (string->number y))))

(define *coords* (map parse-coord *inputs*))


;;; Part 1
;;; --------------------------------------------------------------------------------

