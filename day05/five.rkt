#lang racket

;;; Get input file
;;; --------------------------------------------------------------------------------

;; inputs : List-of char?
(define *inputs*
  (map integer->char
   (bytes->list
     (with-input-from-file "input5.txt" port->bytes))))

;;; Definitions
;;; --------------------------------------------------------------------------------

;; True for, eg, a A, or B b; false for a a, or a B
(define (can-react? u1 u2)
  (and (char-ci=? u1 u2)
       (or (and (char-lower-case? u1) (char-upper-case? u2))
           (and (char-upper-case? u1) (char-lower-case? u2)))))

(define (reaction-step left right)
  (cond
    [(null? right) left]
    [(null? left)  (reaction-step (list (car right)) (cdr right))]
    [else
     (if (can-react? (car left) (car right))
         (reaction-step (cdr left) (cdr right))
         (reaction-step (cons (car right) left) (cdr right)))]))

(define (react polymer)
  (reverse (reaction-step '() polymer)))


;;; Part 1
;;; --------------------------------------------------------------------------------

(define *output*
  (react *inputs*))

(length *output*)

;;; Part 2
;;; --------------------------------------------------------------------------------

(define *units* (sort (remove-duplicates (map char-downcase *inputs*)) char<?))


;; remove-unit : char? [List-of char?] -> [List-of char?]
(define (remove-unit unit polymer)
  (remq* (list (char-upcase unit) (char-downcase unit)) polymer))

(for-each
 (λ (unit)
   (printf "Unit: ~a => Length: ~a\n"
           unit
           (length (react (remove-unit unit *inputs*)))) *units*)
 *units*)
