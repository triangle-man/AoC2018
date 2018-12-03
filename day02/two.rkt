#lang racket

;;; Part 1
;;; --------------------------------------------------------------------------------

;; ids : List-of string?
(define ids
  (with-input-from-file "input2.txt" port->lines))

;; count-letters : string? -> hasheq? 
;; Given a string, return a dictionary of the counts of each character in the
;; string.
(define (count-letters s)
  (foldl tally-char (hasheq) (string->list s)))

;; tally-char
;; If `c` is not in the dictionary `counts`, add it; otherwise bump its counter.
(define (tally-char c counts)
  (if (hash-has-key? counts c)
      (hash-update counts c (curry + 1))
      (hash-set counts c 1)))

;; letter-counts : List-of hasheq?
(define letter-counts (map (compose hash-values count-letters) ids))

;; Count number of ids with at least one of its letter-counts being N
;; count-Ns : N List-of (List-of number?)
(define (count-Ns N nss)
  (length (filter (curry memq N) nss)))

;; Answer to part 1
(println (* (count-Ns 2 letter-counts) (count-Ns 3 letter-counts)))

;;; Part 
;;; --------------------------------------------------------------------------------

