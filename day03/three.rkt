#lang racket

;;; Part 1
;;; --------------------------------------------------------------------------------

;; inputs : List-of string?
(define inputs
  (with-input-from-file "input3.txt" port->lines))

(define CLAIM-RE
  #px"^#([[:digit:]]+) @ ([[:digit:]]+),([[:digit:]]+): ([[:digit:]]+)x([[:digit:]]+)")
