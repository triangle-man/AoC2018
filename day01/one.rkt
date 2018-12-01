#lang racket

;;; Part 1
;;; --------------------------------------------------------------------------------

;; inputs : Listof string?
(define inputs
  (with-input-from-file "input1.txt" port->lines))

;; deltas : Listof number?
(define deltas
  (map string->number inputs))

;; Answer to part 1
(println (foldl + 0 deltas))


;;; Part 2
;;; --------------------------------------------------------------------------------

;; Plan:
;; Find the first repeating element of an infinite stream ...
;; ... that is the partial sums ...
;; ... of the inputs (repeated indefinitely).

;; stream-repeat : stream? -> stream?
;; Make a stream that is the concatenation of infinite copies of a stream s.
;; Oddly not provided by Racket.
(define (stream-repeat s)
  (define (stream-repeat-aux current)
    (cond ([stream-empty? current] (stream-repeat-aux s))
          (else (stream-cons (stream-first current)
                             (stream-repeat-aux (stream-rest current))))))
  (stream-repeat-aux s))

;; partial-sums : Stream-of number? -> Stream-of number?
;; Compute the partial sums of a potentially infinite stream
;; starting with zero
(define (partial-sums s)
  (define (partial-sums-aux acc strm)
    (cond ([stream-empty? strm] empty-stream)
          (else (stream-cons acc (partial-sums-aux (+ acc (stream-first strm))
                                                   (stream-rest strm))))))
  (partial-sums-aux 0 s))

;; first-duplicate : stream? -> any/c?
;; Return the first element of the stream that has been seen already
(define (first-duplicate s)
  (define (first-duplicate-aux seen-so-far strm)
    (define next (stream-first strm))
    (if (set-member? seen-so-far next)
        next
        (first-duplicate-aux (set-add seen-so-far next) (stream-rest strm))))
  (first-duplicate-aux (set) s))

;; Answer to part 2
(println (first-duplicate (partial-sums (stream-repeat deltas))))
