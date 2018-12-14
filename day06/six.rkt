#lang racket

(struct Pos (x y) #:transparent)

;; Manhattan distance
(define (pos-distance p1 p2)
  (+ (abs (- (Pos-x p1) (Pos-x p2)))
     (abs (- (Pos-y p1) (Pos-y p2)))))

;; A two-dimensional array, implemented as a vector
(struct Grid (w h v) #:transparent)

(define (make-grid width height)
  (Grid width height (make-vector (* width height))))

(define (grid-coord->vector-pos grid x y)
  (+ x (* y (Grid-w grid))))

(define (grid-size g)
  (* (Grid-w g) (Grid-h g)))

(define (grid-ref grid x y)
  (vector-ref (Grid-v grid) (grid-coord->vector-pos grid x y)))

(define (grid-set! grid x y v)
  (vector-set! (Grid-v grid) (grid-coord->vector-pos grid x y)))

(define (vector->grid v width)
  (Grid width (quotient (vector-length v) width) (vector-copy v)))

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

(define *coords* (list->vector (map parse-coord *inputs*)))


;;; Part 1
;;; --------------------------------------------------------------------------------

(printf "x : ~a - ~a\n"
        (vector-argmin Pos-x *coords*)
        (vector-argmax Pos-x *coords*))

(printf "y : ~a - ~a\n"
        (vector-argmin Pos-y *coords*)
        (vector-argmax Pos-y *coords*))

;; This is ugly, but I'm going to assume that the important areas are bounded by
;; 0,0 - 399,399 and that any points whose influence extends to those boundaries
;; have infinite area and should be excluded.

(define XSIZE 400)
(define YSIZE 400)

(define *map* (make-grid XSIZE YSIZE))

; nearest-neighbour : Pos? [Vector-of Pos?] -> Integer?
;; Return either the index in points of nearest neighbour or #f if a tie
(define (nearest-neighbour pos points)
  (vector-map (curry pos-distance pos) points)
  )
