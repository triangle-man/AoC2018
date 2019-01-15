#lang racket

(provide make-grid
         grid-size
         grid-ref
         grid-set!
         vector->grid
         lists->grid
         grid->lists
         grid-map
         (struct-out Grid))

#|

A Grid is a two-dimensional array, implemented as a vector and laid out row-wise

|#

(struct Grid (w h v) #:transparent)

(define (make-grid width height #:fill [fill #f])
  (Grid width height (make-vector (* width height) fill)))

(define (grid-coord->vector-pos grid col row)
  (+ col (* row (Grid-w grid))))

(define (grid-size g)
  (* (Grid-w g) (Grid-h g)))

(define (grid-ref grid col row)
  (vector-ref (Grid-v grid) (grid-coord->vector-pos grid col row)))

(define (grid-set! grid col row v)
  (vector-set! (Grid-v grid) (grid-coord->vector-pos grid col row) v))

(define (vector->grid v width)
  (Grid width (quotient (vector-length v) width) (vector-copy v)))

;; List-of List-of any/c? -> Grid?
(define (lists->grid xss)
  (let ([width (length (car xss))]
        [vs    (list->vector (apply append xss))])
    (vector->grid vs width)))

;; Grid? -> List-of List-of any/c?
(define (grid->lists g)
  (let* ([width (Grid-w g)]
         [ls    (vector->list (Grid-v g))])
    (split-by ls width)))

(define (split-by xs n)
  (if (null? xs)
      null
      (cons (take xs n) (split-by (drop xs n) n))))

;; Like vector-map for grids
(define (grid-map proc g)
  (struct-copy Grid g [v (vector-map proc (Grid-v g))]))
