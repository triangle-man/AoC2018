#lang racket
(require rackunit) ;; for tests

;; In this puzzle, coords are 1 .. 300 x 1 .. 300

;; cell rackID = x-coord + 10
;; power =
;;   (rackID * y-coord + *grid-serial-number*) * rackID
;;   keep 100s digit
;;; - 5

;; Part I
(module+ main
  (which-max-n-by-n 3 (make-power-grid 300 300 1133)))

;; Part II
(module+ main
  (which-max (make-power-grid 300 300 18)))

;;; Types
;;; --------------------------------------------------------------------------------

;; Grids
;; -----

;; A two-dimensional array, implemented as a vector
(struct Grid (w h v) #:transparent)

(define (make-grid width height)
  (Grid width height (make-vector (* width height) 0)))

(define (grid-coord->vector-pos grid x y)
  (+ x (* y (Grid-w grid))))

(define (grid-size g)
  (* (Grid-w g) (Grid-h g)))

(define (grid-ref grid x y)
  (vector-ref (Grid-v grid) (grid-coord->vector-pos grid x y)))

(define (grid-set! grid x y v)
  (vector-set! (Grid-v grid) (grid-coord->vector-pos grid x y) v))

(define (vector->grid v width)
  (Grid width (quotient (vector-length v) width) (vector-copy v)))


;;; Definitions
;;; --------------------------------------------------------------------------------

(define (cell-power x-game y-game serial-number)
  (let ([rack-id (+ x-game 10)])
        (-
         (modulo (quotient (* (+ (* rack-id y-game) serial-number) rack-id)
                           100)
                 10)
         5)))

(module+ test
  (check-equal? (cell-power 3 5 8) 4)
  (check-equal? (cell-power 122 79 57) -5)
  (check-equal? (cell-power 217 196 39) 0)
  (check-equal? (cell-power 101 153 71) 4))


(define (make-power-grid width height serial-number)
  (define g (make-grid width height))
  (for* ([y (in-range height)]
         [x (in-range width)])
    (grid-set! g x y (cell-power (+ x 1) (+ y 1) serial-number)))
  g)

;; Find the total power of an nxn grid whose top-left is x, y
;; (where x and y are 0-indexed)
(define (total-n-by-n n grid x y)
  (for*/sum ([yy (in-range y (+ y n))]
             [xx (in-range x (+ x n))])
    (grid-ref grid xx yy)))

;; Finds the top-left coords of the nxn grid that maximises power
;; Assumes that max power > 0
(define (which-max-n-by-n n grid)
  (for*/fold ([max-x 0]
              [max-y 0]
              [max-power 0])
             ([y (in-range (- (Grid-h grid) (- n 1)))]
              [x (in-range (- (Grid-w grid) (- n 1)))])
    (let ([this-power (total-n-by-n n grid x y)])
      (if (> this-power max-power)
          (values x y this-power)
          (values max-x max-y max-power)))))

;; Finds the nxn grid with the largest power
;; Assumes max power > 0
(define (which-max grid)
  (for/fold ([max-x 0]
             [max-y 0]
             [max-N 0]
             [max-power 0])
            ([N (in-range 1 (+ (Grid-w grid) 1))])
    (let-values ([(results cpu-time wall-time gc-time)
                  (time-apply which-max-n-by-n (list N grid))])
      (match-let ([(list this-x this-y this-power) results]) 
        (printf "N = ~s (~s ms): ~s,~s,~s" N cpu-time this-x this-y this-power)
        (when (> this-power max-power)
          (print "(best so far)"))
        (print "\n")
        (if (> this-power max-power)
            (values this-x this-y N this-power)
            (values max-x max-y max-N max-power))))))

(define (quick-which-max grid)
  (for ([N (in-range 1 (+ (Grid-w grid) 1))])
   (let-values ([(results cpu-time wall-time gc-time)
                  (time-apply which-max-n-by-n (list N grid))])
      (match-let ([(list this-x this-y this-power) results]) 
        (printf "N = ~s (~s ms): ~s,~s,~s\n" N cpu-time this-x this-y this-power)))))


;; Old, rewritten
;; --------------------------------------------------------------------------------

;; Find the total power of 3x3 grid whose top-left is x, y
;; (where x, y go 0 ... N - 1
(define (total-three-by-three grid x y)
  (+ (grid-ref grid x       y)
     (grid-ref grid (+ x 1) y)
     (grid-ref grid (+ x 2) y)
     (grid-ref grid x       (+ y 1))
     (grid-ref grid (+ x 1) (+ y 1))
     (grid-ref grid (+ x 2) (+ y 1))
     (grid-ref grid x       (+ y 2))
     (grid-ref grid (+ x 1) (+ y 2))
     (grid-ref grid (+ x 2) (+ y 2))))

;; Finds the top-left coords of the 3x3 grid that maximises power
;; Assumes that max power > 0
(define (which-max-three-by-three grid)
  (for*/fold ([max-x 0]
              [max-y 0]
              [max-power 0])
             ([y (in-range (- (Grid-h grid) 2))]
              [x (in-range (- (Grid-w grid) 2))])
    (let ([this-power (total-n-by-n 3 grid x y)])
      (if (> this-power max-power)
          (values x y this-power)
          (values max-x max-y max-power)))))


