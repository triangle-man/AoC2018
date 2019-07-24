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
  (vector-set! (Grid-v grid) (grid-coord->vector-pos grid x y) v))

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

(define *coords* (map parse-coord *inputs*))


;;; Part 1
 ;;; --------------------------------------------------------------------------------


;; This is ugly, but I'm going to assume that the important areas are bounded by
;; 0,0 - 399,399 and that any points whose influence extends to those boundaries
;; have infinite area and should be excluded.

(define XSIZE 400)
(define YSIZE 400)

(define *map* (make-grid XSIZE YSIZE))

;; nearest-neighbours : Pos? [Vector-of Pos?] -> [List-of Integer?]
;; Return either the index in points of nearest neighbour or #f if a tie
(define (nearest-neighbour pos points)
  (let* ([distances (map (curry pos-distance pos) points)]
         [smallest (argmin values distances)]
         [nearest (indexes-of distances smallest)])
    (if (null? (cdr nearest)) ;; if there's only one value
        (car nearest)         ;; return it,
        #f)))                 ;; or #f if there's a tie

;; Make a map of the nearest *coord* to each point in the grid 
(for* ([y (in-range YSIZE)]
       [x (in-range XSIZE)])
  (grid-set! *map* x y (nearest-neighbour (Pos x y) *coords*))
  (when (and (= 0 (modulo y 20)) (= x 0)) (display "\n"))
  (when (and (= 0 (modulo x 20)) (= 0 (modulo y 20))) (display "."))
)

;; Compute sizes of all areas (in the bounded grid)

;; *counts* : hasheq? of the id of each coord and its count
(define (hash-zero n)
  (for/hasheq ([i (in-range n)])
    (values i 0)))

;; hash-counter-add1 : hash? any? -> hash?
;; Bump the value of the associated key by 1, or set to 1 if it's not there.
(define (hash-counter-add1 key hash) 
  (hash-update hash key (λ (n) (+ n 1)) 1))

(define *counts*
  (foldl hash-counter-add1
         (hash-zero (length *coords*))
         (vector->list (Grid-v *map*))))

;; Which are the largest?
(sort (hash->list *counts*) < #:key cdr)

;; But also, which ones are on the boundary? 
(printf "x : ~a - ~a\n"
        (argmin Pos-x *coords*)
        (argmax Pos-x *coords*))

(printf "y : ~a - ~a\n"
        (argmin Pos-y *coords*)
        (argmax Pos-y *coords*))

; (indexes-where *coords* (λ (p) (= (Pos-x p) (Pos-x (argmin Pos-x *coords*)))))
; (indexes-where *coords* (λ (p) (= (Pos-x p) (Pos-x (argmax Pos-x *coords*)))))
; (indexes-where *coords* (λ (p) (= (Pos-y p) (Pos-y (argmin Pos-y *coords*)))))
; (indexes-where *coords* (λ (p) (= (Pos-y p) (Pos-y (argmax Pos-y *coords*)))))

;; New plan: reject any point for which there is a point on the boundary of the
;; grid for which that is the largest point.


(remove-duplicates
 (flatten
  (for/list ([i (in-range XSIZE)])
    (list (grid-ref *map* 0 i)
          (grid-ref *map* (- XSIZE 1) i)
          (grid-ref *map* i 0)
          (grid-ref *map* i (- YSIZE 1))))))


;; Worked!

;;; Part 2
;;; --------------------------------------------------------------------------------
;;; Find coords whose aggregate distance to other points is < 10,000

;; Pos? [List-of Pos?] -> number?
(define (aggregate-distance-to pos points)
  (for/sum ([p (in-list points)])
    (pos-distance pos p)))

;; Make a map of all aggregate distances
(define *map-dist* (make-grid XSIZE YSIZE))

(for* ([y (in-range YSIZE)]
       [x (in-range XSIZE)])
  (grid-set! *map-dist* x y (aggregate-distance-to (Pos x y) *coords*))
  (when (and (= 0 (modulo y 20)) (= x 0)) (display "\n"))
  (when (and (= 0 (modulo x 20)) (= 0 (modulo y 20))) (display ".")))

;; Count how many are < 10,000
(for*/sum ([i (in-range XSIZE)]
          [j (in-range YSIZE)])
  (if (< (grid-ref *map-dist* i j) 10000) 1 0))

;; Yay!
