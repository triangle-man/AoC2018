#lang racket


;;; Answers
;;; --------------------------------------------------------------------------------

(module+ main
  (define *stars*
    (map parse-star
         (with-input-from-file "input10.txt" port->lines))))

;;; Get input file
;;; --------------------------------------------------------------------------------

(define STAR-RE
  (let ([NUM-RE "[[:blank:]]*(-?[[:digit:]]+)"])
    (pregexp
     (string-append "position=<"
                    NUM-RE "," NUM-RE
                    "> velocity=<"
                    NUM-RE "," NUM-RE
                    ">"))))

(define (parse-star str)
  (let ([star (cdr (regexp-match STAR-RE str))])
    (Star (Pos (string->number (car star))
               (string->number (cadr star)))
          (Vel (string->number (caddr star))
               (string->number (cadddr star))))))

;;; Types
;;; --------------------------------------------------------------------------------

;; Positions and velocities
;; ------------------------

(struct Pos (x y) #:transparent)
(struct Vel (x y) #:transparent)
(struct Star (pos vel) #:transparent)

;; Star? -> number?
(define (coord-x st)
  (Pos-x (Star-pos st)))

(define (coord-y st)
  (Pos-y (Star-pos st)))

;; Grids
;; -----

;; A two-dimensional array, implemented as a vector
(struct Grid (w h v) #:transparent)

(define (make-grid width height)
  (Grid width height (make-vector (* width height) #f)))

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

;;; bounding-box : List-of Star? -> Pair Pos? Pos?
;;; Find top-left and bottom-right
(define (bounding-box stars)
  (let ([xs (map coord-x stars)]
        [ys (map coord-y stars)])
    (cons
     (Pos (apply min xs)
          (apply min ys))
     (Pos (apply max xs)
          (apply max ys)))))

;; evolve : List-of Star? -> List-of Star?
(define (evolve delta stars)
  (map (curry evolve-star delta) stars))

(define (evolve-star delta star)
  (match-let ([(Star (Pos x y) (Vel xv yv)) star])
    (Star
     (Pos (+ x (* delta xv))
          (+ y (* delta yv)))
     (Star-vel star))))

(define (print-stars stars)
  (print-matrix (stars->matrix stars)))

;; Print "#" for every true value, "." for every false, with newline between rows
(define (print-matrix matrix)
  (for* ([row (in-range (Grid-h matrix))]
         [col (in-range (Grid-w matrix))])
    (printf (if (grid-ref matrix col row) "#" "."))
    (printf (if (= col (- (Grid-w matrix) 1)) "\n" ""))))

;; List-of Star? -> Vector-of [Vector-of Boolean?]
(define (stars->matrix stars)
  (match-let ([(cons (Pos xmin ymin) (Pos xmax ymax)) (bounding-box stars)])
    (define matrix (make-grid (+ (- xmax xmin) 1) (+ (- ymax ymin) 1)))
    (for ([star stars])
      (grid-set! matrix (- (coord-x star) xmin) (- (coord-y star) ymin) #t))
    matrix))
