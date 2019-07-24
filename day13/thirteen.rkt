#lang racket

(require "grid.rkt")

(module+ test
  (define *input*
    (map string->list
         (with-input-from-file "test.txt" port->lines)))
  (define-values (*map* *carts*) (parse-tracks *input*))
  (for/fold ([cs *carts*])
            ([i (in-range 10)])
    (displayln (map->string *map* cs))
    (evolve-step *map* cs)))





;; A segment is
;; - a symbol describing the two sides of the square that it connects:
;;   'ns (|), 'ew (-), 'se (/), 'nw (/), 'sw (\), 'ne (\)
;; - or 'cross (+)  

(define (segment->char seg)
  (match seg
    ['ns    VERTL]
    ['ew    HORIZ]
    ['se    SLASH]
    ['nw    SLASH]
    ['sw    HSALS]
    ['ne    HSALS]
    ['cross CROSS]
    ['empty EMPTY])) 


;; Path segment representations

(define VERTL      #\| )
(define HORIZ      #\- )
(define CROSS      #\+ )
(define SLASH      #\/ )
(define HSALS      #\\ )
(define EMPTY      #\space)
(define CART-UP    #\^ )
(define CART-DOWN  #\v )
(define CART-RIGHT #\> )
(define CART-LEFT  #\< )

;; A Cart is a position, a direction, and a "next turn", where direction is one
;; of 'north, 'east, 'south, or 'west; and next turn is one of 'left, 'straight,
;; or 'right
(struct Cart (x y dir turn) #:transparent)

(define (cart->char c)
  (match (Cart-dir c)
    ['north CART-UP]
    ['east  CART-RIGHT]
    ['south CART-DOWN]
    ['west  CART-LEFT]))

(define (turn-by dir turn)
  (match turn
    ['left (match dir
             ['north 'west]
             ['east  'north]
             ['south 'east]
             ['west  'south])]
    ['straight dir]
    ['right (match dir
              ['north 'east]
              ['east  'south]
              ['south 'west]
              ['west  'north])]))

(define (bump-turn t)
  (match t
    ['left     'straight]
    ['straight 'right]
    ['right    'left]))

;; One cart is "less than" another if it is above it (smaller row) or, if the
;; same row, then to the left.
(define (cart<? c1 c2)
  (or (< (Cart-y c1) (Cart-y c2))
      (and (= (Cart-y c1) (Cart-y c2))
           (< (Cart-x c1) (Cart-x c2)))))

;; Detect collision
(define (cart=? c1 c2)
  (and (= (Cart-x c1) (Cart-x c2))
       (= (Cart-y c1) (Cart-y c2))))

;; parse-tracks : List-of List-of char? -> values Grid? (List-of Cart?) 
;; No bounds checking!
(define (parse-tracks css)
  (let* ([tracks (lists->grid css)]
         [height (Grid-h tracks)]
         [width (Grid-w tracks)]
         [result (make-grid width height #:fill 'empty)]
         [carts (for*/fold ([carts null])
                           ([j (in-range height)]
                            [i (in-range width)])
                  (let-values ([(seg cart) (track-to-seg-and-cart tracks i j)])
                    (grid-set! result i j seg)
                    (if cart (cons cart carts) carts)))])
    (values result carts)))

;; Turn a representation of a segment (possibly ambiguous) into a segment;
;; extract the cart if there is one here.
(define (track-to-seg-and-cart tracks i j)
  (let ([seg (grid-ref tracks i j)])
    (cond 
      [(char=? seg CART-UP)    (values 'ns (Cart i j 'north 'left))]
      [(char=? seg CART-DOWN)  (values 'ns (Cart i j 'south 'left))]
      [(char=? seg CART-LEFT)  (values 'ew (Cart i j 'west 'left))]
      [(char=? seg CART-RIGHT) (values 'ew (Cart i j 'east 'left))]
      [(char=? seg CROSS)      (values 'cross #f)]
      [(char=? seg VERTL)      (values 'ns #f)]
      [(char=? seg HORIZ)      (values 'ew #f)]
      [(char=? seg SLASH)
       (values (resolve-slash-ambiguity SLASH tracks i j) #f)]
      [(char=? seg HSALS)
       (values (resolve-slash-ambiguity HSALS tracks i j) #f)]
      [else                    (values 'empty #f)])))

(define (resolve-slash-ambiguity path tracks i j)
  (if (char=? path SLASH)
      ;; There is a "/" at i j. Is it 'se or 'nw?
      (if (is-east-not-horiz? tracks i j)
          'nw
          'se)
      ;; There is a "\" at i j. Is it 'sw or 'ne?
      (if (is-east-not-horiz? tracks i j)
          'sw
          'ne)))

(define (is-east-not-horiz? tracks i j)
  (or
   (not (< i (Grid-w tracks))) 
   (not (char=? (grid-ref tracks (+ i 1) j) HORIZ))))


;;; Printing
;;; --------

(define (map->string m carts)
  (define char-grid (grid-map segment->char m))
  (for ([c carts])
    (grid-set! char-grid (Cart-x c) (Cart-y c) (cart->char c)))
  (string-join
   (map list->string (grid->lists char-grid))
   "\n"))


;;; Moving
;;; --------------------------------------------------------------------------------

;; Move all carts on timestep and check for collisions
;; map? [List-of Cart?] -> [List-of Cart?] or #f

(define (evolve-step tracks carts)
  (define carts+
    (map (λ (c) (turn-cart tracks c))
         (map (λ (c) (move-cart c)) carts)))
  carts+)

;; Move cart without changing direction
(define (move-cart cart)
  (match-define (Cart x y dir turn) cart)
  (match dir
    ['north (Cart x (- y 1) dir turn)]
    ['east  (Cart (+ x 1) y dir turn)]
    ['south (Cart x (+ y 1) dir turn)]
    ['west  (Cart (- x 1) y dir turn)]))

;; Turn cart without moving based on underlying symbol
;; track is one of 'ns, 'ew, etc
(define (turn-cart tracks cart)
  (match-define (Cart x y dir turn) cart)
  (match (grid-ref tracks x y)
    ['ns (Cart x y dir turn)]
    ['ne (Cart x y (if (eq? dir 'south) 'east 'north) turn)]
    ['se (Cart x y (if (eq? dir 'north) 'east 'south) turn)]
    ['sw (Cart x y (if (eq? dir 'north) 'west 'south) turn)]
    ['ew (Cart x y dir turn)]
    ['nw (Cart x y (if (eq? dir 'south) 'west 'north) turn)]
    ['cross (Cart x y (turn-by dir turn) (bump-turn turn))]))

