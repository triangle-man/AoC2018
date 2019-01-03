#lang racket



;; A path segment is
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
    ['cross CROSS])) 

;; Path segment representations

(define VERTL      #\| )
(define HORIZ      #\- )
(define CROSS      #\+ )
(define SLASH      #\/ )
(define HSALS      #\\ )
(define CART-UP    #\^ )
(define CART-DOWN  #\v )
(define CART-RIGHT #\> )
(define CART-LEFT  #\< )
