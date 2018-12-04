#lang racket

;; An Interval is a pair of ordered, non-negative integers, which we call left
;; and right although they are also used to represent top and bottom. The
;; interval is closed -- both endpoints are included
(struct Interval (l r) #:transparent)

;; A Claim is an id; two Intervals representing the top and side of the
;; rectangle; and an integer count
(struct Claim (id xx yy N) #:transparent)

(define (claim-bump-id clm)
  (struct-copy Claim clm [N (+ 1 (Claim-N clm))]))

;; Convert input format into Claim struct

(define CLAIM-RE
  #px"^#([[:digit:]]+) @ ([[:digit:]]+),([[:digit:]]+): ([[:digit:]]+)x([[:digit:]]+)")

;; parse-claim : string? -> Claim?
(define (parse-claim s)
  (define claim-parts (map string->number (cdr (regexp-match CLAIM-RE s))))
  (match-let ([(list id top-left-x top-left-y width height) claim-parts])
    (Claim id
           (Interval top-left-x (- (+ top-left-x width) 1))
           (Interval top-left-y (- (+ top-left-y height) 1))
           1)))

;; Combine claims

;; add-stack : (List-of Claim?) (List-of Claim?) -> List-of Claim?
(define (add-stack hold-stack done-stack)
  (if (null? hold-stack)
      done-stack
      (add-stack (cdr hold-stack) (add-claim (car hold-stack) done-stack))))

;; add-claim : Claim? (list-of Claim?) -> List-of Claim?
(define (add-claim clm done-stack)
  (if (null? done-stack)
      (list clm)
      (let-values ([(lsplit rsplit inter)
                    (disjointify-claims clm (car done-stack))])
        (append rsplit (add-stack lsplit (cdr done-stack))))))

;; disjointify-intervals : Interval? Interval? -> 5 x Interval?
;; Given two overlapping intervals, A and B return five Intervals: two
;; from A, two from B, and the intersection (each Interval other than the
;; intersection may be #f)
(define (disjointify-intervals A B)
  (match-define (Interval Al Ar) A)
  (match-define (Interval Bl Br) B)
  (cond
    [(< Al Bl)
     (cond
       [(< Ar Br) (values (Interval Al (- Bl 1)) #f
                          #f (Interval (+ Ar 1) Br)
                          (Interval Bl Ar))]
       [(< Br Ar) (values (Interval Al (- Bl 1)) (Interval (+ Br 1) Ar)
                          #f #f
                          (Interval Bl Br))]
       [else      (values (Interval Al (- Bl 1)) #f
                          #f #f
                          (Interval Bl Br))])]
    [(< Bl Al)
     (cond
       [(< Ar Br) (values #f #f
                          (Interval Bl (- Al 1)) (Interval (+ Ar 1) Br)
                          (Interval Al Ar))]
       [(< Br Ar) (values #f (Interval (+ Br 1) Ar)
                          (Interval Bl (- Al 1)) #f
                          (Interval Al Br))]
       [else      (values #f #f
                          (Interval Bl (- Al 1)) #f
                          (Interval Al Ar))])]
    [else
     (cond
       [(< Ar Br) (values #f #f
                          #f (Interval (+ Ar 1) Br)
                          (Interval Al Ar))]
       [(< Br Ar) (values #f (Interval (+ Br 1) Ar)
                          #f #f
                          (Interval Bl Br))]
       [else      (values #f #f
                          #f #f
                          (Interval Al Ar))])]))

;; disjointify-claims : Claim? Claim? ->
;;  or/c (values [List-of Claim?] [List-of Claim?] [List-of Claim?]) #f
;; Produce a disjoint cover of the union of two Claims. There are three parts to
;; the result: rectangles that used to be in A, rectangles that used to be in B,
;; and the overlap
(define (disjointify-claims clm1 clm2)
  (match-define (Claim id1 xx1 yy1 N1) clm1)
  (match-define (Claim id2 xx2 yy2 N2) clm2)
  (if (or (> (Interval-l xx1) (Interval-r xx2))
          (< (Interval-r xx1) (Interval-l xx2))
          (> (Interval-l yy1) (Interval-r yy2))
          (< (Interval-r yy1) (Interval-l yy2))) 
      (values (list clm1) (list clm2) #f) ; no overlap
      (let-values ([(xx1l xx1r xx2l xx2r xxi) (disjointify-intervals xx1 xx2)]
                   [(yy1l yy1r yy2l yy2r yyi) (disjointify-intervals yy1 yy2)])
        #f)))



;;; Part 1
;;; --------------------------------------------------------------------------------

;; inputs : List-of string?
(define inputs
  (with-input-from-file "input3.txt" port->lines))

;; claims : List-of Claim?
(define claims (map parse-claim inputs))

;; The plan is to maintain two structures:
;; 1. A stack of "to be processed" Claims
;; 2. A stack of disjoint Claims with their current counts: the "done" stack

;; We repeatedly pick the top Claim off the to-be-processed stack, and then
;; "add" it to the processed stack.

;; To add a Claim, we first start a new, empty, done stack, and move the current
;; done stack to the running stack. We compare the new Claim to the top of the
;; running stack; if there is no overlap, we move the top of the running stack
;; to the done stack and continue. If we reach the end of running stack, we add
;; the new Claim to the done stack and go back to the beginning.

;; If there is an overlap:
;;
;; 1. Compute the disjoint partition of the two Claims.
;; 2. Add the partition of the running stack Claim (including the overlap) to
;; the done stack.
;; 3. Put the partition of the new Claim back on the to-be-processed stack and
;; start again.




