#lang racket

;; An Interval is a pair of ordered, non-negative integers, which we call left
;; and right although they are also used to represent top and bottom. The
;; interval is closed -- both endpoints are included
(struct Interval (l r) #:transparent)

(define (interval-length inl)
  (+ (- (Interval-r inl) (Interval-l inl)) 1))

;; A Claim is an id; two Intervals representing the top and side of the
;; rectangle; and an integer count
(struct Claim (id xx yy N) #:transparent)

(define (claim-area clm)
  (* (interval-length (Claim-xx clm)) (interval-length (Claim-yy clm))))

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
      (let-values ([(splitA splitB intersect)
                    (disjointify-claims clm (car done-stack))])
        (append splitB intersect (add-stack splitA (cdr done-stack))))))

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
;;  values [List-of Claim?] [List-of Claim?] [List-of Claim?]
;; 
;; Produce a disjoint cover of the union of two Claims. There are three parts to
;; the result: rectangles that used to be in clm1, rectangles that used to be in
;; clm2, and the overlap
(define (disjointify-claims clm1 clm2)
  (match-define (Claim id1 xx1 yy1 N1) clm1)
  (match-define (Claim id2 xx2 yy2 N2) clm2)
  (if (or (> (Interval-l xx1) (Interval-r xx2)) ; Quick check for non-overlap
          (< (Interval-r xx1) (Interval-l xx2))
          (> (Interval-l yy1) (Interval-r yy2))
          (< (Interval-r yy1) (Interval-l yy2))) 
      (values (list clm1) (list clm2) null) ; no overlap
      (let-values ([(xx1a xx1b xx2a xx2b xxi) (disjointify-intervals xx1 xx2)]
                   [(yy1a yy1b yy2a yy2b yyi) (disjointify-intervals yy1 yy2)])
        (values
         (filter not-false? (list (make-claim -1 xx1a yy1a N1)
                                  (make-claim -1 xxi  yy1a N1)
                                  (make-claim -1 xx1b yy1a N1)
                                  (make-claim -1 xx1a yyi  N1)
                                  (make-claim -1 xx1b yyi  N1)
                                  (make-claim -1 xx1a yy1b N1)
                                  (make-claim -1 xxi  yy1b N1)
                                  (make-claim -1 xx1b yy1b N1)))
         (filter not-false? (list (make-claim -1 xx2a yy2a N2)
                                  (make-claim -1 xxi  yy2a N2)
                                  (make-claim -1 xx2b yy2a N2)
                                  (make-claim -1 xx2a yyi  N2)
                                  (make-claim -1 xx2b yyi  N2)
                                  (make-claim -1 xx2a yy2b N2)
                                  (make-claim -1 xxi  yy2b N2)
                                  (make-claim -1 xx2b yy2b N2)))
         (filter not-false? (list (make-claim -1 xxi  yyi  (+ N1 N2))))))))


;; make-claim : number? Interval? Interval? number? -> Claim?
;; Make a claim. However, if either Interval is #f, then return #f
(define (make-claim id xx yy N)
  (if (and xx yy)
      (Claim id xx yy N)
      #f))

(define (not-false? x) x)


;;; Part 1
;;; --------------------------------------------------------------------------------

;; inputs : List-of string?
(define inputs
  (with-input-from-file "input3.txt" port->lines))

;; claims : List-of Claim?
(define claims (map parse-claim inputs))

(define disjoint-claims (add-stack claims '()))

(define multiple-claims (filter (λ (clm) (> (Claim-N clm) 1)) disjoint-claims))

(println (apply + (map claim-area multiple-claims)))

;;; Part 2
;;; --------------------------------------------------------------------------------

(println (filter (λ (clm) (> (Claim-id clm) 0)) disjoint-claims))
