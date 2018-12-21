#lang racket

;;; Get input file
;;; --------------------------------------------------------------------------------

;; inputs : List-of string?
(define *inputs*
  (map string->number
       (string-split
        (with-input-from-file "input8.txt" port->string))))

;;; Part I: Answer
;;; --------------------------------------------------------------------------------

(module+ main
  (define-values (*tree* rest) (parse-node *inputs*))
  (println (sum-metadata *tree*)))

;;; Part 2: Answer
;;; --------------------------------------------------------------------------------

(module+ main
  (println (node-value *tree*)))


;;; Part I: Definitions
;;; --------------------------------------------------------------------------------

;; A Node is:
;; - A [List-of Node?]; and
;; - A list of number? (the "metadata")

(struct Node (children metadata) #:transparent)

;; parse-node : List-of number? -> values Node? [List-of number?]
;; Parse a node; return that node and the remainder of the list
(define (parse-node ns)
  (let ([n-children (car ns)]
        [n-metadata (cadr ns)]
        [remainder (cddr ns)])
    (let-values ([(children rest) (parse-nodes n-children remainder)])
      (values
       (Node children (take rest n-metadata))
       (drop rest n-metadata)))))

;; parse-nodes : number? [List-of number?] -> values [List-of Node?] [List-of
;; number?]
;; Parse N nodes; return a list of N nodes and the remainder of the list
(define (parse-nodes N ns)
  (if (= N 0)
      (values null ns)
      (let-values ([(node rest) (parse-node ns)])
        (let-values ([(nodes final) (parse-nodes (- N 1) rest)])
          (values (cons node nodes) final)))))

;; Compute the sum of all metadata within a tree
(define (sum-metadata node)
  (+ (apply + (Node-metadata node))
     (if (null? (Node-children node))
         0
         (apply +
                (map sum-metadata (Node-children node))))))

;;; Part II
;;; --------------------------------------------------------------------------------

;; Very inefficient!
(define (node-value node)
  (match-let ([(Node cs ms) node])
    (if (null? cs)
        (apply + ms)
        (let ([c-values (map node-value cs)])
          (indexed-sum c-values ms)))))

(define (indexed-sum vals indxs)
  (let* ([max (length vals)]
         [good-indxs (filter (curry good-index? max) indxs)])
    (apply + (map (λ (i) (list-ref vals (- i 1))) good-indxs))))

(define (good-index? N i)
  (and (>  i 0)
       (<= i N)))
