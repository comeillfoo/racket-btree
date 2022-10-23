;; We must specify the reader and the expander for our code.
#lang racket


;; Next we should decide what is identity element for our binary tree.
;; The most convenient one for us is #<void>.

;; Next step is defining the structure of the binary tree itself:
(struct binary-tree
  ;; as the recursive data structure it should contain the keeping data, left node and right node.
  (data left right)
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? s)
    (eq? empty-stream s))

   (define (stream-first s)
    (if (binary-tree? s)
      (stream-first (binary-tree-left s))
      s))

   (define (stream-rest s)
    (if (binary-tree? s)
      (cond
        [(binary-tree? (binary-tree-left s))
          (binary-tree
            (binary-tree-data s)
            (stream-rest (binary-tree-left s))
            (binary-tree-right s))]
        [(binary-tree? (binary-tree-right s))
          (binary-tree
            (binary-tree-data (binary-tree-right s))
            (binary-tree
              (binary-tree-left (binary-tree-right s))
              (binary-tree-data s)
              (void))
            (binary-tree-right (binary-tree-right s)))]
        [else
          (if (and (void? (binary-tree-right s)) (void? (binary-tree-data s))) 
            empty-stream
            (binary-tree
              (binary-tree-right s)
              (binary-tree-data s)
              (void)))])
      s))]) ;; it's just to see the values of current instance in the output


;; Then we should implement the basic operations with out binary tree
(define (bt-cons tree subtree)
  (cond
    [(not (binary-tree? tree))
      (binary-tree tree subtree (void))]

    [(void? (binary-tree-left tree))
      (binary-tree
        (binary-tree-data tree)
        subtree
        (binary-tree-right tree))]

    [(void? (binary-tree-right tree))
      (binary-tree
        (binary-tree-data tree)
        (binary-tree-left tree)
        subtree)]

    [(not (binary-tree? (binary-tree-left tree)))
      (binary-tree
        (binary-tree-data tree)
        (binary-tree
          (binary-tree-left tree) subtree (void))
        (binary-tree-right tree))]

    [(not (binary-tree? (binary-tree-right tree)))
      (binary-tree
        (binary-tree-data tree)
        (binary-tree-left tree)
        (binary-tree
          (binary-tree-right tree) subtree (void)))]
    [else
      (binary-tree
        (binary-tree-data tree)
        (binary-tree-cons (binary-tree-left tree) subtree)
        (binary-tree-right tree))]))


(define (binary-tree-cons a d)
  (match (list (void? a) (void? d))
    [(list #t #t) (void)]
    [(list #t #f) (bt-cons d (void))]
    [(list #f #t) (bt-cons a (void))]
    [(list #f #f) (bt-cons a d)]))


(define (binary-tree->list t)
  (filter
    (lambda (x) (not (stream? x)))
    (stream->list t)))


(require syntax/parse/define)


(define-syntax-parser define-binary-tree-op
  [(define-binary-tree-op op:id)
  #'(lambda (f t)
    (for/fold
      ([tree (void)])
      ([v (op f
        (binary-tree->list t))])
      (binary-tree-cons tree v)))])

(define binary-tree-map (define-binary-tree-op map))
(define binary-tree-filter (define-binary-tree-op filter))


(provide (struct-out binary-tree) binary-tree-cons binary-tree-map binary-tree-filter)
