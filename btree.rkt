;; We must specify the reader and the expander for our code.
#lang racket


;; Next we should decide what is identity element for our binary tree.
;; The most convenient one for us is #<void>.

;; Next step is defining the structure of the binary tree itself:
(struct binary-tree
  ;; as the recursive data structure it should contain the keeping data, left node and right node.
  (data left right)
  #:transparent) ;; it's just to see the values of current instance in the output

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

    [(not (binary-tree? (binary-tree-left tree)))
      (binary-tree
        (binary-tree-data tree)
        (binary-tree
          (binary-tree-left tree) subtree (void))
        (binary-tree-right tree))]

    [(void? (binary-tree-right tree))
      (binary-tree
        (binary-tree-data tree)
        (binary-tree-left tree)
        subtree)]

    [(not (binary-tree? (binary-tree-right tree)))
      (binary-tree
        (binary-tree-data tree)
        (binary-tree-left tree)
        (binary-tree
          (binary-tree-right tree) subtree (void)))]))


(define (binary-tree-cons a d)
  (match (list (void? a) (void? d))
    [(list #t #t) (void)]
    [(list #t #f) d]
    [(list #f #t) a]
    [(list #f #f) (bt-cons a d)]))


(provide (struct-out binary-tree) binary-tree-cons)
