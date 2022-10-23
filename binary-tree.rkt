#lang racket


(require syntax/parse/define racket/stream)


(struct binary-tree
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
      s))])


(define (bt-cons tree [subtree (void)])
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
    [(list #t #f) (bt-cons d)]
    [(list #f #t) (bt-cons a)]
    [(list #f #f) (bt-cons a d)]))


(define (binary-tree->list t)
  (filter
    (lambda (x) (not (stream? x)))
    (stream->list t)))


(define-syntax define-binary-tree-op
  (syntax-rules ()
    [(define-binary-tree-op op func tree)
      (lambda (func tree)
        (for/fold
          ([tree (void)])
          ([v (op func
          (binary-tree->list tree))])
        (binary-tree-cons tree v)))]
    [(define-binary-tree-op op func init tree)
      (lambda (func init tree)
        (op func init (binary-tree->list tree)))]))


(define binary-tree-map
  (define-binary-tree-op map func tree))

(define binary-tree-filter
  (define-binary-tree-op filter func tree))

(define binary-tree-foldl
  (define-binary-tree-op foldl func init tree))

(define binary-tree-foldr
  (define-binary-tree-op foldr func init tree))


(provide
  (struct-out binary-tree)
  binary-tree-cons
  binary-tree-map
  binary-tree-filter
  binary-tree-foldl
  binary-tree-foldr)
