#|review: ignore|#
#lang racket

(module+ binary-tree
  (provide (except-out (all-defined-out) bt-cons bt-transform))

  (struct binary-tree-iterator (left root right)
    #:transparent
    #:methods gen:stream
    [(define (stream-empty? iter)
       (and (void? (binary-tree-iterator-left iter))
            (void? (binary-tree-iterator-root iter))
            (void? (binary-tree-iterator-right iter))))
     (define (stream-first iter)
       (define left (binary-tree-iterator-left iter))
       (if (binary-tree? left)
           (stream-first (binary-tree-iterator (binary-tree-left left)
                                               (binary-tree-data left)
                                               (binary-tree-right left)))
           left))
     (define (stream-rest iter)
       (define left (binary-tree-iterator-left iter))
       (define root (binary-tree-iterator-root iter))
       (define right (binary-tree-iterator-right iter))
       (cond
         [(binary-tree? left)
          (define reduced-left-iter
            (stream-rest (binary-tree-iterator (binary-tree-left left)
                                               (binary-tree-data left)
                                               (binary-tree-right left))))
          (define reduced-left (binary-tree-iterator-left reduced-left-iter))
          (define reduced-root (binary-tree-iterator-root reduced-left-iter))
          (define reduced-right (binary-tree-iterator-right reduced-left-iter))
          (binary-tree-iterator (if (and (void? reduced-root) (void? reduced-right))
                                    reduced-left
                                    (binary-tree reduced-root reduced-left reduced-right))
                                root
                                right)]

         [(binary-tree? right)
          (binary-tree-iterator (binary-tree (binary-tree-left right) root (void))
                                (binary-tree-data right)
                                (binary-tree-right right))]

         [else (binary-tree-iterator root right (void))]))])

  (define (make-binary-tree-iterator btree)
    (if (binary-tree? btree)
        (binary-tree-iterator (binary-tree-left btree)
                              (binary-tree-data btree)
                              (binary-tree-right btree))
        (raise-argument-error 'make-binary-tree-iterator "binary-tree?" btree)))

  (struct binary-tree (data left right)
    #:transparent
    #:property prop:sequence
    make-binary-tree-iterator)

  (define (bt-transform tree)
    (if (binary-tree? tree)
        (match (list (void? (binary-tree-data tree))
                     (void? (binary-tree-left tree))
                     (void? (binary-tree-right tree)))
          [(list #t #t #t) (void)]
          [(list #t #f #t) (binary-tree (binary-tree-left tree) (void) (void))]
          [(list #t #t #f) (binary-tree (binary-tree-right tree) (void) (void))]
          [(list #f #t #t) (binary-tree-data tree)]
          [(list _ _ _) tree])
        tree))

  (define (bt-cons tree [subtree (void)])
    (cond
      [(not (binary-tree? tree))
       (if (and (binary-tree? subtree) (void? (binary-tree-right subtree)))
           (binary-tree tree (binary-tree-data subtree) (binary-tree-left subtree))
           (binary-tree tree subtree (void)))]

      [(void? (binary-tree-left tree))
       (binary-tree (binary-tree-data tree) subtree (binary-tree-right tree))]

      [(void? (binary-tree-right tree))
       (binary-tree (binary-tree-data tree) (binary-tree-left tree) subtree)]

      [else
       (binary-tree (binary-tree-data tree)
                    (binary-tree-cons (binary-tree-left tree) subtree)
                    (binary-tree-right tree))]))

  (define (binary-tree-cons a d)
    (match (list (void? a) (void? d))
      [(list #t #t) (void)]
      [(list #t #f) (bt-transform d)]
      [(list #f #t) (bt-transform a)]
      [(list #f #f) (bt-cons (bt-transform a) (bt-transform d))]))

  (define (binary-tree->list tree)
    (if (binary-tree? tree)
        (for/list ([t tree])
          t)
        (raise-argument-error 'binary-tree->list "binary-tree?" tree)))

  (define (binary-tree-map func tree)
    (for/fold ([acc (void)]) ([t tree])
      (binary-tree-cons acc (func t))))

  (define (binary-tree-filter func tree)
    (for/fold ([acc (void)]) ([t tree])
      (if (func t) (binary-tree-cons acc t) acc)))

  (define (binary-tree-foldl func init tree)
    (for/fold ([acc init]) ([t tree])
      (func acc t)))

  (define (binary-tree-foldr func init tree)
    (for/fold ([acc init]) ([t tree])
      (func t acc)))

  (define (binary-tree-remove v tree [func equal?])
    (binary-tree-filter (lambda (node) (not (func v node))) tree)))

(module+ test
  (require (submod ".." binary-tree)
           rackunit)
  ;;; (define (test-struct-identity data left)
  ;;;   (equal? (binary-tree-cons data left) (if (void? data) left data)))

  ;;; ;;; # Property-based tests

  ;;; ;;; ## binary-tree-cons
  ;;; ;;; 1. testing operations with the neutral element
  ;;; (check-equal? (binary-tree-cons (void) (void)) (void) "property of e * e failed")

  ;;; (check-true (test-struct-identity 1 (void)) "property of x * e = (x) failed where x is number")

  ;;; (check-true (test-struct-identity (void) 1) "property of x * e = (x) failed where x is number")

  ;;; (check-true (test-struct-identity '(1 2 3 4 5 6 7) (void))
  ;;;             "property of x * e = (x) failed where x is a complex data")

  ;;; (check-true (test-struct-identity (void) '(7 6 5 4 3 2 1))
  ;;;             "property of e * x = (x) failed where x is a complex data")

  ;;; (check-true (test-struct-identity (binary-tree 1 2 3) (void))
  ;;;             "property of x * e = x failed where x is a tree")

  ;;; (check-true (test-struct-identity (void) (binary-tree 1 2 3))
  ;;;             "property of e * x = x failed where x is a tree")

  ;;; ;;; 2. testing associativity
  ;;; (check-equal? (binary-tree-cons 1 (binary-tree-cons 2 3))
  ;;;               (binary-tree-cons (binary-tree-cons 1 2) 3)
  ;;;               "property of a * (b * c) = (a * b) * c failed where a, b, c are numbers")

  ;;; (check-equal? (binary-tree-cons '(1 2) (binary-tree-cons '(3 4) '(5 6)))
  ;;;               (binary-tree-cons (binary-tree-cons '(1 2) '(3 4)) '(5 6))
  ;;;               "property of a * (b * c) = (a * b) * c failed where a, b, c are complex data")

  ;;; (check-equal? (binary-tree-cons (binary-tree 1 2 3) (binary-tree-cons 4 5))
  ;;;               (binary-tree-cons (binary-tree-cons (binary-tree 1 2 3) 4) 5)
  ;;;               "property of a * (b * c) = (a * b) * c failed where a is a tree and b, c are numbers")

  ;;; (check-equal?
  ;;;  (binary-tree-cons (binary-tree '(1 2 3) '(3 4) '(5)) (binary-tree-cons '(6 7) '(8 9 10 11)))
  ;;;  (binary-tree-cons (binary-tree-cons (binary-tree '(1 2 3) '(3 4) '(5)) '(6 7)) '(8 9 10 11))
  ;;;  "property of a * (b * c) = (a * b) * c failed where a is a tree and b, c are complex data")

  ;;; ;;; 3. total number of nodes in a perfect tree of height h is 2^(h + 1) - 1
  ;;; (check-equal? (length (binary-tree->list (binary-tree 4 (void) (void)))) 1)
  ;;; (check-equal? (length (binary-tree->list (binary-tree 1 2 3))) 3)
  ;;; (check-equal? (length (binary-tree->list (binary-tree 1 (binary-tree 2 3 4) (binary-tree 5 6 7))))
  ;;;               7)

  ;;; # Unit-tests

  ;;; ## binary-tree-cons
  (check-equal? (binary-tree-cons (binary-tree (void) (void) (void)) (void))
                (void)
                "inalid tree not corrected")
  (check-equal? (binary-tree-cons (binary-tree 1 (void) (void)) 2)
                (binary-tree 1 2 (void))
                "new element not in the left leaf")
  (check-equal? (binary-tree-cons (binary-tree 1 2 (void)) 3)
                (binary-tree 1 2 3)
                "new element not in the right leaf")

  ;;; ## binary-tree-remove
  (check-equal? (binary-tree-remove 3 (binary-tree 1 3 3)) 1 "not all 3 removed")

  (check-false
   (member 3 (binary-tree->list (binary-tree-remove 3 (binary-tree 1 (binary-tree 3 4 5) 3))))
   "not all 3 removed")

  (check-false (member '(1 2 3)
                       (binary-tree->list
                        (binary-tree-remove '(1 2 3) (binary-tree '(1 2 3) (binary-tree 3 4 5) 3))))
               "not all '(1 2 3) removed")

  ;;; ## binary-tree-map
  (check-equal? (binary-tree->list (binary-tree-map (lambda (v) (add1 v)) (binary-tree 1 2 3)))
                '(2 3 4)
                "not all elements increased")

  (check-equal? (binary-tree->list (binary-tree-map (lambda (v) (range v)) (binary-tree 1 2 3)))
                '((0) (0 1) (0 1 2))
                "not all elements increased")

  ;;; ## binary-tree-foldl
  (check-equal? (binary-tree-foldl + 0 (binary-tree 2 1 (binary-tree 4 3 5)))
                15
                "not folded in reverse-sorted list")

  ;;; ## binary-tree-foldr
  (check-equal? (binary-tree-foldr cons '() (binary-tree 2 1 (binary-tree 4 3 5)))
                '(5 4 3 2 1)
                "not folded in reverse-sorted list")

  ;;; ## binary-tree->list
  (check-exn exn:fail:contract? (lambda () (binary-tree->list '())) "type not checked")

  (check-equal? (binary-tree->list (binary-tree (void) (void) (void))) '() "type not checked")

  (check-equal? (binary-tree->list (binary-tree 4 (binary-tree 2 1 3) 5))
                '(1 2 3 4 5)
                "not transferred inorder"))
