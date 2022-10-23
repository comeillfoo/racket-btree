Министерство науки и высшего образования Российской Федерации федеральное государственное автономное образовательное учреждение высшего образования

«Национальный исследовательский университет ИТМО»

---
__ФПИиКТ, Системное и Прикладное Программное Обеспечение__

__Лабораторная работа №2__

по Функциональному программированию

Выполнил: Ханнанов Л. И.

Группа: P34112

Преподаватель: Пенской Александр Владимирович

###### Санкт-Петербург
###### 2022 г.

---

## Требования к разработанному ПО

Реализовать бинарное дерево.

1. Функции:
  * добавление и удаление элементов;
  * фильтрация;
  * отображение (map);
  * свертки (левая и правая);
  * структура должна быть моноидом.
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства монойда).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования.

## Ключевые элементы реализации с минимальными комментариями

Нейтральным элементом выбран результат, который возвращает функция `void` --- `(void) ;;; #<void>`.

### Код структуры

```
(struct binary-tree
  (data left right) ;;; поля: значение, левое и правое поддерево
  #:transparent     ;;; чтобы в при выводе в консоль можно было посмотреть конкретные значения
  ;;; далее в структуре реализованы базовые методы интерфейса stream,
  ;;; чтобы можно было далее легко определить функции foldl, foldr, map, filter
  #:methods gen:stream
  [(define (stream-empty? s)
    (eq? empty-stream s))

    ;;; stream-first и stream-rest обходят деревое в inorder-порядке:
    ;;; левое поддерево, корень, правое поддерево
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
```

### Код добавления нового узла в дерево или поддерева

Функция обертка, которая обеспечивает свойство $e\cdot x = x$ и $x\cdot e = x$.
```
(define (binary-tree-cons a d)
  (match (list (void? a) (void? d))
    [(list #t #t) (void)]
    [(list #t #f) (bt-transform d)]
    [(list #f #t) (bt-transform a)]
    [(list #f #f) (bt-cons
      (bt-transform a)
      (bt-transform d))]))
```
Функция `bt-transform` (не экспортируется) нужна только, чтобы исправлять невалидные конфигурации дерева, наподобие `(binary-tree (void) (void) (void)) => #<void>`
```
(define (bt-transform tree)
  (if (binary-tree? tree)
    ;;; используется pattern-matching
    ;;; символ _ указывает, что не важно, что тут
    (match (list
      (void? (binary-tree-data tree))
      (void? (binary-tree-left tree))
      (void? (binary-tree-right tree)))
      [(list #t #t #t) (void)]
      [(list #t #f #t) (binary-tree (binary-tree-left tree) (void) (void))]
      [(list #t #t #f) (binary-tree (binary-tree-right tree) (void) (void))]
      [(list #f #t #t) (binary-tree-data tree)]
      [(list  _  _  _) tree])
    tree))
```
Логика добавления сокрыта в `bt-cons` (не экспортируется):
```
;;; subtree по умолчанию #<void>
(define (bt-cons tree [subtree (void)])
  (cond
    ;;; так типизация динамическая, то нужно учесть, что на самом деле tree

    ;;; случаи, когда tree не дерево (число, список, вектор и т.д.)
    [(not (binary-tree? tree))
      (if (and (binary-tree? subtree) (void? (binary-tree-right subtree)))
        (binary-tree
          tree
          (binary-tree-data subtree)
          (binary-tree-left subtree))
        (binary-tree tree subtree (void)))]
    
    ;;; случаи, когда в дереве свободен левый узел
    [(void? (binary-tree-left tree))
      (binary-tree
        (binary-tree-data tree)
        subtree
        (binary-tree-right tree))]

    ;;; случаи, когда в дереве свободен правый узел
    [(void? (binary-tree-right tree))
      (binary-tree
        (binary-tree-data tree)
        (binary-tree-left tree)
        subtree)]

    [else
      (binary-tree
        (binary-tree-data tree)
        (binary-tree-cons (binary-tree-left tree) subtree)
        (binary-tree-right tree))]))
```

### Дополнительные функции

Вспомогательная функция, которая преобразует дерево в список через поток.
Нужна, потому что при реализации методов в структуре в конечном списке присутствуют пустые стримы, так как порядок вызовов в `stream->list` функций `stream-empty?, stream-first, stream-rest` не позволяет от них избавиться.
```
(define (binary-tree->list t)
  (if (binary-tree? t)
    (filter
      (λ (x) (not (or (and (stream? x) (stream-empty? x)) (void? x)))) ;;; предикат, по которому остаются только значения из дерева
      (stream->list t))
    (raise-argument-error 'binary-tree->list "binary-tree?" t)))
```

### Требуемые операции

Остальные функции, кроме `binary-tree-remove`, реализуются через `binary-tree->list` и `binary-tree-cons`:

1. дерево преобразуется в список
2. с ним производится требуемая операция из модуля `racket/list` (`foldl, foldr, map, filter`)
3. список собирается в дерево заново
```
;;; (define (func args...) body...) == (define func (lambda (args...) body...))

(define binary-tree-map
  (define-binary-tree-op map func tree))

(define binary-tree-filter
  (define-binary-tree-op filter func tree))

(define binary-tree-foldl
  (define-binary-tree-op foldl func init tree))

(define binary-tree-foldr
  (define-binary-tree-op foldr func init tree))


;;; по логике данная функция эквивалентна отфильтровыванию
;;; всех элементов равных `v`, поэтому реализована через `filter`
(define (binary-tree-remove v tree [func equal?])
  (binary-tree-filter
    (λ (node) (not (func v node)))
    tree)))
```
`define-binary-tree-op` макрос, который разворачивается в соответствующую лямбду с алгоритмом выше.

## Тесты, отчет инструмента тестирования, метрики

```
;;; # Property-based testing

;;; ## binary-tree-cons
;;; 1. testing operations with the neutral element
(check-equal?
  (binary-tree-cons (void) (void))
  (void)
  "property of e * e failed")

(check-true
  (test-struct-identity 1 (void))
  "property of x * e = (x) failed where x is number")

(check-true
  (test-struct-identity (void) 1)
  "property of x * e = (x) failed where x is number")

(check-true
  (test-struct-identity '(1 2 3 4 5 6 7) (void)) 
  "property of x * e = (x) failed where x is a complex data")

(check-true
  (test-struct-identity (void) '(7 6 5 4 3 2 1)) 
  "property of e * x = (x) failed where x is a complex data")

(check-true
  (test-struct-identity (binary-tree 1 2 3) (void)) 
  "property of x * e = x failed where x is a tree")

(check-true
  (test-struct-identity (void) (binary-tree 1 2 3)) 
  "property of e * x = x failed where x is a tree")

;;; 2. testing associativity
(check-equal?
  (binary-tree-cons 1 (binary-tree-cons 2 3))
  (binary-tree-cons (binary-tree-cons 1 2) 3)
  "property of a * (b * c) = (a * b) * c failed where a, b, c are numbers")

(check-equal?
  (binary-tree-cons '(1 2) (binary-tree-cons '(3 4) '(5 6)))
  (binary-tree-cons (binary-tree-cons '(1 2) '(3 4)) '(5 6))
  "property of a * (b * c) = (a * b) * c failed where a, b, c are complex data")

(check-equal?
  (binary-tree-cons
    (binary-tree 1 2 3)
    (binary-tree-cons 4 5))
  (binary-tree-cons
    (binary-tree-cons (binary-tree 1 2 3) 4)
    5)
  "property of a * (b * c) = (a * b) * c failed where a is a tree and b, c are numbers")

(check-equal?
  (binary-tree-cons
    (binary-tree '(1 2 3) '(3 4) '(5))
    (binary-tree-cons '(6 7) '(8 9 10 11)))
  (binary-tree-cons
    (binary-tree-cons (binary-tree '(1 2 3) '(3 4) '(5)) '(6 7))
    '(8 9 10 11))
  "property of a * (b * c) = (a * b) * c failed where a is a tree and b, c are complex data")

;;; 3. total number of nodes in a perfect tree of height h is 2^(h + 1) - 1
(check-equal?
  (length (binary-tree->list (binary-tree 4 (void) (void))))                           1)
(check-equal?
  (length (binary-tree->list (binary-tree 1 2 3)))                                     3)
(check-equal?
  (length (binary-tree->list (binary-tree 1 (binary-tree 2 3 4) (binary-tree 5 6 7)))) 7)


;;; # Unit-tests

;;; ## binary-tree-cons
(check-equal? (binary-tree-cons (binary-tree (void) (void) (void)) (void)) (void)
  "inalid tree not corrected")
(check-equal? (binary-tree-cons (binary-tree 1 (void) (void)) 2) (binary-tree 1 2 (void))
  "new element not in the left leaf")
(check-equal? (binary-tree-cons (binary-tree 1 2 (void)) 3) (binary-tree 1 2 3)
  "new element not in the right leaf")

;;; ## binary-tree-remove
(check-equal? (binary-tree-remove 3 (binary-tree 1 3 3)) 1
  "not all 3 removed")

;;; проверяем, есть ли в списке удаленный элемент
(check-false
  (member
    3
    (binary-tree->list
      (binary-tree-remove
        3
        (binary-tree 1 (binary-tree 3 4 5) 3))))
  "not all 3 removed")
  
(check-false
  (member
    '(1 2 3)
    (binary-tree->list
      (binary-tree-remove
        '(1 2 3)
        (binary-tree '(1 2 3) (binary-tree 3 4 5) 3))))
  "not all '(1 2 3) removed")
  
;;; ## binary-tree-map
(check-equal?
  (binary-tree->list
    (binary-tree-map
      (lambda (v) (add1 v))
      (binary-tree 1 2 3)))
  '(2 3 4)
  "not all elements increased")

(check-equal?
  (binary-tree->list
    (binary-tree-map
      (lambda (v) (range v))
      (binary-tree 1 2 3)))
  '((0) (0 1) (0 1 2))
  "not all elements increased")

;;; ## binary-tree-foldl
(check-equal?
  (binary-tree-foldl
    cons
    '()
    (binary-tree 2 1 (binary-tree 4 3 5)))
  '(5 4 3 2 1)
  "not folded in reverse-sorted list")

;;; ## binary-tree-foldr
(check-equal?
  (binary-tree-foldr
    cons
    '()
    (binary-tree 2 1 (binary-tree 4 3 5)))
  '(1 2 3 4 5)
  "not folded in sorted list")

;;; ## binary-tree->list
(check-exn
  exn:fail:contract?
  (lambda ()
    (binary-tree->list '()))
  "type not checked")

(check-equal?
  (binary-tree->list (binary-tree (void) (void) (void)))
  '()
  "type not checked")
  
(check-equal?
  (binary-tree->list (binary-tree 4 (binary-tree 2 1 3) 5))
  '(1 2 3 4 5)
  "not transferred inorder"))
```

```
raco test: (submod "./binary-tree.rkt" test)
27 tests passed
```

## Выводы

Из тех особенностей, что я использовал в этой лабораторной, мне запомнился больше pattern-matching, он невероятно удобен, когда нужно реализовать логику с несколькими проверками, и не приходится городить вложенные if'ы. Плохо, наверное, что по умолчанию у структур скрыто значение полей и нужно каждый раз прописывать `#:transparent`. Хорошо сэкономило время и силы возможность реализовать базовые методы для потоков (stream) в структуре, что позволило сразу переиспользовать кучу методов из `racket/streams`. И труднее всего было поддержать полиморфизм, так как в рекурсивной структуре сложнее следить за типами узлов, так чтобы в итоге сохранить требуемые свойства.

## P.S. Инструкция по импорту

`(require (submod (file "binary-tree.rkt") binary-tree))`
