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
(struct binary-tree (data left right)
  #:transparent
  #:property prop:sequence   ; структура умеет работать как последовательность
  make-binary-tree-iterator) ; указываем функцию для создания итератора над деревом
```

Логика для обхода структуры переехала в другую структура, названную итератором. Здесь 
```
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
      ; переменные для удобства
      (define left (binary-tree-iterator-left iter))
      (define root (binary-tree-iterator-root iter))
      (define right (binary-tree-iterator-right iter))

      ; результат функции
      (cond
        ; 1. condition
        [(binary-tree? left)
         ; переменные для удобства
         (define reduced-left-iter ; здесь хранится итератор на левое поддерево после обхода
          (stream-rest (binary-tree-iterator (binary-tree-left left)
                                             (binary-tree-data left)
                                             (binary-tree-right left))))
         ; поля итератора левого поддерева
         (define reduced-left (binary-tree-iterator-left reduced-left-iter))
         (define reduced-root (binary-tree-iterator-root reduced-left-iter))
         (define reduced-right (binary-tree-iterator-right reduced-left-iter))
         
         ; результат для первого выполненного условия
         (binary-tree-iterator (if (and (void? reduced-root) (void? reduced-right))
                                  reduced-left
                                  (binary-tree reduced-root reduced-left reduced-right))
                               root
                               right)]

        ; 2. condition
        [(binary-tree? right)
         (binary-tree-iterator (binary-tree (binary-tree-left right) root (void))
                               (binary-tree-data right)
                               (binary-tree-right right))]

        ; 3. condition
        [else (binary-tree-iterator root right (void))]))])


; функция создания итератора из дерева
(define (make-binary-tree-iterator btree)
  (if (binary-tree? btree)
      (binary-tree-iterator (binary-tree-left btree)
                            (binary-tree-data btree)
                            (binary-tree-right btree))
      (raise-argument-error 'make-binary-tree-iterator "binary-tree?" btree)))
```

### Код добавления нового узла в дерево или поддерева

Функция обертка, которая обеспечивает свойство $e\cdot x = x$ и $x\cdot e = x$.
```
(define (binary-tree-cons a d)
  (match (list (void? a) (void? d))
    [(list #t #f) (bt-transform d)]
    [(list #f #t) (bt-transform a)]
    [(list  _  _)
      (bt-transform
        (bt-cons (bt-transform a) (bt-transform d)))]))
```

Функция `bt-transform` (не экспортируется) нужна только, чтобы исправлять невалидные конфигурации дерева, наподобие `(binary-tree (void) (void) (void)) => #<void>`
```
(define (bt-transform tree)
  (if (binary-tree? tree)
      (match (list (void? (binary-tree-data tree))
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
```

### Дополнительные функции

Вспомогательная функция, которая преобразует дерево в список.
```
(define (binary-tree->list tree)
  (if (binary-tree? tree)
      (for/list ([t tree])
        t)
      (if (void? tree)
        null
        (list tree))))
```

### Требуемые операции


```
(define (binary-tree-map func tree)
  (for/fold ([acc (void)]) ([t tree])
    (binary-tree-cons acc (func t))))

(define (binary-tree-filter func tree)
  (for/fold ([acc (void)]) ([t tree])
    (if (func t)
      (binary-tree-cons acc t)
      acc)))

(define (binary-tree-foldl func init tree)
  (for/fold ([acc init]) ([t tree])
    (func acc t)))

(define (binary-tree-foldr func init tree)
  (for/fold ([acc init]) ([t tree])
    (func t acc)))

;;; по логике данная функция эквивалентна отфильтровыванию
;;; всех элементов равных `v`, поэтому реализована через `filter`
(define (binary-tree-remove v tree [func equal?])
  (binary-tree-filter
    (lambda (node) (not (func v node)))
    tree)))
```

## Тесты, отчет инструмента тестирования, метрики

```
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
(check-equal? (binary-tree->list '()) (list '()) "not correct non-tree input  handling")

(check-equal? (binary-tree->list (binary-tree (void) (void) (void))) '() "not correct non-tree input  handling")

(check-equal? (binary-tree->list (binary-tree 4 (binary-tree 2 1 3) 5))
              '(1 2 3 4 5)
              "not transferred inorder")

;;; # Property-based tests

(define (gen:binary-tree #:max-length [max-len 128]) ; определяем генератор для генерации случайных деревьев на основе списка чисел
  (gen:let
    ([lst
      (gen:list gen:natural #:max-length max-len)])
    (for/fold
      ([tree (void)])
      ([el lst])
      (binary-tree-cons tree el))))

;;; 1. testing operations with the neutral element
(define-property neutral-element-ops
  ([tree (gen:binary-tree)])
  (check-equal? (binary-tree-cons tree (void)) tree)
  (check-equal? (binary-tree-cons (void) tree) tree))

;;; 2. testing associativity
(define (gen:list-or-natural #:max-length [max-len 128])
  (gen:choice
    gen:natural
    (gen:list gen:natural #:max-length max-len)))

(define-property associativity
  ([a (gen:list-or-natural)]
    [b (gen:list-or-natural)]
    [c (gen:list-or-natural)])
  (check-equal?
    (binary-tree-cons (binary-tree-cons a b) c)
    (binary-tree-cons a (binary-tree-cons b c))))

;;; 3. total number of nodes in a perfect tree of height h is 2^(h + 1) - 1
(define (gen:perfect-binary-tree #:max-length [max-len 128])
  (gen:let
    ([lst
      (gen:let ([h (gen:integer-in 1 6)])
        (gen:resize
          (gen:list gen:natural #:max-length max-len)
          (sub1 (expt 2 (add1 h)))))])
    (for/fold
      ([tree (void)])
      ([el lst])
      (binary-tree-cons tree el))))

(define-property total-nodes-in-perfect-tree-the-power-of-two
  ([perfect-tree (gen:perfect-binary-tree)])
  (check-false
    (member
      (void)
      (binary-tree->list perfect-tree))))

(check-property neutral-element-ops)
(check-property associativity)
(check-property total-nodes-in-perfect-tree-the-power-of-two)
```

В отчет инструмента тестирования попало 3 вида property-based тестов по 100 в каждом тестовых случаем, и 13 unit-тестов. Итого 16
```
raco test: (submod "./binary-tree.rkt" test)
  ✓ property neutral-element-ops passed 100 tests.
  ✓ property associativity passed 100 tests.
  ✓ property total-nodes-in-perfect-tree-the-power-of-two passed 100 tests.
16 tests passed
```

## Выводы

Из тех особенностей, что я использовал в этой лабораторной, мне запомнился больше pattern-matching, он невероятно удобен, когда нужно реализовать логику с несколькими проверками, и не приходится городить вложенные if'ы. Плохо, наверное, что по умолчанию у структур скрыто значение полей и нужно каждый раз прописывать `#:transparent`. Также было интересно поработать с фреймворком rackcheck. В плане вывода логических ошибок все очень понятно и shrink позволяет на простых примерах потом все отладить.

## P.S. Инструкция по импорту

`(require (submod (file "binary-tree.rkt") binary-tree))`
