#lang racket

(provide btree)

(struct btree (data left right)
  #:transparent
  #:methods gen:stream [
    (define (stream-empty? s)
      (void? s))
    (define (stream-first s)
      (btree-left s))
    (define (stream-rest s)
      (stream-cons (btree-data s) (stream (btree-right s))))])
