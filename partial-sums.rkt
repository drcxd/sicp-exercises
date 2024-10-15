#lang sicp

(#%require "./stream.rkt")

(#%provide partial-sums)
;; Exercise 3.55

(define (partial-sums s)
  (define ps
    (cons-stream (car s)
                 (stream-add ps (stream-cdr s))))
  ps)
