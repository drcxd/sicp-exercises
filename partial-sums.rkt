#lang sicp

(#%require "./stream.rkt")

(#%provide partial-sums)
;; Exercise 3.55

(define (partial-sums s)
  (define ps
    (cons-stream (car s)
                 (add-streams ps (stream-cdr s))))
  ps)
