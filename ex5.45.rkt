#lang sicp

;; Test code for exercise 5.45

(#%require "./compile-and-go.rkt")

(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))
