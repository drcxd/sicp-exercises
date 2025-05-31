#lang sicp

(#%require "./compile-and-go.rkt")
(#%require "./compiler.rkt")

(compile-and-go
 '(begin
    (define (f x y)
      (g x y))))


;; (compile '(begin
;;             (define (f x y)
;;               (g x y))
;;             (define (g x y) (+ x y))) 'val 'next the-compile-environment)
