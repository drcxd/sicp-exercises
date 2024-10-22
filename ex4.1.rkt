#lang sicp

;; Exercise 4.1

(#%require "./evaluator.rkt")

;; list-of-values, operands are evaluated from left to right
(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
        (cons left
              (list-of-values (rest-operands exps) env)))))

;; list-of-values, operands are evaluated from right to left
(define (list-of-values-r2l exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest))))
