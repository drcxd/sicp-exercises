#lang sicp

;; Exercise 4.3

;; For self evaluating expressions and variables, we still need to
;; dispatch them explicitly, because these expressions do not carry
;; any data can be used as identifiers.

;; Other expressions, except of applications, can be dispatched in a
;; data-driven way.

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (car exp))
         ((get 'eval (car exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         ((error "Unknown expression type: EVAL" exp)))))
