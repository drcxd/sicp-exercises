#lang sicp

(#%require "./evaluator.rkt")
(#%require "./tagged-list.rkt")
(#%require "./environment.rkt")

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assignment-value exp) env)
                       env)
  'ok)
(define (install-assignment-to-evaluator!)
  (install-new-exp! 'set! assignment? eval-assignment))
(#%provide install-assignment-to-evaluator!)
