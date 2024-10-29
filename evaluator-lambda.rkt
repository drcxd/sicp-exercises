#lang sicp

(#%require "./tagged-list.rkt")
(#%require "./evaluator.rkt")
(#%require "./evaluator-procedure.rkt")

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

(define (install-lambda!)
  (install-new-exp! 'lambda lambda? eval-lambda))

(#%provide make-lambda
           install-lambda!)
