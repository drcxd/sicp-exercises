#lang sicp

(#%require "./evaluator.rkt")
(#%require "./environment.rkt")
(#%require "./tagged-list.rkt")
(#%require "./evaluator-lambda.rkt")

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body

(define (make-definition var value)
  (list 'define var value))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (install-definition!)
  (install-new-exp! 'define definition? eval-definition))

(#%provide install-definition!
           make-definition)
