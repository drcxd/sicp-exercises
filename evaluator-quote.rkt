#lang sicp

(#%require "./tagged-list.rkt")
(#%require "./evaluator.rkt")

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp env) (cadr exp))
(define (install-quote-to-evaluator!)
  (install-new-exp! 'quote quoted? text-of-quotation))
(#%provide install-quote-to-evaluator!)
