#lang sicp

(#%require "./tagged-list.rkt")

(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(#%provide compound-procedure?
           procedure-parameters
           procedure-body
           procedure-environment
           make-procedure)
