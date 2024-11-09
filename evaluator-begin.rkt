#lang sicp

(#%require "./tagged-list.rkt")
(#%require "./evaluator.rkt")

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))
(define (make-begin seq) (cons 'begin seq))
(define (install-begin!)
  (install-new-exp! 'begin begin? eval-begin))
(#%provide make-begin
           sequence->exp
           install-begin!)
