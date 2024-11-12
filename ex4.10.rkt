#lang sicp

(#%require "./tagged-list.rkt")

;; We can easily change the key word used in expressions. For example:

(define (let? exp) (tagged-list? exp 'tel))

;; We also can change the order of the variable and the
;; value in a set! expression:

(define (assignment-variable exp) (caddr exp))
(define (assignment-value exp) (cadr exp))
