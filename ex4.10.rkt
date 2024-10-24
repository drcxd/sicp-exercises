#lang sicp

;; First we can easily change the key word used in expressions. For
;; example:

(define (let? exp) (tagged-list exp 'tel))

;; Alternatively, we can change the order of the variable and the
;; value in a set! expression:

(define (assignment-variable exp) (caddr exp))
(define (assignment-value exp) (cadr exp))

