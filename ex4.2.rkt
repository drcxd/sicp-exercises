#lang sicp

;; Exercise 4.2

;; a

;; Louis' plan does not work since the criteria of "application" is
;; actually "not other operations", thus it must be the last
;; case. Louis' evaluator will try to interpret (define x 3) as
;; applying a procedure named define with arguments x and 3.

;; b

;; Thanks to the abstract syntax used in the evaluator implementation,
;; we only need to change the following procedures

(#%require "./evaluator.rkt")

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
