#lang sicp

;; Exercise 4.4

(#%require "./evaluator.rkt")

(define (and? exp) (tagged-list? exp 'and))
(define (and-exps exp) (cdr exp))
(define (eval-and exps env)
  (cond
    ((null? exps) 'true)
    ((last-exp? exps)
     (let ((r (eval (first-exp exps) env)))
       (if (true? r)
           r
           'false)))
    (else
     (let ((r (eval (first-exp exps) env)))
       (if (true? r)
           (eval-and (cdr exps) env)
           'false)))))

(define (or? exp) (tagged-list? exp 'or))
(define (or-exps exp) (cdr exp))
(define (eval-or exps env)
  (cond ((null? exps) 'false)
        (else
         (let ((r (eval (first-exp exps) env)))
           (if (true? r)
               r
               (eval-or (cdr exps) env))))))

;; To install and and or as new special forms, add the following
;; clauses to the cond expression in eval

;; ((and? exp) (eval-and (and-exps exp) env))
;; ((or? exp) (eval-or (or-exps exp) env))

;; To implement and and or as derived expressions, we must first
;; determine how to define them using other special forms.

;; For or, (or a b c) can be defined using if as follows:

;; (if a
;;     a
;;     (if b
;;         b
;;         (if c
;;             c
;;             'false)))

;; This transformation has the problem that each predicate is
;; evaluated twice. We can not use let during the expansion, because
;; that defeats the idea of implementing or as a derived expression.

(define (or->if exp)
  (expand-or-expressions (or-exps exp)))

(define (expand-or-expressions exps)
  (cond ((null? exps) 'false)
        ((last-exp? exps) (first-exp exps))
        (else
         (make-application
          (make-lambda '(first)
                       (make-if 'first 'first (expand-or-expressions (rest-exps exps))))
          (first-exp exps)))))

;; and is implemented in a similar way

(define (and->if exp)
  (expand-and-expressions (and-exps exp)))

(define (expand-and-expressions exps)
  (cond ((null? exps) 'true)
        ((last-exp? exps)
         ;; TODO: fix make-application usage
         (make-application (make-lambda '(exp)
                                        (make-if 'exp 'exp 'false))
                           (first-exp exps)))
        (else
         (make-if (first-exp exps)
                  (expand-and-expressions (rest-exps exps))
                  'false))))
