#lang sicp

;; Exercise 4.4

;; Implementing and/or as a special form.

(#%require "./evaluator.rkt")
(#%require "./tagged-list.rkt")

(define (and? exp) (tagged-list? exp 'and))
(define (and-exps exp) (cdr exp))
(define (eval-and exp env)
  (define (iter exps env)
      (cond
        ((null? exps) 'true)
        ((last-exp? exps)
         (let ((r (my-eval (first-exp exps) env)))
           (if (true? r)
               r
               'false)))
        (else
         (let ((r (my-eval (first-exp exps) env)))
           (if (true? r)
               (iter (rest-exps exps) env)
               'false)))))
  (let ((exps (and-exps exp)))
    (iter exps env)))

(define (or? exp) (tagged-list? exp 'or))
(define (or-exps exp) (cdr exp))
(define (eval-or exp env)
  (define (iter exps env)
    (cond ((null? exps) 'false)
          (else
           (let ((r (my-eval (first-exp exps) env)))
             (if (true? r)
                 r
                 (iter (rest-exps exps) env))))))
  (let ((exps (or-exps exp)))
    (iter exps env)))

(define (install-and-or!)
  (install-new-exp! 'and and? eval-and)
  (install-new-exp! 'or or? eval-or))

(#%provide install-and-or!)
