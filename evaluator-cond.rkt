#lang sicp

(#%require "./tagged-list.rkt")
(#%require "./evaluator.rkt")
(#%require "./evaluator-begin.rkt")
(#%require "./evaluator-if.rkt")
(#%require "./evaluator-lambda.rkt")

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (if (recipient-clause? first)
                (make-application
                 (make-lambda '(test recipient)
                              (make-application 'recipient (list 'test)))
                 (list (clause-test first) (sequence->exp (clause-recipient first))))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))
(define (eval-cond exp env) (my-eval (cond->if exp) env))

;; Exercise 4.5

;; The test-recipient syntax has been integrated in expand-clauses

(define (recipient-clause? clause) (eq? '=> (cadr clause)))
(define (clause-test clause) (car clause))
(define (clause-recipient clause) (cddr clause))

(define (install-cond!)
  (install-new-exp! 'cond cond? eval-cond))
(#%provide install-cond!)
