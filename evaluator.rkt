#lang sicp

(#%require "./table.rkt")
(#%require "./environment.rkt")
(#%require "./evaluator-primitive.rkt")
(#%require "./evaluator-procedure.rkt")

;; (define apply-in-underlying-scheme apply)

(define exp-table (make-table))
(define (install-new-exp! keyword predicate procedure)
  (table-set exp-table predicate keyword 'predicate)
  (table-set exp-table procedure keyword 'eval))

;; Exercise 4.3

;; For self evaluating expressions and variables, we still need to
;; dispatch them explicitly, because these expressions do not carry
;; any data can be used as identifiers.

;; Other expressions, except of applications, can be dispatched in a
;; data-driven way.

(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((and (table-get exp-table (car exp) 'predicate)
              ((table-get exp-table (car exp) 'predicate) exp))
         ((table-get exp-table (car exp) 'eval) exp env))
        ((application? exp)
         (my-apply (my-eval (operator exp) env)
                   (list-of-values (operands exp) env)))
        (else
         ((error "Unknown expression type: EVAL" exp)))))

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure))))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (true? x) (not (eq? false x)))
(define (false? x) (eq? false x))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (my-eval (first-exp exps) env))
        (else
         (my-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (make-application operator operands)
  (cons operator operands))

(#%provide my-eval
           my-apply
           true?
           false?
           eval-sequence
           first-exp
           last-exp?
           rest-exps
           make-application
           install-new-exp!)
