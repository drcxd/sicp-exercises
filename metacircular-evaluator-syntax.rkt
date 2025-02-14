#lang sicp

(#%require "./tagged-list.rkt")

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (empty-arglist) '())
(define (last-operand? ops) (null? (cdr ops)))
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (true? x) (not (eq? false x)))

(#%provide
 self-evaluating?
 variable?

 quoted?
 text-of-quotation

 assignment?
 assignment-variable
 assignment-value

 definition?
 definition-variable
 definition-value

 if?
 if-predicate
 if-consequent
 if-alternative

 lambda?
 lambda-parameters
 lambda-body

 begin?
 begin-actions

 application?
 operator
 operands
 no-operands?
 first-operand
 rest-operands
 last-operand?
 empty-arglist
 adjoin-arg

 primitive-procedure?
 compound-procedure?
 make-procedure
 procedure-parameters
 procedure-body
 procedure-environment

 first-exp
 last-exp?
 rest-exps

 apply-primitive-procedure

 true?)
