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
(define (make-definition var value)
  (list 'define var value))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-first-clause exps) (car exps))
(define (cond-rest-clauses exps) (cdr exps))
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
            (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))))))


(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (empty-arglist) '())
(define (last-operand? ops) (null? (cdr ops)))
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (make-application operator operands)
  (cons operator operands))

(define (let? exp) (tagged-list? exp 'let))
(define (named-let? exp)
  (and (let? exp)
       (symbol? (cadr exp))))
(define (let-name exp) (cadr exp))
(define (let-decls exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))
(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))
(define (decls->vars-and-exps decls)
  (cons (map (lambda (decl) (car decl)) decls)
        (map (lambda (decl) (cadr decl)) decls)))

(define (let->combination exp)
  (let ((name (let-name exp))
        (decls (let-decls exp))
        (body (let-body exp)))
    (let ((vars-and-exps (decls->vars-and-exps decls)))
      (let ((vars (car vars-and-exps))
            (exps (cdr vars-and-exps)))
        (if (named-let? exp)
            (make-application (make-lambda '() (list (make-definition
                                                      (cons name vars)
                                                      body)
                                                     (make-application name exps)))
                              '())
            (make-application (make-lambda vars body)
                              exps))))))


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

(define (or? exp) (tagged-list? exp 'or))
(define (or-exps exp) (cdr exp))
(define (or->application exp)
  (expand-or-expressions (or-exps exp)))

(define (expand-or-expressions exps)
  (cond ((null? exps) 'false)
        ((last-exp? exps) (first-exp exps))
        (else
         (make-application
          (make-lambda '(first)
                       (list (make-if 'first 'first (expand-or-expressions (rest-exps exps)))))
          (list (first-exp exps))))))

(define (and? exp) (tagged-list? exp 'and))
(define (and-exps exp) (cdr exp))
(define (and->application exp)
  (expand-and-expressions (and-exps exp)))

(define (expand-and-expressions exps)
  (cond ((null? exps) 'true)
        ((last-exp? exps) (first-exp exps))
        (else
         (make-application
          (make-lambda '(first)
                       (list (make-if 'first (expand-and-expressions (rest-exps exps)) 'first)))
          (list (first-exp exps))))))

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

 cond?
 cond->if
 cond-clauses
 cond-first-clause
 cond-else-clause?
 cond-rest-clauses
 cond-predicate
 cond-actions

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

 let?
 let->combination

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

 true?

 or?
 or->application
 and?
 and->application)
