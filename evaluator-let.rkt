#lang sicp

;; Exercise 4.6 & Exercise 4.8

(#%require "./evaluator.rkt")
(#%require "./tagged-list.rkt")
(#%require "./evaluator-lambda.rkt")
(#%require "./evaluator-definition.rkt")

(define (let? exp) (tagged-list? 'let exp))
(define (let-name exp) (cadr exp))
(define (named-let? exp)
  (and (tagged-list? exp 'let)
       (symbol? (cadr exp))))

(define (decls->vars-and-exps decls)
    (if (null? decls)
        (cons '() '())
        (let ((var (caar decls))
              (exp (cadr decls))
              (rest-vars-and-exps (decls->vars-and-exps (cdr decls))))
          (cons (cons var (car rest-vars-and-exps))
                (cons exp (cdr rest-vars-and-exps))))))


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

(define (eval-let exp env)
  (eval (let->combination (let-decls exp) (let-body exp))) env)

(define (let-decls exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))
(define (first-decl decls) (car decls))
(define (rest-decls decls) (cdr decls))
(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))
(define (make-let decls body) (list 'let decls body))
(define (install-let)
  (install-new-exp! 'let let? eval-let))

;; Exercise 4.7

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
  (define (aux decls body)
    (if (null? decls)
        (make-lambda '() body)
        (let ((first (first-decl decls))
              (rest (rest-decls decls)))
          (make-let (list first)
                    (aux rest body)))))
  (aux (let-decls exp) (let-body exp)))
(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))
(define (install-let*)
  (install-let) ;; because let* depends on let
  (install-new-exp! 'let* let*? eval-let*))

(#%provide install-let
           install-let*
           make-let)
