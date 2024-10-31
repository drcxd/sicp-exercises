#lang sicp

;; Exercise 4.6 & Exercise 4.8

(#%require "./evaluator.rkt")
(#%require "./tagged-list.rkt")
(#%require "./evaluator-lambda.rkt")

(define (let? exp) (tagged-list? 'let exp))

(define (let->combination decls body)
  (define (decls->vars-and-exps decls)
    (if (null? decls)
        (cons '() '())
        (let ((var (caar decls))
              (exp (cadr decls))
              (rest-vars-and-exps (decls->vars-and-exps (cdr decls))))
          (cons (cons var (car rest-vars-and-exps))
                (cons exp (cdr rest-vars-and-exps))))))
  (let ((vars-and-exps (decls->vars-and-exps decls)))
    (make-application (make-lambda (car vars-and-exps) body)
                      (cdr vars-and-exps))))
(define (eval-let exp env)
  (eval (let->combination (let-decls exp) (let-body exp))) env)

(define (let-decls exp) (cadr exp))
(define (first-decl decls) (car decls))
(define (rest-decls decls) (cdr decls))
(define (let-body exp) (caddr exp))
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
