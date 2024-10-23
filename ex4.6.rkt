#lang sicp

;; Exercise 4.6

(#%require "./evaluator.rkt")

(define (let? exp) (tagged-list 'let exp))

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

;; Add the following clauses to eval
;; ((let? exp) (eval (let->combination
;;                    (let-declarations exp)
;;                    (let-body exp))
;;                   env))

(define (let-decls exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (make-let decls body) (list 'let decls body))
