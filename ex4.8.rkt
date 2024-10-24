#lang sicp

;; Exercise 4.8

(#%require "./evaluator.rkt")

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
                                                     (make-application var exps)))
                              '())
            (make-application (make-lambda vars body)
                              exps))))))

;; Add the following clauses to eval

;; ((let? exp) (eval (let->combination exp) env))

(define (let? exp) (tagged-list exp 'let))

(define (let-decls exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (cadr exp)))

(define (let-body exp)
  (if (symbol? (cadr exp))
      (cdddr exp)
      (cddr exp)))

(define (let-name exp)
  (cadr exp))

(define (named-let? exp)
  (and (tagged-list? exp 'let)
       (symbol? (cadr exp))))

(define (make-let decls body) (list 'let decls body))

;; Exercise 4.8

;; A named let can be transformed to a define and an application
;; inside a new environment. For example,

;; (define (fib n)
;;   (let fib-iter ((a 1)
;;                  (b 0)
;;                  (count n))
;;     (if (= count 0)
;;         b
;;         (fib-iter (+ a b) a (- count 1)))))

;; is equivalent to

;; (define (fib n)
;;   ((lambda ()
;;      (define (fib-iter a b count)
;;        (if (= count 0)
;;            b
;;            (fib-iter (+ a b) a (- count 1))))
;;      (fib-iter 1 0 n))))
