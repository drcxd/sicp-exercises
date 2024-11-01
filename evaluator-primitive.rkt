#lang sicp

(#%require "./tagged-list.rkt")

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitve (cadr proc))) primitive-procedures))

(#%provide primitive-procedure-names
           primitive-procedure-objects
           primitive-procedure?
           primitive-implementation)
