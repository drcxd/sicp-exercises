#lang sicp

(define (remove-from-list l x)
  (if (null? l)
      '()
      (if (eq? x (car l))
          (cdr l)
          (cons (car l) (remove-from-list (cdr l) x)))))

(#%provide remove-from-list)
