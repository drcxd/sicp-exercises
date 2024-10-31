#lang sicp

(define (filter p seq)
  (if (null? seq)
      nil
      (if (p (car seq))
          (cons (car seq) (filter p (cdr seq)))
          (filter p (cdr seq)))))
(#%provide filter)
