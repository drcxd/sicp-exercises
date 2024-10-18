#lang sicp

(#%require "./stream.rkt")
(#%require "./weighted-pairs.rkt")
(#%require "./integers.rkt")
;; Exercise 3.72

(define (pair-squares-sum p)
  (let ((first (car p))
        (second (cadr p)))
    (+ (* first first) (* second second))))

(define s (weighted-pairs
           integers
           integers
           (lambda (p1 p2)
             (< (pair-squares-sum p1) (pair-squares-sum p2)))))

(define (f s)
  (let ((p1 (stream-ref s 0))
        (p2 (stream-ref s 1))
        (p3 (stream-ref s 2)))
    (if (= (pair-squares-sum p1)
           (pair-squares-sum p2)
           (pair-squares-sum p3))
        (cons-stream
         (list (pair-squares-sum p1) p1 p2 p3)
         (f (stream-cdr s)))
        (f (stream-cdr s)))))

(define three-way-square-sum-numbers (f s))
