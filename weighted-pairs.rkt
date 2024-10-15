#lang sicp

(#%require "./stream.rkt")
(#%require "./integers.rkt")
(#%require "./pairs.rkt")

;; Exercise 3.70

(define (merge-weighted s1 s2 f)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((eq? (f s1car s2car) 'less)
                  (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1) s2)))
                 ((eq? (f s1car s2car) 'more)
                  (cons-stream
                   s2car
                   (merge-weighted s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1) (stream-cdr s2)))))))))

(define (weighted-pairs s1 s2 f)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s1) x))
                (stream-cdr s2))
    (pairs (stream-cdr s1) (stream-cdr s2))
    f)))

(define a
  (weighted-pairs
   integers
   integers (lambda (p1 p2)
              (let ((sum1 (+ (car p1) (cadr p1)))
                    (sum2 (+ (car p2) (cadr p2))))
                (cond ((< sum1 sum2) 'less)
                      ((> sum1 sum2) 'more)
                      (else 'equal))))))
