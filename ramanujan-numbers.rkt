#lang sicp

(#%require "./stream.rkt")
(#%require "./weighted-pairs.rkt")
(#%require "./integers.rkt")

;; Exercise 3.71

(define (pair-cube-sum p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (* i i i) (* j j j))))

(define s (weighted-pairs
           integers
           integers
           (lambda (p1 p2)
             (< (pair-cube-sum p1) (pair-cube-sum p2)))))

(define (f s)
  (let ((first (stream-ref s 0))
        (second (stream-ref s 1)))
    (if (= (pair-cube-sum first) (pair-cube-sum second))
        (cons-stream (pair-cube-sum second)
                     (f (stream-cdr s)))
        (f (stream-cdr s)))))

(define ramanujan-numbers (f s))
(#%provide ramanujan-numbers)

;; ex3.71.rkt> (display-stream-n ramanujan-numbers 5)
;; 1729
;; 4104
;; 13832
;; 20683
;; 32832
;; 39312
;; done
