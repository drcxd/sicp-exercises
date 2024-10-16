#lang sicp

(#%require "./stream.rkt")
(#%require "./integers.rkt")

;; Exercise 3.70

;; Note that merge-weighted does not eliminate elements has the same
;; weight but order them arbitrarily, this behavior is different from
;; the original merge.
(define (merge-weighted s1 s2 f)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (if (f s1car s2car)
               (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1) s2 f))
               (cons-stream
                   s2car
                   (merge-weighted s1 (stream-cdr s2) f)))))))

(define (weighted-pairs s1 s2 f)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s1) x))
                (stream-cdr s2))
    (weighted-pairs (stream-cdr s1) (stream-cdr s2) f)
    f)))

(define a
  (weighted-pairs
   integers
   integers
   (lambda (p1 p2)
     (< (+ (car p1) (cadr p1))
        (+ (car p2) (cadr p2))))))

(define b
  (stream-filter
   (lambda (p)
     (define (aux x)
       (and (not (= 0 (remainder x 2)))
            (not (= 0 (remainder x 3)))
            (not (= 0 (remainder x 5)))))
     (and (aux (car p)) (aux (cadr p))))
   (weighted-pairs
    integers
    integers
    (lambda (p1 p2)
      (define (sum p)
        (let ((i (car p))
              (j (cadr p)))
          (+ (* 2 i) (* 3 j) (* 5 i j))))
      (< (sum p1) (sum p2))))))
