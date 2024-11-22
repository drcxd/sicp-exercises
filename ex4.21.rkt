#lang sicp

;; Exercise 4.21

;; a

(define (fib n)
  (let ((f (lambda (g n)
             (cond ((= n 0) 1)
                   ((= n 1) 1)
                   (else (+ (g g (- n 1)) (g g (- n 2))))))))
    (f f n)))

;; b

(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
