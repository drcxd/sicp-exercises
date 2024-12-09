#lang sicp

;; Exercise 4.36

;; Simply replacing an-integer-between with an-integer-starting-from
;; results the procedure generating triples only advancing k, leaving
;; i and j untouched.

(#%require "./amb-integers.rkt")
(#%require "./require.rkt")

(define (a-pythagorean-triple)
  (let ((k (an-integer-starting-from 1)))
    (let ((i (an-integer-between 1 k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
