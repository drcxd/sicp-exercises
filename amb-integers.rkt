#lang sicp

;; Exercise 4.35

(define (an-integer-between low high)
  (if (> low high)
      (amb)
      (amb low (an-integer-between (+ 1 low) high))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(#%provide an-integer-between
           an-integer-starting-from)
