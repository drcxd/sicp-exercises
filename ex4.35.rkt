#lang sicp

;; Exercise 4.35
(define (an-integer-between low high)
  (if (> low high)
      (amb)
      (amb low (an-integer-between (+ 1 low) high))))
