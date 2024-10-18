#lang sicp

(define (sign-change-detector x y)
  (cond ((and (y > 0) (x < 0)) 1)
        ((and (y < 0) (x > 0)) -1)
        (else 0)))
(#%provide sign-change-detector)
