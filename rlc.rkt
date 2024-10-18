#lang sicp

;; Exercise 3.80

(#%require "./integral.rkt")
(#%require "./stream.rkt")

(define (rlc R L C dt)
  (lambda (vc0 il0)
    (define il (integral-delayed (delay (force dil)) il0 dt))
    (define vc (integral-delayed (delay (force dvc)) vc0 dt))
    (define dil (delay (add-streams (scale-stream il (- (/ R L)))
                                    (scale-stream vc (/ 1 L)))))
    (define dvc (delay (scale-stream il (- (/ 1 C)))))
    (cons il vc)))

(define x ((rlc 1 1 0.2 0.1) 10 0))
