#lang sicp

(#%require "./stream.rkt")

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;; Exercise 3.82

(define (random-in-range low high)
  (+ low (random (- high low))))

(define (make-random-pairs-stream x1 x2 y1 y2)
  (cons-stream (cons (random-in-range x1 x2) (random-in-range y1 y2))
               (make-random-pairs-stream x1 x2 y1 y2)))

(define (estimate-integral-stream p x1 x2 y1 y2)
  (define s (stream-map (lambda (pair)
                          (p (car pair) (cdr pair)))
                        (make-random-pairs-stream x1 x2 y1 y2)))
  (scale-stream (monte-carlo s 0 0)
                (* (- x2 x1)
                   (- y2 y1))))

(define (p x y)
  (<= (+ (* x x) (* y y)) 1))

(define pi (estimate-integral-stream p -1.0 1.0 -1.0 1.0))
