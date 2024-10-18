#lang sicp

(#%require "./stream.rkt")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)
(#%provide integral)

;; Name the procedure as integral-delayed so that code depends on
;; integral does not break
(define (integral-delayed delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt) int))))
  int)
(#%provide integral-delayed)

;; Exercise 3.77
(define (integral-delayed1 delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral-delayed1 (delay (stream-cdr integrand))
                            (+ (* dt (stream-car integrand))
                               initial-value)
                            dt)))))
(#%provide integral-delayed1)
