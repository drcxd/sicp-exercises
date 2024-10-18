#lang sicp

;; Exercise 3.78

(#%require "./integral.rkt")
(#%require "./stream.rkt")

;; See comments in solve.rkt for the extra delay and force
(define (solve-2nd a b dt y0 dy0)
  (define y (integral-delayed (delay (force dy)) y0 dt))
  (define dy (delay (integral-delayed (delay (force ddy)) dy0 dt)))
  (define ddy (delay (add-streams (scale-stream y b)
                                  (scale-stream (force dy) a))))
  y)
(#%provide solve-2nd)

;; Exercise 3.79
(define (solve-2nd-general f dt y0 dy0)
  (define y (integral-delayed (delay (force dy)) y0 dt))
  (define dy (delay (integral-delayed (delay (force ddy)) dy0 dt)))
  (define ddy (delay (stream-map f y (force dy))))
  y)
(#%provide solve-2nd-general)
