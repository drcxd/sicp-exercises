#lang sicp

(#%require "./integral.rkt")
(#%require "./stream.rkt")

;; The definition of solve is different from the book. See
;; https://github.com/sicp-lang/sicp/issues/28 for more details.
(define (solve f y0 dt)
  (define y (integral-delayed (delay (force dy)) y0 dt))
  (define dy (delay (stream-map f y)))
  y)
(#%provide solve)

(stream-ref (solve (lambda (y) y)
                   1
                   0.001)
            1000)
