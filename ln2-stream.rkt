#lang sicp

(#%require "./stream.rkt")
(#%require "./partial-sums.rkt")

;; Exercise 3.65

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

;; The sequence does not converge fast. After 32 iterations, the value
;; is only bounded between 0.6777662022075269 and 0.7080692325105572,
;; while the value of ln2 is approximately 0.6931471805599453.
