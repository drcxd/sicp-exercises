#lang sicp

;; Exercise 3.75

;; The problem is that the second call to make-zero-crossing is using
;; an average value to compute another average value, rather than two
;; values from the original stream.

(#%require "./stream.rkt")
(#%require "./sign-change-detector.rkt")

(define (make-zero-crossings input-stream last-value last-avg-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2))
        (current-value (stream-car input-stream)))
    (cons-stream
     (sign-change-detector avpt last-avg-value)
     (make-zero-crossings
      (stream-cdr input-stream) current-value avpt))))
