#lang sicp

(#%require "./stream.rkt")
(#%require "./integral.rkt")

;; Exercise 3.73

(define (RC R C dt)
  (lambda (istream v0)
    (add-streams (integral (scale-stream istream (/ 1 C)) v0 dt)
                 (scale-stream istream R))))
