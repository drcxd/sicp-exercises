#lang sicp

;; Exercise 3.74

(#%require "./stream.rkt")
(#%require "./sign-change-detector.rkt")

(define (zero-crossings sense-data)
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))
