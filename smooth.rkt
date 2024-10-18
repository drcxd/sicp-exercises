#lang sicp

;; Exercise 3.76

(#%require "./stream.rkt")
(#%require "./average.rkt")
(#%require "./sign-change-detector.rkt")

(define (smooth s)
  (stream-map
   average
   s
   (stream-cdr s)))
(#%provide smooth)

(define (zero-crossings s)
  (stream-map
   sign-change-detector
   s
   (cons-stream 0 s)))
