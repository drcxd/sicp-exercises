#lang sicp

(#%require "./stream.rkt")

(define random-init 0)

(define (rand-update x)
  (modulo (+ 101 (* x 713)) 53))

(define random-numbers
  (cons-stream
   random-init
   (stream-map rand-update random-numbers)))
(#%provide random-numbers)
(#%provide rand-update)
