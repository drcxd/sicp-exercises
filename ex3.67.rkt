#lang sicp

(#%require "./stream.rkt")
(#%require "./interleave.rkt")

;; Exercise 3.67

;; To generate a stream of all integer pairs, we have to partition the
;; whole table into four parts:

;; (1, 1) | (1, 2) (1, 3) ...
;; -------------------------
;; (2, 1) | (2, 2) (2, 3) ...
;; (3, 1) | (3, 2) (3, 3) ...

(define (pairs-all s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (interleave
     (stream-map (lambda (x) (list x (stream-car t)))
                 (stream-cdr s))
     (pairs-all (stream-cdr s) (stream-cdr t))))))
