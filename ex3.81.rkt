#lang sicp

;; Exercise 3.81

(#%require "./stream.rkt")
(#%require "./random-numbers.rkt")

(define (random-streams requests init)
  (define s
    (cons-stream
     init
     (stream-map (lambda (r n)
                   (cond ((eq? r 'generate) (rand-update n))
                         ((and (pair? r)
                               (eq? (car r) 'reset)
                               (number? (cdr r)))
                          (cdr r))
                         (else (error "Unknown request RANDOM-STREAMS: " r))))
                 requests
                 s)))
  s)

(define requests (cons-stream
                  'generate
                  (cons-stream
                   'generate
                   (cons-stream
                    '(reset . 5)
                    (cons-stream
                     'generate
                     the-empty-stream)))))
(define x (random-streams requests 0))
