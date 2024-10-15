#lang sicp

(#%provide stream-car
           stream-cdr
           stream-map
           stream-add
           stream-ref
           stream-for-each
           stream-filter
           display-stream-n)

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-add s1 s2)
  (stream-map + s1 s2))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-line s)
  (display s)
  (newline))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-stream-n s n)
  (if (or (< n 0) (stream-null? s))
      'done
      (begin
        (display-line (stream-car s))
        (display-stream-n (stream-cdr s) (- n 1)))))
