#lang sicp

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

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;; Exercise 3.55
(define (partial-sums s)
  (define ps
    (cons-stream (car s)
                 (stream-add ps (stream-cdr s))))
  ps)


(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

;; Exercise 3.63

;; In Louis' version, each call to sqrt-stream creates a new stream.
;; Iterating through the stream requires computing every element
;; before the program reaches the target element. However, these
;; elements has already been computed in previous calls to
;; sqrt-stream. The original version uses only one stream, thus, only
;; the first access to each element in the stream requires
;; computation. If the delay is not optimized by memo-proc, then there
;; will be no performance difference between these two versions.

;; Exercise 3.64

(define (stream-limit stream tolerance)
  (let ((first (stream-ref stream 0))
        (second (stream-ref stream 1)))
    (if (< (abs (- first second)) tolerance)
        second
        (stream-limit (stream-cdr stream) tolerance))))

(define (ssqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; Exercise 3.65

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

;; The sequence does not converge fast. After 32 iterations, the value
;; is only bounded between 0.6777662022075269 and 0.7080692325105572,
;; while the value of ln2 is approximately 0.6931471805599453.

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; Exercise 3.66

;; 1. Element (1, 1) is the first element, alternatively, it is
;; element 1.

;; 2. If element (i, i) is element x, then element (i, i+1) is
;; element x + 2^(i-1).

;; 2.a Element (i, i) is element 2^i - 1.

;; 3. If element (i, i+1) is element x, then element (i, j), j > 1,
;; is element x + (j-(i+1)) * 2^i.

;; 4. If element (i, i+1) is element x, then element (i+1, i+1) is
;; element x + 2^(i-1).

;; Using the above rules, we can compute the index of any element in
;; the stream.

;; To compute the index of (1, 100) we start with (1, 1), which is
;; element 2^1 - 1 = 1; then (1, 2) is element 1 + 2^(1-1) = 2; and
;; finally (1, 100) is element 2 + (100-(1+1)) * 2^1 = 198.

;; Similarly, to compute (99, 100), we start with (99, 99), which is
;; element 2^99 - 1; (99, 100) is element 2^99 - 1 + 2^(99-1).

;; (100, 100) is element 2^100 - 1.

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

;; Exercise 3.68

;; Louis' pairs works on finite streams but for infinite streams,
;; evaluation would never return. For it recursively calls itself on
;; infinite streams and no cons-stream is used to delay evaluation.

;; Exercise 3.69

;; All triples can be generated by combining 1 with all pairs starting
;; with (1, 1); combining 2 with all pairs starting with (2, 2), ...

(define (triples s1 s2 s3)
  (cons-stream
   (list (stream-car s1) (stream-car s2) (stream-car s3))
   (interleave
    (stream-map (lambda (x) (list (stream-car s1) (stream-car s2) x))
                (stream-cdr s3))
    (interleave (stream-map (lambda (x) (cons (stream-car s1) x))
                            (pairs (stream-cdr s2) (stream-cdr s3)))
                (triples (stream-cdr s1) (stream-cdr s2) (stream-cdr s3))))))
