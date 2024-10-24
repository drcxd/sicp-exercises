#lang sicp

;; Exercise 4.9

;; Let's design while by first implementing fib using while

;; (define (fib n)
;;   (let ((a 0)
;;         (b 1))
;;     (while (> n 0)
;;       (set! b (+ a b))
;;       (set! a (- b a))
;;       (set! n (- n 1)))
;;     a))

;; How can we transform it using existing special forms?

;; (while <test> <actions>)

;; Basically we have to implement loop using recursive function, this
;; inevitably introduces a new named function, which might conflict
;; with the names used in <actions>.

;; If we can some how make sure the new function name is not used in
;; <actions>, then we can transform the while expression to a
;; named-let expression:

;; (let r () (if <test> (begin <actions> (r))))

(define (while->named-let exp)
  (let ((test (while-test exp))
        (actions (while-actions exp)))
    (make-named-let
     'r
     '()
     (make-if
      test
      (make-begin
       (append
        actions
        (list (make-application 'r '()))))))))


(define (while-test exp) (cadr exp))

(define (while-actions exp) (cddr exp))
