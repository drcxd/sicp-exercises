#lang sicp

;; Exercise 4.13

;; Only remove the binding from the first frame, because other code
;; may still use the binding in the enclosing frame.

;; The following procedure is based on the implementation of
;; environment in exercise 4.11

(define (remove-from-env var env)
  (let ((frame (first-frame env)))
    (set-car! env (filter (lambda (binding) (not (eq? var (car binding)))) frame))))
