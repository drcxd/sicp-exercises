#lang sicp

;; Exercise 4.12

;; Based on the environment implementation of exercise 4.11, i.e.,
;; each frame consists of a list of pairs of variable and value.

(define (lookup-variable var env found not-found frame-end)
  (define (env-iter env)
    (if (null? env)
        (not-found)
        (let ((frame (first-frame env)))
          (define (frame-iter frame)
            (cond ((null? frame) (frame-end))
                  ((eq? var (caar frame)) (found frame))
                  (else (frame-iter (cdr frame)))))
          (frame-iter frame))))
  (env-iter env))

(define (lookup-variable-value var env)
  (lookup-variable
   var
   env
   (lambda (frame) (cdar frame))
   (lambda () (error "Unbound variable" var))
   (lambda () (lookup-variable-value var (enclosing-environment env)))))

(define (set-variable-value! var val env)
  (lookup-variable
   var
   env
   (lambda (frame) (set-cdr! (car frame) val))
   (lambda () (error "Unbound variable: SET!" var))
   (lambda () (set-variable-value! var val (enclosing-environment env)))))

(define (define-variable! var val env)
  (lookup-variable
   var
   env
   (lambda (vals) (set-car! vals val))
   (lambda () 'nothing)
   (lambda () (add-binding-to-frame! var val (first-frame env)))))
