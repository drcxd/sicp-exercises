#lang sicp

;; Exercise 4.11

;; The following procedures need to be modified:

(define (make-frame variables values)
  (if (null? variables)
      '()
      (cons (cons (car variables)
                  (car values))
            (make-frame (cdr variables)
                        (cdr values)))))

;; We no longer need frame-variables and frame-values

(define (add-binding-to-frame! var val frame)
  (cons (cons var val) frame))

;; extend-environment stays the same

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame) (env-loop (enclosing-environment env)))
            ((eq? var (caar frame)) (cdar frame))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame)) (set-cdr! (car frame) val))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((null? frame)
             (add-binding-to-frame! var val frame))
            ((eq? var (caar frame)) (set-cdr! (car frame) val))
            (else (scan (cdr frame)))))
    (scan frame)))
