#lang sicp

(#%require "./evaluator-primitive.rkt")
(#%require "./filter.rkt")

;; Exercise 4.11

;; The frame is implemented as a list of variable, value pairs

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (set! frame (cons (cons var val) frame)))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
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
            ((eq? var (caar frame)) (set-car! (car frame) val))
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
            ((eq? var (caar frame)) (set-car! (car frame) val))
            (else (scan (cdr frame)))))
    (scan frame)))

;; Exercise 4.12

;; Alternative implementation

;; (define (lookup-variable var env found not-found frame-end)
;;   (define (env-iter env)
;;     (if (null? env)
;;         (not-found)
;;         (let ((frame (first-frame env)))
;;           (define (frame-iter frame)
;;             (cond ((null? frame) (frame-end))
;;                   ((eq? var (caar frame)) (found frame))
;;                   (else (frame-iter (cdr frame)))))
;;           (frame-iter frame))))
;;   (env-iter env))

;; (define (lookup-variable-value var env)
;;   (lookup-variable
;;    var
;;    env
;;    (lambda (frame) (cdar frame))
;;    (lambda () (error "Unbound variable" var))
;;    (lambda () (lookup-variable-value var (enclosing-environment env)))))

;; (define (set-variable-value! var val env)
;;   (lookup-variable
;;    var
;;    env
;;    (lambda (frame) (set-cdr! (car frame) val))
;;    (lambda () (error "Unbound variable: SET!" var))
;;    (lambda () (set-variable-value! var val (enclosing-environment env)))))

;; (define (define-variable! var val env)
;;   (lookup-variable
;;    var
;;    env
;;    (lambda (vals) (set-car! vals val))
;;    (lambda () 'nothing)
;;    (lambda () (add-binding-to-frame! var val (first-frame env)))))


(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

;; Exercise 4.13

;; Only remove the binding from the first frame, because other code
;; may still use the binding in the enclosing frame.

;; The following procedure is based on the implementation of
;; environment in exercise 4.11

(define (remove-from-env! var env)
  (let ((frame (first-frame env)))
    (set-car! env (filter (lambda (binding) (not (eq? var (car binding)))) frame))))

(#%provide lookup-variable-value
           extend-environment
           set-variable-value!
           define-variable!
           remove-from-env!
           the-global-environment)
