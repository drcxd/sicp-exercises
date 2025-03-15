#lang sicp

(#%require "./filter.rkt")

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '/ /)
        (list '> >)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

;; -- begin environment

;; Exercise 4.11

;; The frame is implemented as a list of variable, value pairs

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons 'frame (map cons variables values)))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan var-list)
      (cond ((null? var-list)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar var-list)) (cdar var-list))
            (else (scan (cdr var-list)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (cdr frame)))))
  ;; Exercise 4.16 a
  (let ((value (env-loop env)))
    (if (eq? value '*unassigned*)
        (error "Unassigned variable" var)
        value)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan var-list)
      (cond ((null? var-list)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar var-list)) (set-cdr! (car var-list) val))
            (else (scan (cdr var-list)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (cdr frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan var-list)
      (cond ((null? var-list)
             (add-binding-to-frame! var val frame))
            ((eq? var (caar var-list)) (set-cdr! (car var-list) val))
            (else (scan (cdr var-list)))))
    (scan (cdr frame))))

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
(define (get-global-environment) the-global-environment)

;; Exercise 4.13

;; Only remove the binding from the first frame, because other code
;; may still use the binding in the enclosing frame.

;; The following procedure is based on the implementation of
;; environment in exercise 4.11

(define (remove-from-env! var env)
  (let ((frame (first-frame env)))
    (set-car! env (filter (lambda (binding) (not (eq? var (car binding)))) frame))))

;; Exercise 5.39

(define (lex-addr-frame lex-addr) (car lex-addr))

(define (lex-addr-index lex-addr) (cdr lex-addr))

(define (lexical-address-fetch lex-addr env)
  (let ((frame (list-ref env (lex-addr-frame lex-addr))))
    (list-ref frame (lex-addr-index lex-addr))))

(define (lexical-address-lookup lex-addr env)
  (let ((v-pair (lexical-address-fetch lex-addr env)))
    (if (eq? (cdr v-pair) '*unassigned*)
          (error "Unassgined variable LEXICAL-ADDRESS-LOOKUP" (car v-pair))
          (cdr v-pair))))

(define (lexical-address-set! lex-addr env val)
  (let ((v-pair (lexical-address-fetch lex-addr env)))
    (set-cdr! v-pair val)
    'done))
;; -- end environment

(#%provide
 extend-environment
 lookup-variable-value
 set-variable-value!
 define-variable!
 remove-from-env!
 the-global-environment
 get-global-environment
 lexical-address-lookup
 lexical-address-set!)
