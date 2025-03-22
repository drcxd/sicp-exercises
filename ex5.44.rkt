#lang sicp

;; Test code for exercise 5.44

(#%require "./compiler.rkt")
(#%require "./register-machine-simulator.rkt")
(#%require "./register-machine-operations.rkt")

(define initializer
  '((perform (op initialize-stack))
    (assign env (op get-global-environment))))

;; use lexical addressing and compile-time environment
(define controller1
  (append initializer
          (statements
           (compile
            '((lambda (+ x)
                (+ x)))
            'val
            'next
            the-compile-environment))))

;; controller1

(define controller2
  (append initializer
          (statements
           (compile
            '((lambda (x)
                (+ x 1)))
           'val
           'next
           the-compile-environment))))

controller2

;; (define test-machine (make-machine all-regs machine-operations controller))
;; (start test-machine)
;; (get-register-contents test-machine 'val)
