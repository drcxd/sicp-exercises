#lang sicp

;; Test code for exercise 5.42

(#%require "./compiler.rkt")
(#%require "./register-machine-simulator.rkt")
(#%require "./register-machine-operations.rkt")

(define initializer
  '((perform (op initialize-stack))
    (assign env (op get-global-environment))))

(define controller
  (append initializer
          (statements
           (compile
            '((lambda (x y)
                (set! x 10)
                x) 1 2)
            'val
            'next
            the-compile-environment))))

controller

(define test-machine (make-machine all-regs machine-operations controller))
(start test-machine)
(get-register-contents test-machine 'val)
