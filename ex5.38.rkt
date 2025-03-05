#lang sicp

(#%require "./compiler.rkt")
(#%require "./register-machine-simulator.rkt")
(#%require "./register-machine-operations.rkt")

;; (define controller
;;   (statements
;;    (compile '(begin
;;                (define (factorial n)
;;                  (define (iter product counter)
;;                    (if (> counter n)
;;                        product
;;                        (iter (* counter product)
;;                              (+ counter 1))))
;;                  (iter 1 1))
;;                (factorial 5)) 'val 'next)))

;; controller

(define initializer
  '((perform (op initialize-stack))
    (assign env (op get-global-environment))))

;; (define fib-machine
;;   (make-machine all-regs machine-operations
;;                 (append initializer controller)))

;; (start fib-machine)
;; (get-register-contents fib-machine 'val)

(define controller
  (append initializer
          (statements (compile '(* 1 2 3 4 5) 'val 'next))))

controller

(define test-machine (make-machine all-regs machine-operations controller))
(start test-machine)
(get-register-contents test-machine 'val)
