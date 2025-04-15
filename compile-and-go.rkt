#lang sicp

(#%require "./compiler.rkt")
(#%require "./register-machine-simulator.rkt")
(#%require "./explicit-control-evaluator.rkt")

(define (compile-and-go expression)
  (let ((instructions
         (assemble
          (statements
           (compile expression 'val 'return the-compile-environment))
          eceval)))
    ;; (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))

(#%provide compile-and-go)
