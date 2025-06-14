#lang sicp

(#%require "./register-machine-simulator.rkt")
(#%require "./register-machine-operations.rkt")
(#%require "./compiler.rkt")

(define controller
  '(read-compile-print-loop
    (perform (op initialize-stack))
    (perform (op prompt-for-input) (const ";;RCPL input:"))
    (assign exp (op read))
    (assign env (op get-global-environment))
    (assign exp (op compile) (reg exp))
    (assign exp (op statements) (reg exp))
    (assign exp (op assemble-for-self) (reg exp))
    (assign continue (label print-result))
    (goto (reg exp))
    print-result
    (perform (op announce-output) (const ";;RCPL value:"))
    (perform (op user-print) (reg val))
    (goto (label read-compile-print-loop))
    ))

(define extended-machine-operations
  (append machine-operations (list (list 'compile (lambda (exp) (compile exp 'val 'return the-compile-environment)))
                                   (list 'statements statements)
                                   (list 'assemble-for-self (lambda (text)
                                                              (assemble text m))))))

(define m
  (make-machine
   '(exp env val continue proc argl unev)
   extended-machine-operations
   controller))

(start m)
