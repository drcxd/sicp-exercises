#lang sicp

;; Test code for exercise 5.46

(#%require "./compile-and-go.rkt")
(#%require "./register-machine-simulator.rkt")
(#%require "./compiler.rkt")
(#%require "./register-machine-operations.rkt")

;; ;; compiled version
;; (compile-and-go
;;  '(define (fib n)
;;     (if (< n 2)
;;         n
;;         (+ (fib (- n 1))
;;            (fib (- n 2))))))

;; machine version
(define controller
  '((assign continue (label fib-done))
    fib-loop
    (test (op <) (reg n) (const 2))
    (branch (label immediate-answer))
    ;; set up to compute Fib(n − 1)
    (save continue)
    (assign continue (label afterfib-n-1))
    (save n) ; save old value of n
    (assign n (op -) (reg n) (const 1)) ; clobber n to n-1
    (goto (label fib-loop)) ; perform recursive call
    afterfib-n-1 ; upon return, val contains Fib(n − 1)
    (restore n)
    (restore continue)
    ;; set up to compute Fib(n − 2)
    (assign n (op -) (reg n) (const 2))
    (save continue)
    (assign continue (label afterfib-n-2))
    (save val) ; save Fib(n − 1)
    (goto (label fib-loop))
    afterfib-n-2 ; upon return, val contains Fib(n − 2)
    (assign n (reg val)) ; n now contains Fib(n − 2)
    (restore val) ; val now contains Fib(n − 1)
    (restore continue)
    (assign val ; Fib(n − 1) + Fib(n − 2)
            (op +) (reg val) (reg n))
    (goto (reg continue)) ; return to caller, answer is in
    val
    immediate-answer
    (assign val (reg n)) ; base case: Fib(n) = n
    (goto (reg continue))
    fib-done
    (perform (op print-stack-statistics))))

(define fib-machine
  (make-machine '(n val continue) machine-operations controller))
;; (set-register-contents! fib-machine 'n 0)
;; (start fib-machine)
;; (get-register-contents fib-machine 'val)
(map (lambda (n)
       (set-register-contents! fib-machine 'n n)
       (start fib-machine)
       (get-register-contents fib-machine 'val))
     '(0 1 2 3 4 5 6 7 8 9))
