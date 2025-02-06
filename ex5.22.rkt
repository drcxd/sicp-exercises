#lang sicp

(#%require "./register-machine-simulator.rkt")

;; (define (append x y)
;;   (if (null? x)
;;       y
;;       (cons (car x)
;;             (append (cdr x) y))))

(define append-machine
  (make-machine
   '(x y continue)
   (list
    (list 'null? null?)
    (list 'cdr cdr)
    (list 'car car)
    (list 'cons cons))
   '((assign continue (label append-done))
     append-begin
     (test (op null?) (reg x))
     (branch (label base-case))
     (save continue)
     (save x)
     (assign x (op cdr) (reg x))
     (assign continue (label append-mid))
     (goto (label append-begin))
     append-mid
     (restore x)
     (assign x (op car) (reg x))
     (assign y (op cons) (reg x) (reg y))
     (restore continue)
     base-case
     (goto (reg continue))
     append-done)))

;; (define (append! x y)
;;   (if (null? (cdr x))
;;       (set-cdr! x y)
;;       (append! (cdr x) y)))

(define append!-machine
  (make-machine
   '(continue x y t)
   (list
    (list 'null? null?)
    (list 'set-cdr! set-cdr!)
    (list 'cdr cdr))
   '((assign continue (label append!-done))
     append!-begin
     (assign t (op cdr) (reg x))
     (test (op null?) (reg t))
     (branch (label base-case))
     (assign x (op cdr) (reg x))
     (goto (label append!-begin))
     base-case
     (perform (op set-cdr!) (reg x) (reg y))
     (goto (reg continue))
     append!-done)))
