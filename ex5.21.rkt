#lang sicp

(#%require "./register-machine-simulator.rkt")

(define count-leaves-machine-a
  (make-machine
   '(continue tree val)
   (list (list '+ +)
         (list 'null? null?)
         (list 'pair? pair?)
         (list 'car car)
         (list 'cdr cdr))
   '((assign continue (label count-leaves-done))
     count-leaves-begin
     (test (op null?) (reg tree))
     (branch (label empty-tree))
     (test (op pair?) (reg tree))
     (branch (label recursive))
     (assign val (const 1))
     (goto (reg continue))
     recursive
     (save continue)
     (save tree)
     (assign continue (label after-count-leaves-1))
     (assign tree (op car) (reg tree))
     (goto (label count-leaves-begin))
     after-count-leaves-1
     (restore tree)
     (assign tree (op cdr) (reg tree))
     (save val)
     (assign continue (label after-count-leaves-2))
     (goto (label count-leaves-begin))
     after-count-leaves-2
     (restore tree) ;; tree now holds (count-leaves (car tree))
     (restore continue)
     (assign val (op +) (reg tree) (reg val))
     (goto (reg continue))
     empty-tree
     (assign val (const 0))
     (goto (reg continue))
     count-leaves-done)))

(define count-leaves-machine-b
  (make-machine
   '(continue tree n)
   (list
    (list '+ +)
    (list 'null? null?)
    (list 'pair? pair?)
    (list 'car car)
    (list 'cdr cdr))
   '((assign continue (label count-leaves-done))
     (assign n (const 0))
     count-leaves-begin
     (test (op null?) (reg tree))
     (branch (label empty-tree))
     (test (op pair?) (reg tree))
     (branch (label recursive))
     (assign n (op +) (reg n) (const 1))
     (goto (reg continue))
     recursive
     (save continue)
     (save tree)
     (assign tree (op car) (reg tree))
     (assign continue (label count-leaves-car))
     (goto (label count-leaves-begin))
     count-leaves-car
     (restore tree)
     (assign tree (op cdr) (reg tree))
     (assign continue (label count-leaves-cdr))
     (goto (label count-leaves-begin))
     count-leaves-cdr
     (restore continue)
     (goto (reg continue))
     empty-tree
     (goto (reg continue))
     count-leaves-done)))
