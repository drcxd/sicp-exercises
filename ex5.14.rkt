#lang sicp

(#%require "./register-machine-simulator.rkt")


(define fact-machine
  (make-machine
   '(val n continue)
   (list
    (list '= =)
    (list '* *)
    (list '- -)
    (list 'read read))
   '(repl-loop
     (assign n (op initialize-stack))
     (assign n (op read))
     (assign continue (label fact-done)) ;set up final return address
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving n and continue.
     ;; Set up continue so that the computation will continue
     ;; at after-fact when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)!
     (goto (reg continue)) ;return to caller
     base-case
     (assign val (const 1)) ;base case: 1! = 1
     (goto (reg continue)) ;return to caller
     fact-done
     (assign n (op print-stack-statistics))
     (goto (label repl-loop)))))

(start fact-machine)
