#lang sicp

(#%require "./tagged-list.rkt")

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '()))
    (define (push x) (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (unique-insts-map '())
        (entry-point-regs '())
        (stack-regs '())
        (reg-sources-map '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (add-unique-inst type text)
        (let ((class (assoc type unique-insts-map)))
          (if (not class)
              (set! unique-insts-map (cons (cons type (list text))
                                           unique-insts-map))
              (set-cdr! class (add-unique text (cdr class))))))
      (define (get-unique-insts) unique-insts-map)
      (define (add-entry-point-reg reg-name)
        (set! entry-point-regs (add-unique reg-name entry-point-regs)))
      (define (get-entry-point-regs) entry-point-regs)
      (define (add-stack-reg reg-name)
        (set! stack-regs (add-unique reg-name stack-regs)))
      (define (get-stack-regs) stack-regs)
      (define (add-reg-source reg-name source-exp)
        (let ((reg-list (assoc reg-name reg-sources-map)))
          (if (not reg-list)
              (set! reg-sources-map (cons (cons reg-name (list source-exp))
                                          reg-sources-map))
              (set-cdr! reg-list (add-unique source-exp (cdr reg-list))))))
      (define (get-reg-sources) reg-sources-map)
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'add-unique-inst) add-unique-inst)
              ((eq? message 'get-unique-insts) get-unique-insts)
              ((eq? message 'add-entry-point-reg) add-entry-point-reg)
              ((eq? message 'get-entry-point-regs) get-entry-point-regs)
              ((eq? message 'add-stack-reg) add-stack-reg)
              ((eq? message 'get-stack-regs) get-stack-regs)
              ((eq? message 'add-reg-source) add-reg-source)
              ((eq? message 'get-reg-sources) get-reg-sources)
              (else (error "Unknown request: MACHINE"
                           message))))
      dispatch)))

(define (add-unique-inst machine type text)
  ((machine 'add-unique-inst) type text))

(define (get-unique-insts machine)
  ((machine 'get-unique-insts)))

(define (add-entry-point-reg machine reg-name)
  ((machine 'add-entry-point-reg) reg-name))

(define (get-entry-point-regs machine)
  ((machine 'get-entry-point-regs)))

(define (add-stack-reg machine reg-name)
  ((machine 'add-stack-reg) reg-name))

(define (get-stack-regs machine)
  ((machine 'get-stack-regs)))

(define (add-reg-source machine reg-name source-exp)
  ((machine 'add-reg-source) reg-name source-exp))

(define (get-reg-sources machine)
  ((machine 'get-reg-sources)))

(define (same-inst? i1 i2)
  (if (and (pair? i1) (pair? i2))
      (and (same-inst? (car i1) (car i2))
           (same-inst? (cdr i1) (cdr i2)))
      (eq? i1 i2)))

(define (contains? e l)
  (if (null? l)
      false
      (or (same-inst? (car l) e)
          (contains? e (cdr l)))))

(define (add-unique e l)
  (if (contains? e l)
      l
      (cons e l)))

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
                 value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels
   controller-text
   (lambda (insts labels)
     (update-insts! insts labels machine)
     insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               ;; Exercise 5.8
               (if (assoc next-inst labels)
                   (error "Duplicated label: ASSEMBLE" next-inst)
                   (receive insts
                       (cons (make-label-entry next-inst
                                               insts)
                             labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                   labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag stack ops))
       (let ((inst-text (instruction-text inst)))
         (let ((inst-type (car inst-text)))
           (add-unique-inst machine inst-type inst-text))))
     insts)))

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE"
               label-name))))

(define (make-execution-procedure
         inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else
         (error "Unknown instruction type: ASSEMBLE"
                inst))))

(define (make-assign inst machine labels operations pc)
  (let ((reg-name (assign-reg-name inst)))
    (let ((target (get-register machine reg-name))
          (value-exp (assign-value-exp inst)))
      (let ((value-proc
             (if (operation-exp? value-exp)
                 (make-operation-exp
                  value-exp machine labels operations)
                 (make-primitive-exp
                  (car value-exp) machine labels)))
            (source-exp
             (if (operation-exp? value-exp)
                 value-exp
                 (car value-exp))))
        (add-reg-source machine reg-name source-exp)
        (lambda ()
          ; execution procedure for assign
          (set-contents! target (value-proc))
          (advance-pc pc))))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label
                labels
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label
                         labels
                         (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg-name (register-exp-reg dest)))
             (let ((reg (get-register
                       machine
                       reg-name)))
             (add-entry-point-reg machine reg-name)
             (lambda ()
               (set-contents! pc (get-contents reg))))))
          (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (add-stack-reg machine reg-name)
      (lambda ()
        (push stack (get-contents reg))
        (advance-pc pc)))))
(define (make-restore inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (add-stack-reg machine reg-name)
      (lambda ()
        (set-contents! reg (pop stack))
        (advance-pc pc)))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda () (action-proc) (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label
                       labels
                       (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
                         operations))
        (aprocs
         (map (lambda (e)
                ;; Exercise 5.9
                (if (label-exp? e)
                    (error "Unknown operand type: ASSEMBLE" e)
                    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))

(define fact-machine (make-machine
                      '(continue n val)
                      (list (list '< <)
                            (list '- -)
                            (list '+ +))
                      '((assign continue (label fib-done))
                        fib-loop
                        (test (op <) (reg n) (const 2))
                        (branch (label immediate-answer))
                        ;; set up to compute Fib(n − 1)
                        (save continue)
                        (assign continue (label afterfib-n-1))
                        (save n)
                        ; save old value of n
                        (assign n (op -) (reg n) (const 1)) ; clobber n to n-1
                        (goto (label fib-loop))
                        ; perform recursive call
                        afterfib-n-1
                        ; upon return, val contains Fib(n − 1)
                        (restore n)
                        (restore continue)
                        ;; set up to compute Fib(n − 2)
                        (assign n (op -) (reg n) (const 2))
                        (save continue)
                        (assign continue (label afterfib-n-2))
                        (save val)
                        ; save Fib(n − 1)
                        (goto (label fib-loop))
                        afterfib-n-2
                        ; upon return, val contains Fib(n − 2)
                        (assign n (reg val))
                        ; n now contains Fib(n − 2)
                        (restore val)
                        ; val now contains Fib(n − 1)
                        (restore continue)
                        (assign val ; Fib(n − 1) + Fib(n − 2)
                                (op +) (reg val) (reg n))
                        (goto (reg continue))
                        ; return to caller, answer is in
                        val
                        immediate-answer
                        (assign val (reg n))
                        ; base case: Fib(n) = n
                        (goto (reg continue))
                        fib-done)))
