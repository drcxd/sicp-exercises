#lang sicp

(#%require "./tagged-list.rkt")
(#%require "./io.rkt")

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

(define (make-register new-name)
  (let ((name new-name)
        (contents '*unassigned*)
        (tracing false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            ((eq? message 'trace-on)
             (lambda () (set! tracing true)))
            ((eq? message 'trace-off)
             (lambda () (set! tracing false)))
            ((eq? message 'tracing?) tracing)
            ((eq? message 'get-name) name)
            (else
             (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value)
  (if (register 'tracing?)
      (let ((old-value (register 'get))
            (name (register 'get-name)))
        (display-line (list 'assigning name
                       'old-value old-value
                       'new-value value))))
  ((register 'set) value))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display-line (list 'total-pushes '= number-pushes
                          'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (inst-count 0)
        (tracing false)
        (labels '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
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
      (define (execute ignore-break)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (let ((inst (car insts)))
                (if (or (not (instruction-break? inst))
                        ignore-break)
                    (begin
                      (if tracing
                          (begin
                            (map (lambda (label)
                                   (let ((name (car label))
                                         (label-insts (cdr label)))
                                     (if (eq? insts label-insts)
                                         (display-line name))))
                                 labels)
                            (display-line (instruction-text inst))))
                      ((instruction-execution-proc inst))
                      (set! inst-count (+ inst-count 1))
                      (execute false))
                    (display-line
                     (list 'breaking (instruction-break-info inst))))))))
      (define (print-inst-statistics)
        (display-line (list 'inst-executed inst-count)))
      (define (reset-inst-statistics) (set! inst-count 0))
      (define (trace-on) (set! tracing true))
      (define (trace-off) (set! tracing false))
      (define (store-labels new-labels) (set! labels new-labels))
      (define (set-breakpoint label n break)
        (let ((insts (lookup-label labels label)))
          (let ((inst (list-ref insts (- n 1))))
            (instruction-set-break inst (if break (cons label n) '())))))
      (define (cancel-all-breakpoints)
        (map (lambda (inst)
               (instruction-set-break inst '()))
             the-instruction-sequence))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute false))
              ((eq? message 'proceed) (execute true))
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
              ((eq? message 'print-inst-statistics) print-inst-statistics)
              ((eq? message 'reset-inst-statistics) reset-inst-statistics)
              ((eq? message 'trace-on) trace-on)
              ((eq? message 'trace-off) trace-off)
              ((eq? message 'store-labels) store-labels)
              ((eq? message 'register-trace-on)
               (lambda (name)
                 (let ((reg (lookup-register name)))
                   ((reg 'trace-on)))))
              ((eq? message 'register-trace-off)
               (lambda (name)
                 (let ((reg (lookup-register name)))
                   ((reg 'trace-off)))))
              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'cancel-all-breakpoints) cancel-all-breakpoints)
              (else (error "Unknown request: MACHINE"
                           message))))
      dispatch)))

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
     ((machine 'store-labels) labels)
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
         labels machine pc flag stack ops)))
     insts)))

(define (make-instruction text) (list text '() '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cadr inst))
(define (instruction-break-info inst) (caddr inst))
(define (instruction-break? inst)
  (not (null? (instruction-break-info inst))))
(define (instruction-set-break inst new-break)
  (set-car! (cddr inst) new-break))
(define (set-instruction-execution-proc! inst proc)
  (set-car! (cdr inst) proc))

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
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

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
           (let ((reg (get-register
                       machine
                       (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
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
                ;; revert exercise 5.9 since the code generated by
                ;; compiler requires labels as operands

                ;; Exercise 5.9
                ;; (if (label-exp? e)
                ;;     (error "Unknown operand type: ASSEMBLE" e)
                ;;     (make-primitive-exp e machine labels)))
                (make-primitive-exp e machine labels))
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

(define (print-inst-statistics machine)
  ((machine 'print-inst-statistics)))

(define (reset-inst-statistics machine)
  ((machine 'rest-inst-statistics)))

(define (trace-on machine)
  ((machine 'trace-on)))

(define (trace-off machine)
  ((machine 'trace-off)))

(define (register-trace-on machine reg-name)
  ((machine 'register-trace-on) reg-name))

(define (register-trace-off machine reg-name)
  ((machine 'register-trace-off) reg-name))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n true))

(define (proceed-machine machine)
  (machine 'proceed))

(define (cancel-breakpoint machine label n)
  ((machine 'set-breakpoint) label n false))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

(#%provide make-machine
           start
           set-register-contents!
           get-register-contents
           print-inst-statistics
           reset-inst-statistics
           trace-on
           trace-off
           register-trace-on
           register-trace-off
           set-breakpoint
           proceed-machine
           cancel-breakpoint
           cancel-all-breakpoints
           assemble)
