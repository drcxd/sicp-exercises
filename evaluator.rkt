#lang sicp

(#%require "./table.rkt")
(#%require "./filter.rkt")
(#%require "./tagged-list.rkt")

;; -- begin primitive

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

;; -- end primitive

;; -- begin environment

;; Exercise 4.11

;; The frame is implemented as a list of variable, value pairs

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons 'frame (map cons variables values)))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan var-list)
      (cond ((null? var-list)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar var-list)) (cdar var-list))
            (else (scan (cdr var-list)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (cdr frame)))))
  ;; Exercise 4.16 a
  (let ((value (env-loop env)))
    (if (eq? value '*unassigned*)
        (error "Unassigned variable" var)
        value)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan var-list)
      (cond ((null? var-list)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar var-list)) (set-cdr! (car var-list) val))
            (else (scan (cdr var-list)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (cdr frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan var-list)
      (cond ((null? var-list)
             (add-binding-to-frame! var val frame))
            ((eq? var (caar var-list)) (set-cdr! (car var-list) val))
            (else (scan (cdr var-list)))))
    (scan (cdr frame))))

;; Exercise 4.12

;; Alternative implementation

;; (define (lookup-variable var env found not-found frame-end)
;;   (define (env-iter env)
;;     (if (null? env)
;;         (not-found)
;;         (let ((frame (first-frame env)))
;;           (define (frame-iter frame)
;;             (cond ((null? frame) (frame-end))
;;                   ((eq? var (caar frame)) (found frame))
;;                   (else (frame-iter (cdr frame)))))
;;           (frame-iter frame))))
;;   (env-iter env))

;; (define (lookup-variable-value var env)
;;   (lookup-variable
;;    var
;;    env
;;    (lambda (frame) (cdar frame))
;;    (lambda () (error "Unbound variable" var))
;;    (lambda () (lookup-variable-value var (enclosing-environment env)))))

;; (define (set-variable-value! var val env)
;;   (lookup-variable
;;    var
;;    env
;;    (lambda (frame) (set-cdr! (car frame) val))
;;    (lambda () (error "Unbound variable: SET!" var))
;;    (lambda () (set-variable-value! var val (enclosing-environment env)))))

;; (define (define-variable! var val env)
;;   (lookup-variable
;;    var
;;    env
;;    (lambda (vals) (set-car! vals val))
;;    (lambda () 'nothing)
;;    (lambda () (add-binding-to-frame! var val (first-frame env)))))


(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

;; Exercise 4.13

;; Only remove the binding from the first frame, because other code
;; may still use the binding in the enclosing frame.

;; The following procedure is based on the implementation of
;; environment in exercise 4.11

(define (remove-from-env! var env)
  (let ((frame (first-frame env)))
    (set-car! env (filter (lambda (binding) (not (eq? var (car binding)))) frame))))

;; -- end environment

;; -- begin procedure

(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (make-procedure parameters body env)
  ;; Exercise 4.16 c, scan-out-defines is installed in make-procedure
  ;; so that it is not executed multiple times.

  ;; NOTE: after switching to analyzing evaluator, we have to invoke
  ;; scan-out-defines in analyze-lambda, since its current
  ;; implementation only works with code that has not been analyzed

  ;; (list 'procedure parameters (scan-out-defines body) env))

  (list 'procedure parameters body env))

;; -- end procedure

;; -- begin evaluator core

;; (define apply-in-underlying-scheme apply)

(define exp-table (make-table))
(define (install-new-exp! keyword predicate evaluator analyzer)
  (table-set exp-table predicate keyword 'predicate)
  (table-set exp-table evaluator keyword 'eval)
  (table-set exp-table analyzer keyword 'analyze))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((let ((pred (table-get exp-table (car exp) 'predicate)))
           (and pred (pred exp))) ((table-get exp-table (car exp) 'analyze) exp))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (my-eval exp env)
  ((analyze exp) env))

;; Exercise 4.3

;; For self evaluating expressions, variables and applications, we
;; still need to dispatch them explicitly, because these expressions
;; do not carry any data can be used as identifiers.

;; Other expressions, can be dispatched in a data-driven way.

;; (define (my-eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((and (table-get exp-table (car exp) 'predicate)
;;               ((table-get exp-table (car exp) 'predicate) exp))
;;          ((table-get exp-table (car exp) 'eval) exp env))
;;         ((application? exp)
;;          (my-apply (my-eval (operator exp) env)
;;                    (list-of-values (operands exp) env)))
;;         (else
;;          ((error "Unknown expression type: EVAL" exp)))))

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure))))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (true? x) (not (eq? false x)))
(define (false? x) (eq? false x))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (variable? exp) (symbol? exp))
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (my-eval (first-exp exps) env))
        (else
         (my-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE")
        (loop (car procs) (cdr procs)))))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env)) aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))
(define (make-application operator operands)
  (cons operator operands))

;; -- end evaluator core

;; -- begin quote

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp env) (cadr exp))
(define (text-of-quotation-analyze exp) (cadr exp))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation-analyze exp)))
    (lambda (env) qval)))
(define (install-quote-to-evaluator!)
  (install-new-exp! 'quote quoted? text-of-quotation analyze-quoted))

;; -- end quote

;; -- begin definition
(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body

(define (make-definition var value)
  (list 'define var value))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (my-eval (definition-value exp) env)
    env)
  'ok)

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env))))

;; Exercise 4.16 b
(define (scan-out-defines body)
  (define (define->decl define-exp)
    (list (definition-variable define-exp) (definition-value define-exp)))
  (define (body->decls-and-exps body)
    (cons (map define->decl (filter definition? body))
          (filter (lambda (exp)
                    (not (definition? exp))) body)))
  (define (decl->set-exp decl)
    (make-assignment (car decl) (cadr decl)))
  (define (decl->unassigned-decl decl)
    (list (car decl) ''*unassigned*))
  (let ((decls-and-exps (body->decls-and-exps body)))
    (let ((decls (car decls-and-exps))
          (exps (cdr decls-and-exps)))
      (if (null? decls)
          body
          (let ((unassigned-decls (map decl->unassigned-decl decls))
                (set-exps (map decl->set-exp decls)))
            (list (make-let unassigned-decls (append set-exps exps))))))))

(define (install-definition!)
  (install-new-exp! 'define definition? eval-definition analyze-definition))

;; -- end definition

;; -- begin assignment

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (make-assignment var value) (list 'set! var value))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assignment-value exp) env)
                       env)
  'ok)
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))
(define (install-assignment-to-evaluator!)
  (install-new-exp! 'set! assignment? eval-assignment analyze-assignment))

;; -- end assignment

;; -- begin if

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))
(define (install-if!)
  (install-new-exp! 'if if? eval-if analyze-if))

;; -- end if

;; -- begin unless

;; Exercise 4.26

;; I just can't imagine when you have to use unless as a procedure.

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless->if exp)
  (let ((predicate (cadr exp))
        (usual-value (caddr exp))
        (exceptional-value (cadddr exp)))
    (make-if predicate exceptional-value usual-value)))
(define (eval-unless exp env)
  (my-eval (unless->if exp) env))
(define (analyze-unless exp)
  (analyze (unless->if exp)))
(define (install-unless!)
  (install-new-exp! 'unless unless? eval-unless analyze-unless))


;; -- end unless

;; -- begin lambda

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        ;; (bproc (analyze-sequence (lambda-body exp))))

        ;; Invoke scan-out-defines here, since this is the only chance
        ;; that it accesses code that has not been analyzed
        (bproc (analyze-sequence (scan-out-defines (lambda-body exp)))))
    (lambda (env)
      (make-procedure vars bproc env))))

(define (install-lambda!)
  (install-new-exp! 'lambda lambda? eval-lambda analyze-lambda))

;; -- end lambda

;; -- begin begin

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))
(define (analyze-begin exp)
  (let ((proc (analyze-sequence (begin-actions exp))))
    (lambda (env)
      (proc env))))
(define (make-begin seq) (cons 'begin seq))
(define (install-begin!)
  (install-new-exp! 'begin begin? eval-begin analyze-begin))

;; -- end begin

;; -- begin cond

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (if (recipient-clause? first)
                (make-application
                 (make-lambda '(test recipient)
                              (make-application 'recipient (list 'test)))
                 (list (clause-test first) (sequence->exp (clause-recipient first))))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))
(define (eval-cond exp env) (my-eval (cond->if exp) env))
(define (analyze-cond exp)
  (analyze (cond->if exp)))

;; Exercise 4.5

;; The test-recipient syntax has been integrated in expand-clauses

(define (recipient-clause? clause) (eq? '=> (cadr clause)))
(define (clause-test clause) (car clause))
(define (clause-recipient clause) (cddr clause))

(define (install-cond!)
  (install-new-exp! 'cond cond? eval-cond analyze-cond))

;; -- end cond

;; -- begin and or

;; Exercise 4.4

;; Implementing and/or as a special form.

(define (and? exp) (tagged-list? exp 'and))
(define (and-exps exp) (cdr exp))
(define (eval-and exp env)
  (define (iter exps env)
      (cond
        ((null? exps) 'true)
        ((last-exp? exps)
         (let ((r (my-eval (first-exp exps) env)))
           (if (true? r)
               r
               'false)))
        (else
         (let ((r (my-eval (first-exp exps) env)))
           (if (true? r)
               (iter (rest-exps exps) env)
               'false)))))
  (let ((exps (and-exps exp)))
    (iter exps env)))
(define (analyze-and exp)
  (define (iter procs env)
    (cond ((null? procs) 'true)
          ((last-exp? procs)
           (let ((r ((first-exp procs) env)))
             (if (true? r)
                 r
                 'false)))
          (else
           (let ((r ((first-exp procs) env)))
             (if (true? r)
                 (iter (rest-exps procs) env)
                 'false)))))
  (let ((eprocs (map analyze (and-exps exp))))
    (lambda (env)
      (iter eprocs env))))

(define (or? exp) (tagged-list? exp 'or))
(define (or-exps exp) (cdr exp))
(define (eval-or exp env)
  (define (iter exps env)
    (cond ((null? exps) 'false)
          (else
           (let ((r (my-eval (first-exp exps) env)))
             (if (true? r)
                 r
                 (iter (rest-exps exps) env))))))
  (let ((exps (or-exps exp)))
    (iter exps env)))
(define (analyze-or exp)
  (define (iter procs env)
    (cond ((null? procs) 'false)
          (else
           (let ((r ((first-exp procs) env)))
             (if (true? r)
                 r
                 (iter (rest-exps procs) env))))))
  (let ((eprocs (map analyze (or-exps exp))))
    (lambda (env)
      (iter eprocs env))))

(define (install-and-or!)
  (install-new-exp! 'and and? eval-and analyze-and)
  (install-new-exp! 'or or? eval-or analyze-or))

;; -- end and or

;; -- begin let

;; Exercise 4.6 & Exercise 4.8

(define (let? exp) (tagged-list? exp 'let))
(define (let-name exp) (cadr exp))
(define (named-let? exp)
  (and (let? exp)
       (symbol? (cadr exp))))

(define (decls->vars-and-exps decls)
  (cons (map (lambda (decl) (car decl)) decls)
        (map (lambda (decl) (cadr decl)) decls)))


(define (let->combination exp)
  (let ((name (let-name exp))
        (decls (let-decls exp))
        (body (let-body exp)))
    (let ((vars-and-exps (decls->vars-and-exps decls)))
      (let ((vars (car vars-and-exps))
            (exps (cdr vars-and-exps)))
        (if (named-let? exp)
            (make-application (make-lambda '() (list (make-definition
                                                      (cons name vars)
                                                      body)
                                                     (make-application name exps)))
                              '())
            (make-application (make-lambda vars body)
                              exps))))))

(define (eval-let exp env)
  (my-eval (let->combination exp) env))
;; Exercise 4.22
(define (analyze-let exp)
  (analyze (let->combination exp)))

(define (let-decls exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))
(define (first-decl decls) (car decls))
(define (rest-decls decls) (cdr decls))
(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))
(define (make-let decls body) (cons 'let (cons decls body)))
(define (install-let!)
  (install-new-exp! 'let let? eval-let analyze-let))

;; Exercise 4.7

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
  (define (aux decls body)
    (if (null? decls)
        (make-application (make-lambda '() body) '())
        (let ((first (first-decl decls))
              (rest (rest-decls decls)))
          (make-let (list first)
                    (aux rest body)))))
  (aux (let-decls exp) (let-body exp)))
(define (eval-let* exp env)
  (my-eval (let*->nested-lets exp) env))
(define (analyze-let* exp)
  (analyze (let*->nested-lets exp)))
(define (install-let*!)
  (install-let!) ;; because let* depends on let
  (install-new-exp! 'let* let*? eval-let* analyze-let*))

;; Exercise 4.20 a

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-decls exp) (cadr exp))
(define (letrec-body exp) (caddr exp))
(define (letrec->let exp)
  ;; how to implement this? We can extract all the declarations in
  ;; letrec and transform them into definitions. Then, we transform
  ;; the definitions plus the body of letrec using the
  ;; scan-out-defines routine into a new let expression.
  (define (decls->definitions decls)
    (map (lambda (decl)
           (make-definition (car decl) (cadr decl))) decls))
  (let ((decls (letrec-decls exp))
        (body (letrec-body exp)))
    (let ((defines (decls->definitions decls)))
      (car (scan-out-defines (append defines body))))))
(define (eval-letrec exp env)
  (my-eval (letrec->let exp) env))
(define (analyze-letrec exp)
  (analyze (letrec->let exp)))
(define (install-letrec!)
  (install-let!)
  (install-new-exp! 'letrec letrec? eval-letrec analyze-letrec))

;; -- end let

;; -- begin repl

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval output:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (my-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))
(define (announce-output string)
  (newline)
  (display string)
  (newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
;; -- end repl

;; install packages for evaluator

(install-quote-to-evaluator!)
(install-definition!)
(install-assignment-to-evaluator!)
(install-if!)
(install-lambda!)
(install-begin!)
(install-cond!)
(install-and-or!)
(install-let*!) ;; this also installs let
(install-letrec!)
(install-unless!)

;; launch the driver loop
;; (driver-loop)
