;; Exercise 4.78

#lang sicp

(#%require "./table.rkt")
(#%require "./tagged-list.rkt")
(#%require "./io.rkt")

(define query-system-table (make-table))
(define (get . keys)
  (table-get query-system-table keys))
(define (put value . keys)
  (table-set query-system-table value keys))

;; The driver loop

(define input-prompt ";;; Amb query input:")
(define output-prompt ";;; Amb query results:")

(define (query-driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (let ((q (query-syntax-process input)))
            (newline)
            (display ";;; Starting a new query\n")
            (cond ((assertion-to-be-added? q)
                   (add-rule-or-assertion! (add-assertion-body q))
                   (newline)
                   (display "Assertion added to data base.\n")
                   (query-driver-loop))
                  (else
                   ;; (display output-prompt)
                   (ambqeval
                    q
                    the-empty-frame
                    (lambda (val next-alternative)
                      (if (failed-frame? val)
                          (next-alternative)
                          (begin
                            (announce-output output-prompt)
                            (display-line
                             (instantiate
                                 q
                                 val
                               (lambda (v f)
                                 (contract-question-mark v))))
                            (internal-loop next-alternative))))
                    (lambda ()
                      (announce-output ";;; There are no more values of")
                      (display-line input)
                      (query-driver-loop)))
                   (query-driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline) (display ";;; There is no current problem")
     (query-driver-loop))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

;; The evaluator

(define (ambqeval query frame succeed fail)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame succeed fail)
        (simple-query query frame succeed fail))))

(define (simple-query query-pattern frame succeed fail)
  (let ((find-assertions-failed
         (lambda ()
           (apply-rules query-pattern
                        frame
                        (lambda (val fail2)
                          (succeed val fail2))
                        fail))))
    (find-assertions query-pattern
                     frame
                     (lambda (val fail3)
                       (succeed val fail3))
                     find-assertions-failed)))

(define (conjoin conjuncts frame succeed fail)
  (if (empty-conjunction? conjuncts)
      frame
      (conjoin (rest-conjuncts conjuncts)
               (ambqeval (first-conjunct conjuncts) frame succeed fail))))
(put conjoin 'and 'qeval)

(define (disjoin disjuncts frame succeed fail)
  (if (empty-disjunction? disjuncts)
      the-failed-frame
      (let ((fail2
             (lambda ()
               (disjoin (rest-disjuncts disjuncts)
                        frame
                        succeed
                        fail))))
        (ambqeval
         (first-disjunct disjuncts)
         frame
         (lambda (val fail3)
           (succeed val fail3))
         fail2))))
(put disjoin 'or 'qeval)

(define (negate operands frame succeed fail)
  (if (failed-frame? (ambqeval (negated-query operands) frame succeed fail))
      frame
      the-failed-frame))
(put negate 'not 'qeval)

(define (lisp-value call frame)
  (if (execute
          (instantiate
              call
              frame
            (lambda (v f)
              (error "Unknown pat var: LISP-VALUE" v))))
      frame
      the-failed-frame))
(put lisp-value 'lisp-value 'qeval)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame) frame)
(put always-true 'always-true 'qeval)

;; Finding assertions by pattern matching

(define (find-assertions pattern frame succeed fail)
  (define (try-next choices)
    (if (null? choices)
        (fail)
        (check-an-assertion (car choices)
                            pattern
                            frame
                            succeed
                            (lambda () (try-next (cdr choices))))))
  (let ((assertions (fetch-assertions pattern frame)))
    (try-next assertions)))

(define (check-an-assertion assertion query-pat query-frame succeed fail)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        (fail)
        (succeed match-result fail))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match
          (cdr pat)
          (cdr dat)
          (pattern-match (car pat) (car dat) frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

;; Rules and unification

(define (apply-rules pattern frame succeed fail)
  (define (try-next choices)
    (if (null? choices)
        (fail)
        (apply-a-rule (car choices)
                      pattern
                      frame
                      succeed
                      (lambda () (try-next (cdr choices))))))
  (let ((rules (fetch-rules pattern frame)))
    (try-next rules)))

(define (apply-a-rule rule query-pattern query-frame succeed fail)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (if (eq? unify-result 'failed)
          (fail)
          (ambqeval (rule-body clean-rule)
                    unify-result
                    succeed
                    fail)))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame)) ; ***
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match (binding-value binding) val frame))
          ((var? val) ; ***
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame) ; ***
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

;; Maintaining the database

(define THE-ASSERTIONS the-empty-stream)
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))
(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))
(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
  (append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons assertion old-assertions))
    'ok))
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put (cons
                assertion
                current-assertion-stream)
               key
               'assertion-stream)))))
(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put (cons rule
                       current-rule-stream)
                 key
                 'rule-stream))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat) (constant-symbol? (car pat)))

;; Stream operations see stream.rkt

;; Query syntax procedures

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))
(define (add-assertion-body exp) (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement)
  (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule)) '(always-true) (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))

;; Frame and bindings

(define the-failed-frame 'failed-frame)
(define (failed-frame? frame) (eq? frame the-failed-frame))
(define the-empty-frame '())
(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))

;; Environment

(define user-initial-environment (interaction-environment))

(query-driver-loop)
