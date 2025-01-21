#lang sicp

(#%require "./prompt-for-input.rkt")
(#%require "./table.rkt")
(#%require "./stream.rkt")
(#%require "./tagged-list.rkt")

(define query-system-table (make-table))
(define (get . keys)
  (table-get query-system-table keys))
(define (put value . keys)
  (table-set query-system-table value keys))

;; The driver loop

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:\n")

(define (add-multiple-assert asserts)
  (map (lambda (assert)
         (process-single-input (list 'assert! assert)))
       asserts)
  'done)

(define (process-multiple-input input)
  (map process-single-input input)
  'done)

(define (process-single-input input)
  (let ((q (query-syntax-process input)))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (display "Assertion added to data base.\n"))
          (else
           (newline)
           (display output-prompt)
           (newline)
           (display-stream
            (stream-map
             (lambda (env)
               (instantiate
                   q
                   env
                 (lambda (v f)
                   (contract-question-mark v))))
             (qeval q (singleton-stream the-empty-environment))))))))

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (process-single-input (read))
  (query-driver-loop))

(define (instantiate-with-region exp region unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-region exp region)))
             (if binding
                 ;; NOTE: shallow instantiating, see ex4.79.org
                 ;; (copy (binding-value binding))
                 (binding-value binding)
                 (unbound-var-handler exp region))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

(define (instantiate exp env unbound-var-handler)
  (let ((region (frame-region-ii (env-first-frame env))))
    (instantiate-with-region exp region unbound-var-handler)))

;; The evaluator

(define (qeval query env-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) env-stream)
        (simple-query query env-stream))))

(define (simple-query query-pattern env-stream)
  (stream-flatmap
   (lambda (env)
     (stream-append-delayed
      (find-assertions query-pattern env)
      (delay (apply-rules query-pattern env))))
   env-stream))

(define (conjoin conjuncts env-stream)
  (if (empty-conjunction? conjuncts)
      env-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) env-stream))))
(put conjoin 'and 'qeval)

(define (disjoin disjuncts env-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) env-stream)
       (delay (disjoin (rest-disjuncts disjuncts) env-stream)))))
(put disjoin 'or 'qeval)

(define (negate operands evn-stream)
  (stream-flatmap
   (lambda (env)
     (if (stream-null?
          (qeval (negated-query operands)
                 (singleton-stream env)))
         (singleton-stream env)
         the-empty-stream))
   evn-stream))
(put negate 'not 'qeval)

(define (lisp-value call env-stream)
  (stream-flatmap
   (lambda (env)
     (if (execute
          (instantiate
              call
              env
            (lambda (v f)
              (error "Unknown pat var: LISP-VALUE" v))))
         (singleton-stream env)
         the-empty-stream))
   env-stream))
(put lisp-value 'lisp-value 'qeval)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore env-stream) env-stream)
(put always-true 'always-true 'qeval)

;; Finding assertions by pattern matching

(define (find-assertions pattern env)
  (stream-flatmap
   (lambda (datum) (check-an-assertion datum pattern env))
   (fetch-assertions pattern env)))

(define (check-an-assertion assertion query-pat env)
  (let ((region (frame-region-ii (env-first-frame env))))
    (let ((match-result
           (pattern-match query-pat assertion region)))
      (if (eq? match-result 'failed)
          the-empty-stream
          (singleton-stream (pack-region-ii-in-env match-result env))))))

(define (pattern-match pat dat region)
  (cond ((eq? region 'failed) 'failed)
        ((equal? pat dat) region)
        ((var? pat) (extend-if-consistent pat dat region))
        ((and (pair? pat) (pair? dat))
         (pattern-match
          (cdr pat)
          (cdr dat)
          (pattern-match (car pat) (car dat) region)))
        (else 'failed)))

(define (extend-if-consistent var dat region)
  (let ((binding (binding-in-region var region)))
    (if binding
        (pattern-match (binding-value binding) dat region)
        (extend var dat region))))

;; Rules and unification

(define (apply-rules pattern env)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern env))
                  (fetch-rules pattern env)))

(define (apply-a-rule rule query-pattern env)
  (let ((unify-result (unify-match2 query-pattern
                                    (conclusion rule)
                                    (create-empty-frame))))
    (if (eq? unify-result 'failed)
        the-empty-stream
        (let ((new-env (cons unify-result env)))
          (let ((new-query (instantiate (rule-body rule) new-env (lambda (v f) v))))
            (stream-map
             (lambda (env)
               (let ((first-frame (env-first-frame env))
                     (second-frame (env-second-frame env)))
                 (if second-frame
                     (let ((new-region-ii
                            (transfer-bindings
                             (resolve-regions first-frame)
                             (frame-region-ii second-frame))))
                       (pack-region-ii-in-env new-region-ii (cdr env)))
                     env)))
             (qeval new-query
                    ;; NOTE: if region i of the first frame contains
                    ;; no bindings, we discard this frame.
                    (singleton-stream (if (null? (frame-region-i unify-result))
                                          env
                                          new-env)))))))))

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

(define (unify-match2 p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((var? p1) (extend-region-i frame p1 p2))
        ((var? p2) (extend-region-ii frame p2 p1))
        ((and (pair? p1) (pair? p2))
         (unify-match2 (cdr p1)
                       (cdr p2)
                       (unify-match2 (car p1)
                                     (car p2)
                                     frame)))
        ((equal? p1 p2) frame)
        (else 'failed)))

(define (extend-region-i frame var val)
  (let ((region-i (frame-region-i frame)))
    (let ((result (extend-region-if-possible region-i var val)))
      (if (eq? result 'failed)
          'failed
          (pack-region-i-in-frame (extend var val region-i) frame)))))

(define (extend-region-ii frame var val)
  (let ((region-ii (frame-region-ii frame)))
    (let ((result (extend-region-if-possible region-ii var val)))
      (if (eq? result 'failed)
          'failed
          (pack-region-ii-in-frame (extend var val region-ii) frame)))))

(define (extend-region-if-possible region var val)
  (let ((binding (binding-in-region var region)))
    (if binding
        (let ((binding-value binding))
          (if (eq? binding-value val)
              region
              'failed))
        (extend var val region))))

(define (transfer-bindings region1 region2)
  (if (null? region1)
      region2
      (transfer-bindings (cdr region1) (cons (car region1) region2))))

(define (resolve-regions frame)
  (let ((region-i (frame-region-i frame))
        (region-ii (frame-region-ii frame)))
    (define (iter r1 r2)
      (if (null? r1)
          r2
          (let ((var (caar r1))
                (val (cdar r1)))
            (iter (cdr r1)
                  (cons (cons var (instantiate-with-region
                                   val
                                   region-ii
                                   (lambda (v f) v)))
                        r2)))))
    (iter region-i '())))

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
  (let ((binding (binding-in-region var frame)))
    (cond (binding
           (unify-match (binding-value binding) val frame))
          ((var? val) ; ***
           (let ((binding (binding-in-region val frame)))
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
               (let ((b (binding-in-region e frame)))
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
  (stream-append
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
          (cons-stream assertion old-assertions))
    'ok))
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put (cons-stream
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
            (put (cons-stream rule
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

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-region variable region)
  (assoc variable region))
(define (extend variable value region)
  (cons (make-binding variable value) region))

;; Environment

(define user-initial-environment (interaction-environment))

;; Query Environment

(define (make-frame region-i region-ii) (cons region-i region-ii))

(define the-empty-frame (make-frame '() '()))

(define (create-empty-frame)
  (make-frame '() '()))

(define (frame-region-i frame)
  (car frame))

(define (frame-region-ii frame)
  (cdr frame))

(define (pack-region-i-in-frame region frame)
  (make-frame region (frame-region-ii frame)))

(define (pack-region-ii-in-frame region frame)
  (make-frame (frame-region-i frame) region))

(define the-empty-environment (list the-empty-frame))

(define (env-first-frame env) (car env))

(define (env-second-frame env)
  (if (not (null? (cdr env)))
      (cadr env)
      false))

(define (pack-region-i-in-env region-i env)
  (let ((frame (env-first-frame env)))
    (let ((region-ii (frame-region-ii frame)))
      (cons (make-frame region-i region-ii) (cdr env)))))

(define (pack-region-ii-in-env region-ii env)
  (let ((frame (env-first-frame env)))
    (let ((region-i (frame-region-i frame)))
      (cons (make-frame region-i region-ii) (cdr env)))))

(add-multiple-assert '((rule (last-pair (?x .()) (?x)))
                       (rule (last-pair (?x . ?y) ?z)
                             (last-pair ?y ?z))
                       (rule (append-to-form () ?y ?y))
                       (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                             (append-to-form ?v ?y ?z))
                       (salary John 1000)
                       (salary Mary 500)
                       (address Mary China)))

(query-driver-loop)
