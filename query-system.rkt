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

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

;; Convenient functions to avoid manually input assertions to the
;; query system every time

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
             (lambda (frame)
               (instantiate
                   q
                   frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (qeval q (singleton-stream '()) the-empty-history)))))))

;; The driver loop

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (process-single-input (read))
  (query-driver-loop))

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

(define (qeval query frame-stream history)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream history)
        (simple-query query frame-stream history))))

(define (simple-query query-pattern frame-stream history)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame history))))
   frame-stream))

;; Exercise 4.76

;; find out the value of p1 in frame1 and add it to frame2
(define (merge-if-possible p1 p2 frame1 frame2)
  (let ((binding (binding-in-frame p1 frame1)))
    (cond (binding
           (merge-unify-match (binding-value binding) p2 frame1 frame2))
          ((var? p2)
           (let ((binding (binding-in-frame p2 frame2)))
             (if binding
                 (merge-unify-match p1 (binding-value binding) frame1 frame2)
                 (extend p1 p2 frame2))))
          ;; TODO: maybe we need to check dependency
          (else (extend p1 p2 frame2)))))

(define (merge-unify-match p1 p2 frame1 frame2)
  (cond ((eq? frame2 'failed) 'failed)
        ((equal? p1 p2) frame2)
        ((var? p1) (merge-if-possible p1 p2 frame1 frame2))
        ((var? p2) (merge-if-possible p2 p1 frame2 frame2))
        ((and (pair? p1) (pair? p2))
         (merge-unify-match
          (cdr p1)
          (cdr p2)
          frame1
          (merge-unify-match (car p1) (car p2) frame1 frame2)))
        (else 'failed)))

(define (add-if-possible var val frame1 frame2)
  (let ((binding2 (binding-in-frame var frame2)))
    (if binding2
        (let ((val2 (binding-value binding2)))
          (merge-unify-match val val2 frame1 frame2))
        (extend var val frame2))))

(define (merge-frame frame1 frame2)
  (define (iter f1 f2)
    (if (null? f1)
        (singleton-stream f2)
        (let ((binding1 (car f1)))
          (let ((var (binding-variable binding1))
                (val (binding-value binding1)))
            (let ((result (add-if-possible var val frame1 f2)))
              (if (eq? result 'failed)
                  the-empty-stream
                  (iter (cdr f1) result)))))))
  (iter frame1 frame2))

(define (merge-frame-stream s1 s2)
  (stream-flatmap
   (lambda (frame1)
     (stream-flatmap
      (lambda (frame2)
        (merge-frame frame1 frame2))
      s2))
   s1))

(define (conjoin conjuncts frame-stream history)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (let ((s (qeval (first-conjunct conjuncts) (singleton-stream '()) history)))
        (conjoin (rest-conjuncts conjuncts)
                 (merge-frame-stream s frame-stream)
                 history))))

;; (define (conjoin conjuncts frame-stream history)
;;   (if (empty-conjunction? conjuncts)
;;       frame-stream
;;       (conjoin (rest-conjuncts conjuncts)
;;                (qeval (first-conjunct conjuncts) frame-stream history)
;;                history)))

(put conjoin 'and 'qeval)

(define (disjoin disjuncts frame-stream history)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream history)
       (delay (disjoin (rest-disjuncts disjuncts) frame-stream history)))))
(put disjoin 'or 'qeval)

(define (negate operands frame-stream history)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null?
          (qeval (negated-query operands)
                 (singleton-stream frame)
                 history))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put negate 'not 'qeval)

(define (lisp-value call frame-stream history)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
              call
              frame
            (lambda (v f)
              (error "Unknown pat var: LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put lisp-value 'lisp-value 'qeval)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream history) frame-stream)
(put always-true 'always-true 'qeval)

;; Finding assertions by pattern matching

(define (find-assertions pattern frame)
  (stream-flatmap
   (lambda (datum) (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

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

(define (apply-rules pattern frame history)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame history))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame history)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (let ((history-record (make-history-record clean-rule rule unify-result)))
        (if (or (eq? unify-result 'failed)
                (has-loop? history-record history))
            the-empty-stream
            (qeval (rule-body clean-rule)
                   (singleton-stream unify-result)
                   (cons history-record history)))))))

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
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))

;; Environment

(define user-initial-environment (interaction-environment))

;; Loop detector

(define the-empty-history '())

(define (make-history-record clean-rule rule frame)
  (list clean-rule rule frame))
(define (history-record-clean-rule record)
  (car record))
(define (history-record-rule record)
  (cadr record))
(define (history-record-frame record)
  (caddr record))

(define (has-same-bindings? clean-rule1 frame1 clean-rule2 frame2)
  (define (tree-walk exp1 exp2)
    (cond ((var? exp1)
           (let ((binding1 (binding-in-frame exp1 frame1))
                 (binding2 (binding-in-frame exp2 frame2)))
             (cond ((and (and binding1 binding2)
                         (eq? (binding-value binding1)
                              (binding-value binding2))) true)
                   ((and (not binding1) (not binding2)) true)
                   (else false))))
          ((pair? exp1)
           (and (tree-walk (car exp1) (car exp2))
                (tree-walk (cdr exp1) (cdr exp2))))
          (else true)))
  (tree-walk clean-rule1 clean-rule2))

(define (duplicated-record? record1 record2)
  (let ((rule1 (history-record-rule record1))
        (rule2 (history-record-rule record2))
        (clean-rule1 (history-record-clean-rule record1))
        (clean-rule2 (history-record-clean-rule record2))
        (frame1 (history-record-frame record1))
        (frame2 (history-record-frame record2)))
    (and
     (eq? rule1 rule2)
     (has-same-bindings? clean-rule1 frame1 clean-rule2 frame2))))

(define (has-loop? record history)
  (define (iter record history)
    (if (null? history)
        false
        (let ((history-record (car history)))
          (if (duplicated-record? record history-record)
              true
              (iter record (cdr history))))))
  (iter record history))

;; unique

;; Exercise 4.75

(define (stream-unique? s)
  (and (not (stream-null? s))
       (stream-null? (stream-cdr s))))

(define (uniqued-query exps) (car exps))

(define (uniquely-asserted operands frame-stream history)
  (stream-flatmap
   (lambda (frame)
     (let ((result (qeval
                    (uniqued-query operands)
                    (singleton-stream frame)
                    history)))
       (if (stream-unique? result)
           result
           the-empty-stream)))
   frame-stream))
(put uniquely-asserted 'unique 'qeval)
