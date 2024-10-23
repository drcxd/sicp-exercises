#lang sicp

;; Exercise 4.5

;; The essence of this is the same with exercise 4.4. I do not like
;; that the book requires the readers to do two exercises which share
;; the same nature, but has not discussed it. That is, the book is
;; testing if you can find out the answer of a problem that it has not
;; taught. However, this defeats the purpose of reading a book and
;; also makes progress harder.

(define (expand-clauses clauses env)
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
                 (list (clause-test first) (clause-recipient first)))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

(define (recipient-clause? clause)
  (eq? '=> (cadr clause)))

(define (clause-test clause)
  (car clause))

(define (clause-recipient clause)
  (caddr clause))
