#lang sicp

;; Exercise 3.25

;; The generalized table implementation is also used as a table
;; library.

(define (make-table)

  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (let ((this-key (car keys))
            (rest-keys (cdr keys)))
        (let ((record (assoc this-key (cdr local-table))))
          (if (null? rest-keys)
              (if record
                  (cdr record)
                  #f)
              (if record
                  (((cdr record) 'lookup-proc) rest-keys)
                  #f)))))

    (define (insert! keys value)
      (let ((this-key (car keys))
            (rest-keys (cdr keys)))
        (let ((record (assoc this-key (cdr local-table))))
          (if (null? rest-keys)
              (if record
                  (set-cdr! record value)
                  (set-cdr! local-table (cons (cons this-key value)
                                              (cdr local-table))))
              (if record
                  (((cdr record) 'insert-proc!) rest-keys value)
                  (let ((new-table (make-table)))
                    (set-cdr! local-table (cons (cons this-key new-table)
                                                (cdr local-table)))
                    ((new-table 'insert-proc!) rest-keys value)))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define (table-get table . keys)
  ((table 'lookup-proc) keys))

(define (table-set table value . keys)
  ((table 'insert-proc!) keys value))

(#%provide make-table
           table-get
           table-set)
