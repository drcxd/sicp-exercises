#lang sicp

;; Exercise 4.7

;; According to the definition of let*, it simply is a series of
;; nested lets.

(define (let*->nested-lets exp)
  (define (aux decls body)
    (if (null? decls)
        (make-lambda '() body)
        (let ((first (first-decl decls))
              (rest (rest-decls delcs)))
          (make-let (list first)
                    (aux rest body)))))
  (aux (let-decls exp) (let-body exp)))

;; It is sufficient to handle let* using the procedure we have defined
;; above.
