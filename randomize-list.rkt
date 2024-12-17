#lang sicp

(#%require "./remove-from-list.rkt")

(define (randomize-list l)
  (if (null? l) '()
      (let ((len (length l)))
        (let ((idx (random len)))
          (let ((x (list-ref l idx)))
            (cons x (randomize-list (remove-from-list l x))))))))

(#%provide randomize-list)
