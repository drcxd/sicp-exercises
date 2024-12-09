#lang sicp

(define (require p)
  (if (not p)
      (amb)))

(#%provide require)
