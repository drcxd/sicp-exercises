(define (add)
  +)

((add) 1 2)

;; if the operator is not forced, then the evaluator will apply the
;; thunk upon the operands, which is undefined.
