#lang sicp

;; Exercise 4.40

;; Before the distinct requirement, there are 5^5 = 3125 floor
;; assignments, since there are 5 different people and each can be
;; assigned to one of the five floors.

;; After the distinct requirement, there are 5! = 120 floor
;; assignments.

(#%require "./require.rkt")
(#%require "./distinct.rkt")

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (let ((fletcher (amb 1 2 3 4 5)))
      (require (not (= fletcher 1)))
      (require (not (= fletcher 5)))
      (let ((cooper (amb 1 2 3 4 5)))
        (require (not (= cooper 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (let ((miller (amb 1 2 3 4 5)))
          (require (> miller cooper))
          (let ((smith (amb 1 2 3 4 5)))
            (require (not (= (abs (- smith fletcher)) 1)))
            (require (distinct? (list baker fletcher cooper miller smith)))
            (list (list 'baker baker) (list 'cooper cooper)
                  (list 'fletcher fletcher) (list 'miller miller)
                  (list 'smith smith))))))))