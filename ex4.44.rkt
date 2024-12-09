#lang sicp

;; Exercise 4.44

(#%require "./require.rkt")

(define (safe? k positions)
  (define (get-last pos)
    (if (null? (cdr pos))
        (car pos)
        (get-last (cdr pos))))
  (let ((new (get-last positions)))
    (define (iter i pos)
      (if (= i k)
          #t
          (let ((old (car pos)))
            (and (not (= (car new) (car old)))
                 (not (= (cadr new) (cadr old)))
                 (not (= (abs (- (car new) (car old)))
                         (abs (- (cadr new) (cadr old)))))
                 (iter (+ 1 i) (cdr pos))))))
    (iter 1 positions)))

(define (queens board-size)
  (define (iter i board)
    (if (> i board-size)
        board
        (begin
          (let ((new-board (append board (list (list (amb 1 2 3 4 5 6 7 8) i)))))
            (require (safe? i new-board))
            (iter (+ i 1) new-board)))))
  (iter 1 '()))

(queens 8)
