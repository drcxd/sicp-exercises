#lang sicp

;; Exercise 4.41
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define floors '(1 2 3 4 5))
(define (multiple-dwelling)
  (let ((result '()))
    (for-each
     (lambda (baker)
       (if (not (= baker 5))
           (for-each
            (lambda (fletcher)
              (if (and (not (= fletcher 5))
                       (not (= fletcher 1)))
                  (for-each
                   (lambda (cooper)
                     (if (and (not (= cooper 1))
                              (not (= (abs (- fletcher cooper)) 1)))
                         (for-each
                          (lambda (miller)
                            (if (> miller cooper)
                                (for-each
                                 (lambda (smith)
                                   (if (and (not (= (abs (- smith fletcher)) 1))
                                            (distinct? (list baker cooper fletcher miller smith)))
                                       (set! result (cons (list (list 'baker baker)
                                                                (list 'cooper cooper)
                                                                (list 'fletcher fletcher)
                                                                (list 'miller miller)
                                                                (list 'smith smith))
                                                          result))))
                                 floors)))
                          floors)))
                   floors)))
            floors)))
     floors)
    result))

(multiple-dwelling)
