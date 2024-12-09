#lang sicp

;; Exercise 4.43

;; This problem is more complicated than previous ones, because of its
;; last constraint "Gabrielle's father owns the yacht that is named
;; after Dr.Parker's daughter."

(#%require "./require.rkt")
(#%require "./distinct.rkt")

(define (amb-list l)
  (if (not (null? l))
      (amb (car l) (amb-list (cdr l)))
      (amb)))

(define father-names '(Barnacle Downing Hall Moore Parker))
(define daughter-names '(Gabrielle Lorna Mary Melissa Rosalind))
(define (daughter-father daughter) (cadr daughter))
(define (daughter-name daughter) (car daughter))
(define (father-name father) (car father))
(define (father-daughter father) (cadr father))
(define (father-yacht father) (caddr father))

(define (father-daughter-yacht)
  (let ((Barnacle (list 'Barnacle (amb-list daughter-names) (amb-list daughter-names))))
    (require (not (eq? (father-daughter Barnacle) (father-yacht Barnacle))))
    (require (eq? (father-yacht Barnacle) 'Gabrielle))
    (require (eq? (father-daughter Barnacle) 'Melissa))
    (let ((Downing (list 'Downing (amb-list daughter-names) (amb-list daughter-names))))
      (require (not (eq? (father-daughter Downing) (father-yacht Downing))))
      (require (eq? (father-yacht Downing) 'Melissa))
      (let ((Hall (list 'Hall (amb-list daughter-names) (amb-list daughter-names))))
        (require (not (eq? (father-daughter Hall) (father-yacht Hall))))
        (require (eq? (father-yacht Hall) 'Rosalind))
        (let ((Moore (list 'Moore (amb-list daughter-names) (amb-list daughter-names))))
          (require (not (eq? (father-daughter Moore) (father-yacht Moore))))
          (require (eq? (father-daughter Moore) 'Mary))
          (require (eq? (father-yacht Moore) 'Lorna))
          (let ((Parker (list 'Parker (amb-list daughter-names) (amb-list daughter-names))))
            (require (not (eq? (father-daughter Parker) (father-yacht Parker))))
            (let ((fathers (list Barnacle Downing Hall Moore Parker)))
              (let ((Gabrielle (list 'Gabrielle (amb-list fathers))))
                (require (eq? (father-daughter (daughter-father Gabrielle)) 'Gabrielle))
                (let ((Lorna (list 'Lorna (amb-list fathers))))
                  (require (eq? (father-daughter (daughter-father Lorna)) 'Lorna))
                  (let ((Mary (list 'Mary (amb-list fathers))))
                    (require (eq? (father-daughter (daughter-father Mary)) 'Mary))
                    (let ((Melissa (list 'Melissa (amb-list fathers))))
                      (require (eq? (father-daughter (daughter-father Melissa)) 'Melissa))
                      (let ((Rosalind (list 'Rosalind (amb-list fathers))))
                        (require (eq? (father-daughter (daughter-father Rosalind)) 'Rosalind))
                        (require (eq? (father-yacht (daughter-father Gabrielle)) (father-daughter Parker)))
                        (let ((daughters (list Gabrielle Lorna Mary Melissa Rosalind)))
                          (require (distinct? (map daughter-father daughters)))
                          (require (distinct? (map father-daughter fathers)))
                          (require (distinct? (map father-yacht fathers)))
                          (father-name (daughter-father Lorna)))))))))))))))

;; The program is horrible

(father-daughter-yacht)

;; If we do not know that Mary Ann's last name is Moore, then there
;; are two possible answers:

;; 1. Lorna's father is Downing, this is the same as the answer when
;; we know that Mary Ann's last name is Moore.

;; | Father   | Barnacle  | Downing | Hall      | Moore | Parker   |
;; | Daughter | Melissa   | Lorna   | Gabrielle | Mary  | Rosalind |
;; | yacht    | Gabrielle | Melissa | Rosalind  | Lorna | Mary     |

;; 2. Lorna's father is Parker.

;; | Father   | Barnacle  | Downing  | Hall     | Moore     | Parker |
;; | Daughter | Melissa   | Rosalind | Mary     | Gabrielle | Lorna  |
;; | yacht    | Gabrielle | Melissa  | Rosalind | Lorna     | Mary   |
