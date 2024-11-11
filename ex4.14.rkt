#lang sicp

;; Exercise 4.14

;; I guess this is because map calls functions. The functions are
;; defined in the meta-circular interpreter but map is defined in the
;; underlying interpreter. The way the underlying map calls functions
;; can not be applied to functions defined in the meta-circular
;; interpreter. Also, the underlying map contains a different
;; environment.
