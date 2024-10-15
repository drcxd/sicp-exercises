;; Exercise 3.66

;; 1. Element (1, 1) is the first element, alternatively, it is
;; element 1.

;; 2. If element (i, i) is element x, then element (i, i+1) is
;; element x + 2^(i-1).

;; 2.a Element (i, i) is element 2^i - 1.

;; 3. If element (i, i+1) is element x, then element (i, j), j > 1,
;; is element x + (j-(i+1)) * 2^i.

;; 4. If element (i, i+1) is element x, then element (i+1, i+1) is
;; element x + 2^(i-1).

;; Using the above rules, we can compute the index of any element in
;; the stream.

;; To compute the index of (1, 100) we start with (1, 1), which is
;; element 2^1 - 1 = 1; then (1, 2) is element 1 + 2^(1-1) = 2; and
;; finally (1, 100) is element 2 + (100-(1+1)) * 2^1 = 198.

;; Similarly, to compute (99, 100), we start with (99, 99), which is
;; element 2^99 - 1; (99, 100) is element 2^99 - 1 + 2^(99-1).

;; (100, 100) is element 2^100 - 1.
