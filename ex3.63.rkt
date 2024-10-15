;; Exercise 3.63

;; In Louis' version, each call to sqrt-stream creates a new stream.
;; Iterating through the stream requires computing every element
;; before the program reaches the target element. However, these
;; elements has already been computed in previous calls to
;; sqrt-stream. The original version uses only one stream, thus, only
;; the first access to each element in the stream requires
;; computation. If the delay is not optimized by memo-proc, then there
;; will be no performance difference between these two versions.
