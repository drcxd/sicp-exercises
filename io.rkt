#lang sicp

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (display-line obj)
  (display obj)
  (newline))

(#%provide prompt-for-input
           announce-output
           display-line)
