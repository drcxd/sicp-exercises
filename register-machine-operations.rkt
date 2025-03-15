#lang sicp

(#%require "./metacircular-evaluator-syntax.rkt")
(#%require "./metacircular-evaluator-env.rkt")
(#%require "./io.rkt")

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define machine-operations
  (list
   (list 'self-evaluating? self-evaluating?)
   (list 'variable? variable?)

   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)

   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)

   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)

   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-alternative if-alternative)
   (list 'if-consequent if-consequent)

   (list 'cond? cond?)
   (list 'cond->if cond->if)

   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)

   (list 'begin? begin?)
   (list 'begin-actions begin-actions)

   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'empty-arglist empty-arglist)
   (list 'first-operand first-operand)
   (list 'last-operand? last-operand?)
   (list 'adjoin-arg adjoin-arg)
   (list 'rest-operands rest-operands)

   (list 'let? let?)
   (list 'let->combination let->combination)

   (list 'make-procedure make-procedure)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-environment procedure-environment)
   (list 'procedure-body procedure-body)

   (list 'compiled-procedure? compiled-procedure?)
   (list 'make-compiled-procedure make-compiled-procedure)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'compiled-procedure-env compiled-procedure-env)

   (list 'lookup-variable-value lookup-variable-value)
   (list 'extend-environment extend-environment)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'get-global-environment get-global-environment)
   (list 'lexical-address-lookup lexical-address-lookup)
   (list 'lexical-address-set! lexical-address-set!)

   (list 'first-exp first-exp)
   (list 'last-exp? last-exp?)
   (list 'rest-exps rest-exps)

   (list 'apply-primitive-procedure apply-primitive-procedure)

   (list 'true? true?)
   (list 'false? false?)

   (list 'or? or?)
   (list 'or->application or->application)

   (list 'and? and?)
   (list 'and->application and->application)

   (list 'prompt-for-input prompt-for-input)
   (list 'read read)
   (list 'announce-output announce-output)
   (list 'user-print user-print)

   (list 'list list)
   (list 'cons cons)
   (list '+ +)
   (list '- -)
   (list '* *)
   (list '= =)))

(#%provide machine-operations)
