
(in-package :cl-user)

(defpackage KU
  (:nicknames :kif-user)
  (:use )
  (:export 
   #:=> 
   #:<=> 
   #:|and| 
   #:|equal| 
   #:|exists| 
   #:|False| ; pod8
   #:|forall| 
   #:|instance| 
   #:|not| 
   #:|or| 
   #:|subclass|
   #:|True| ; pod8
   ;; These are 2012
   #:|lessThan| 
   #:|greaterThan| 
   #:|lessThanOrEqualTo| 
   #:|greaterThanOrEqualTo|
   #:|AdditionFn| 
   #:|SubtractionFn| 
   #:|MultiplicationFn| 
   #:|DivisionFn|))


(defpackage K
  (:nicknames :kif)
  (:use ku cl pod trie)
  (:export
    #:*CLAUSE-CONTAINER*
    #:*KIF-FORMS*
    #:<=>-P
    #:<=>-P*
    #:=>-P
    #:=>-P*
    #:AND-P
    #:AND-P*
    #:CLAUSE-INDEX
    #:CLAUSE-RENAME-VARIABLES
    #:CLAUSIFY
    #:CLAUSIFY-STRING
    #:EXISTS-P
    #:EXISTS-P*
    #:FIND-FORALL-P
    #:FORALL-P
    #:FORALL-P*
    #:FORMS-HT
    #:KIF-COLLECT-ROWVARS
    #:KIF-EXPORT-SAFE-SYMBOLS
    #:KIF-FORMS
    #:KIF-FORMS-CLAUSIFY
    #:KIF-N2FORM
    #:KIF-PPRINT
    #:KIF-READFILE
    #:KIF-READFILE
    #:KIF-READ-FROM-STRING
    #:KIF-SHOW-FORMS
    #:KIF-WEIGHT
    #:KVAR-P
    #:KVAR-P*
    #:KVARIABLE-P
    #:KVARIABLE-P*
    #:KVARS
    #:LINES-READ
    #:NOT-P
    #:NOT-P*
    #:OR-P
    #:OR-P*
    #:PRED-P
    #:PREDICATE-P
    #:RELATIVE-UNIQUE-VARS
    #:SKOLEM-FN-P
    #:SUMO-FORMS-QUOTE
    #:VAR-VERSIONS))

