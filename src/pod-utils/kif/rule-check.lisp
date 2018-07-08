;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: K; -*-

;;; Purpose: Functions to do a 'structural integrity' test of a collection of SUMO KIF axioms.

(in-package "KIF")

(defun doit ()
  (clrhash *kif-forms-ht*)
  (e:clear-tries)
;  (k:kif2clause-readfile #P"ccts:data;kb;amis.kif")
  (k:kif2clause-readfile "/local/sigma/current/KBs/Merge.kif")
  (loop for i from 1 to 20000
        for axiom = (gethash i k:*kif-forms-ht*)
        when axiom do (e:trie-add axiom))
  ;(show-db)
  ;; Everything is an instance of something...
  (loop for pred in (e:get-predicates)
        for of-what = (e:trie-query `(ki::instance ,pred ?x))
        unless (third of-what) do  
       (format t "~% ~A is not an instance of anything." pred (third of-what)))
  ;; Every class hierarchy terminates with Entity...
  (loop for set-or-class in (remove-duplicates (mapcar #'third (e:trie-query-all `(ki:instance ?x ?y))))
        unless (eql 'ki::entity (last1 (superclasses set-or-class))) do
        (format t "~% Class hierarchy does not lead to Entity: ~a ~a" 
                set-or-class (superclasses set-or-class)))
  ;; Everything in a disjointDecomposition is also a subclass of that thing partitioned...
  (loop for dp in (use-of-pred 'ki::disjointDecomposition) do
       (loop for sub in (cddr dp)
             with super = (second dp)
             unless (find super (superclasses sub))
             do (format t "~% ~A is in ~A but is not a subclass of ~A" sub dp super)))
  ;; ?x in (domain ?x <num> <SetOrClass>) is a Relation.
  (loop for (ignore rel num set-or-class) in (e:trie-query-all '(ki::domain ?x ?y ?z))
        for classes-of = (mapcar #'third  (e:trie-query-all `(ki::instance ,rel ?x))) 
        unless (cl::or (find 'ki::relation (append classes-of (mapappend #'superclasses classes-of)))
                       (e:trie-query `(ki::subrelation ,rel ?x))   ; SUMO has such a rule ???
                       (e:trie-query `(ki::subrelation ?x ,rel)))  ; SUMO has such a rule ???
        do (format t "~% (domain ~A ~A ~A) but ~A is not a Relation." rel num set-or-class rel))
  ;; ?x in (domain <Relation> <num> ?x) is a SetOrClass
  (loop for (ignore rel num set-or-class) in (e:trie-query-all '(ki::domain ?x ?y ?z))
        for all-classes = (cons set-or-class (superclasses set-or-class))
        unless (cl::or (find 'ki::SetOrClass all-classes) (eql 'ki::Entity set-or-class)) do
       (let ((actually (cond ((find 'ki::Quantity all-classes) "Quantity")
                             ((find 'ki::Attribute all-classes) "Attribute")
                             ((find 'ki::Relation all-classes) "Relation")
                             ((find 'ki::Proposition all-classes) "Proposition"))))
         (format t "~% (domain ~A ~A ~A) but ~A does not name a SetOrClass, it is a ~A." 
                 rel num set-or-class set-or-class actually))))


;;; Recall that SUMO says: (partition Relation Predicate Function List)
;;; Recall tht SUMO says: (disjointDecomposition Abstract Quantity Attribute SetOrClass Relation Proposition)
;;;  ... but that I argued that SetOrClass doesn't belong in here because there are lots of
;;;      times where other kinds of Abstract are used in the 3rd slot of domain
;;;            but  (domain domain 3 SetOrClass)
;;;  disjointDecomposition is not covering, so this isn't a big deal. 




;(domain instance 2 SetOrClass)

;;; Toward confirming that every Class is a subclass of Entity...
(defvar *accum* nil)


;;; I use the term 'predicate' to mean x in (x ...). 
;;; SUMO says this: (partition Relation Predicate Function List)
(defun use-of-pred (name)
  "Given x returns axioms (x ...)."
  (loop for i from 1 to 10000
        for axiom = (gethash i k:*kif-forms-ht*)
        when (eql name (car axiom)) collect axiom))

;;; POD This needs to look into partitioning too???
(defun superclasses (class-name)
  "Returns a list of the superclasses of the argument. Appears to be in most-specific-first order."
  (labels ((s-t-aux (name &optional accum)
             (let ((stypes (mapcar #'third (e:trie-query-all `(ki:subclass ,name ?x)))))
		(cond ((null stypes) nil)
		      (t 
		       (append accum
			       stypes
			       (mapappend #'s-t-aux stypes)))))))
    (s-t-aux class-name)))

;;; Check that every domain is something that can be a domain.
  

#|
>>A bit of an aside: It would be nice to be able to make the Vampire query:
>>(and (domain ?X ?Y Attribute) (not (instance ?X Function)))

Clean, in as far as we ignore
>>that the domain of functions is allowed to be other than SetOrClass.
>>
>>Maybe the next ontology validation tool we write ought to write should go
>>through formula checking the types of arguments.
|#