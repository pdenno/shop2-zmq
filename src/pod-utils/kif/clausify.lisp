
;;; Purpose: Transform KIF from typical FOL form to Clause Normal Form. 

;;; TODO: There is a BUG that leaves a few logical connectives hanging around.
;;;       To see it, run the k:kif-tests arity test.
;;;       They appear to be in formula that ought to be quoted, so
;;;       it is probably worth while to fix that first.
;;; Here is an example of the faulty output:
;;; (holdsDuring ?X3642 (or (not (instance ?X3640 Birth)) (not (experiencer ?X3640 ?X3641))))

;;; (1) Transform => and <=>
;;; (2) Move NOT inward
;;; (3) Rename variables for uniqueness (across existentials)
;;; (4) Skolemize
;;; (5) Remove universals
;;; (6) Apply distributive law (get CNF)
;;; (7) Remove remaining connectives (obtain multiple clauses)
;;; (8) Rename variables

(in-package :k)

(proclaim '(optimize (speed 3) (safety 1)))

#-clause
(defstruct (clause (:print-function print-clause))
  val     ; the actual lisp list representing the clause
  weight  ; symbol count
  index   ; unique identifier
  parents) ; (If KB, its formula index (line in file at which the formula was written)). 

#-clause
(defun print-clause (object stream depth)
  (declare (ignore depth))
  (format stream "[~{~A~^ ~}]" (clause-val object)))

#-clause
(defvar *clause-index* 0 "Provides a unique clause index.")

#-clause
(defmacro new-clause (&rest args)
  `(make-clause ,@args :index (incf *clause-index*)))

(defconstant +kif-internal+ (find-package :ku))

(defvar *zippy* "for diagnostics") 

(defmacro while-progress (&body body)
  (with-gensyms (func)
    `(flet ((,func () (progn ,@body)))
       (loop for result = (funcall #',func)
             when (cl:equal result (funcall #',func))
             return result))))

(defun clausify-string (str)
  "Return the clauses for the string of KIF."
  (let ((*kif-forms-ht* (make-hash-table)))
    (clausify (kif-read-from-string str) nil)))

;;; -- 7 --
(declaim (inline clause-remove-connectives))
(defun clause-remove-connectives (form)
  "(pred ?x) --> ((pred ?x)) ; (and x y) --> (x, y) ; (or x) --> x
   returns a *LIST* of clauses."
  (if (and-p form)
      (setf form (cdr form))
    (setf form (list form)))
  (setf form (mapcar #'(lambda (x) (if (or-p x) (cdr x) (list x))) form))
  form)

;;; -- 8 --
(declaim (notinline clause-rename-variables))
(defun clause-rename-variables (form)
  "Form is actually a LIST of clauses now. And each clause needs unique variables."
  (loop for c in form
        for vars = (kif-collect-vars c)
        for new-vars = (map-in #'unique-var* (copy-list vars)) do
        (mapc #'(lambda (v nv) (nsubst nv v c)) vars new-vars))
  form)

(defvar *clause-container* (make-array 15000 :element-type 'list :adjustable t :fill-pointer 0))
(defun push-clause (f) (vector-push-extend f *clause-container*))

(defmethod kif-forms-clausify ((kif kif-forms) &key (target :container))
  (clear-unique-var)
  (with-slots (forms-ht lines-read) kif
    (loop for key from 1 to lines-read ; I go through these linearly to retain locality 
          for form = (gethash key forms-ht)
          when (cl:and form (cl:not (documentation-p form))) do
         (let ((result #+debug(clausify form key :check-complete t)
                       #-debug(clausify form key)))
           (case target
             #+clause(:passive (loop for c in result do (m:push-passive c)))
             (:container (loop for c in result do (push-clause c))))))))

(defvar *quoted-formulas-ht* (make-hash-table))
(defvar *quoted-positions* (make-hash-table))
(defvar *quoted-cnt* 0)

(defun sumo-fn-p (symbol)
  (let ((name (symbol-name symbol)))
    (string= "Fn" (subseq name (- (length name) 2)))))

(defun sumo-forms-quote (kif-forms)
  "Preprocessing: Search for predicate domains of type Formula and places where a 
   ground term (Formula) is used. Replace those with a constant. -- SUMO Specific!"
  (clrhash *quoted-formulas-ht*)
  (clrhash *quoted-positions*)
  (setf *quoted-cnt* 0)
  (let ((forms-ht (forms-ht kif-forms)))
    ;; Read once to find places where Formula are used.
    (loop for form being the hash-value of forms-ht
	  when (cl:and (eql 'ku::|domain| (first form))
		       (eql 'ku::|Formula| (nth 3 form))) do
        (push (third form) (gethash (second form) *quoted-positions*)))
    ;; Replace use of Formula with gensym. Store the replaced
    (loop for key being the hash-key of forms-ht do
	  (setf (gethash key forms-ht) (quote-sumo-formula (gethash key forms-ht) t)))))

#|
(quote-sumo-formula 
 (kif-readstring
  "(=>
    (inhibits ?PROC1 ?PROC2)
    (forall (?TIME ?PLACE)
 	    (decreasesLikelihood 
	     (holdsDuring ?TIME (exists (?INST1) (and (instance ?INST1 ?PROC1) (located ?INST1 ?PLACE))))
             (holdsDuring ?TIME (exists (?INST2) (and (instance ?INST2 ?PROC2) (located ?INST2 ?PLACE)))))))")
  t)
|#
(defun quote-sumo-formula (form pred-p)
  "Replace a SUMO formula embedded in a predicate with a constant. 
   Record the formula, keyed by the constant."
  (let (indecies)
    (cond ((atom form) form)
	  ((cl:and pred-p (setf indecies (gethash (car form) *quoted-positions*)))
	   (loop for ix in indecies
		 for subform = (nth ix form) with sym = nil
		 when (cl:and (pred-p subform)
			      (cl:not (sumo-fn-p (car subform)))) do
                  (setf sym (intern (format nil "$quoted-formula-~A" (incf *quoted-cnt*))))		      
		  (setf form (append (subseq form 0 ix) (list sym) (subseq form (1+ ix))))
		  finally (return form)))
	  (t (reuse-cons (quote-sumo-formula (car form) t) 
			 (quote-sumo-formula (cdr form) (pred-p (second form)))
			 form)))))

(defun clausify (form key &key check-complete #+clause(ordering #'m:lpo-clause) (one-time-transforms t))
  "Clausify a KIF form. Key is the line number where it was read from (or NIL)."
  (when check-complete (setf *zippy* form))
  (setf form (copy-tree form))
  (setf form (pod:macroexpand-all form))     ;; 1
  (while-progress
    (setf form (clause-not-inward form)) ;; 2 
    (setf form (clause-and/or/not-simplify form t)))
  (when one-time-transforms
    (setf form (clause-rename-variables-exists form t))) ;; 3
  (while-progress
    (setf form (clause-skolemize form))  ;; 4
    (setf form (clause-remove-universals form t)) ;; 5 
    (setf form (clause-distributive form t))      ;; 6
    (setf form (clause-not-inward form))          ;; 2 
    (setf form (clause-and/or/not-simplify form t)))
  (when one-time-transforms
    (setf form (clause-remove-connectives form))  ;; 7
    (setf form (clause-rename-variables form))    ;; 8 
    #+clause(setf form (loop for c in form collect (funcall ordering c))))
  (when check-complete
    (let ((cform (copy-tree form)))
      (unless (cl:equal
               form 
               (setf cform (clausify cform key :one-time-transforms nil)))
        (kif-pprint form)
        (kif-pprint cform)
        (let ((n (kif-form2n *zippy*)))
          (setf *zippy* (list form cform))
          (format t "~% Original weight ~A~% New weight: ~A"
                  (kif-weight form) (kif-weight cform))
          (error "Clausification was not complete: failed formula ~A" n)))))
  ;; Return a list of clause objects (unless this is a diagnostic recursive).
  (if one-time-transforms
      (loop for c in form 
            collect #+clause(m:new-clause :val c :weight (kif-weight c) :parents (list :kb key))
                    #-clause(new-clause :val c :weight (kif-weight c) :parents (list :kb key)))
    form))

;;; -- 1  -- (use these with macroexpand-all)
(defmacro ku:=> (&body forms) ; nbutlast, no help
  `(ku::|or| (ku::|not| (ku::|and| ,@(butlast forms))) ,@(last forms)))

(defmacro ku:<=> (a b)
  `(ku::|and| (ku::|or| (ku::|not| ,a) ,b)
        (ku::|or| ,a (ku::|not| ,b))))

;;; --  2.1 --
;;; (not (and a b)) --> (or (not a) (not b))
;;; (not (or a b)) --> (and (not a) (not b))
;;; (not (exists (?x) a))) --> (forall (?x) (not a))
;;; (not (forall (?x) a))) --> (exists (?x) (not a))
(defun clause-not-inward (form)
  "Move NOTs inward over quantifiers."
  (cond ((atom form) form)
        ((not-p* form)
         #+nil(assert (cl:not (third form))) ; cannot do on (domain not 1 formula)
         (let ((arg (second form)))
           (cond ((exists-p arg)                                                               ; exists
                  (list 'ku::|forall| (second arg) (list 'ku::|not| (clause-not-inward (third arg)))))
                 ((forall-p arg)                                                               ; forall
                  (list 'ku::|exists| (second arg) (list 'ku::|not| (clause-not-inward (third arg)))))
                 ((and-p arg) (nconc (list 'ku::|or|) (mapcar #'negate-form (cdr arg)))) ; demorgans
                 ((or-p arg) (nconc (list 'ku::|and|) (mapcar #'negate-form (cdr arg)))) ; demorgans
                 (t form))))
        (t (rplaca form (clause-not-inward (car form)))    
           (rplacd form (clause-not-inward (cdr form))))))

;;; -- 3 --
;;; (and (exists (?x) (p ?x)) (exists (?x) (q ?x))) -->
;;; (and (exists (?x1) (p ?x1)) (exists (?x2) (q ?x2)))
;;; so that skolemization is not confused. 
(defun clause-rename-variables-exists (form pred-p)
  (cond ((atom form) form)
        ((cl:and (exists-p* form) pred-p)
         (mapc #'(lambda (var) 
                   (nsubst (unique-var) var 
                           (nconc (list 'ku::|exists|)
                                  (list (second form))
                                  (clause-rename-variables-exists (third form) (pred-p (third form))))))
               (copy-list (second form)))
         form)
        (t
         (rplaca form (clause-rename-variables-exists (car form) t))
         (rplacd form (clause-rename-variables-exists (cdr form) (pred-p (second form)))))))

;;;  -- 4 --
;;; Skolemize
(defstruct (skolem-fn (:print-object print-skolem-fn))
  -name -vars)

(declaim (inline new-skolem-fn))
(defun new-skolem-fn (name vars) 
  (make-skolem-fn :-name (gensym (format nil "SK-~A-" name)) :-vars vars))

#+nil(defun new-skolem-fn (name vars) 
       (let ((obj (make-skolem-fn :-name (gensym (format nil "SK-~A-" name)) :-vars vars)))
         (gensym (format nil "~A" obj))))

(defun print-skolem-fn (obj stream)
  (format stream "~A[~{~A~^ ~}]" 
          (skolem-fn--name obj)
          (skolem-fn--vars obj)))

(declaim (inline new-skolem-c))
(defun new-skolem-c (name)
  (gensym (format nil "SK-~A-" name)))

(let (accum)
  (defun exists-subform-p (form)
    "Returns Values an EXISTS form (or nil) and variables uninversally bound outside it."
    (labels ((esp-aux (form pred-p)  
               (cond ((atom form)
                      (when (kvariable-p* form) (pushnew form accum)))
                     ((cl:and (exists-p* form) pred-p)
                      (return-from exists-subform-p (values (nreverse accum) form)))
                     (t (esp-aux (car form) t)
                        (esp-aux (cdr form) (pred-p (second form)))))))
      (setf accum nil)
      (esp-aux form t)))
)

;;; This may need to be called several times.
(defun clause-skolemize (form)
    (mvb (uvars subform)
         (exists-subform-p form)
         (if subform
             (destructuring-bind (ignore evars body)
                 subform
               (declare (ignore ignore))
               (let ((new-body (loop for evar in evars
                                     for sk = (if uvars 
                                                  (new-skolem-fn (subseq (symbol-name evar) 1) uvars)
                                                (new-skolem-c (subseq (symbol-name evar) 1)))
                                     do (nsubst sk evar body)
                                     finally (return body))))
                 (nsubst new-body subform form :test #'eq)))
           form)))

;;; -- 5 --
(defun clause-remove-universals (form pred-p)
  (cond ((atom form) form)
        ((cl:and (forall-p* form) pred-p) (third form))
        (t
         (rplaca form (clause-remove-universals (car form) t))
         (rplacd form (clause-remove-universals (cdr form) (pred-p (second form)))))))

;;; -- 6 --
(defun clause-distributive (form pred-p)
 "Do distributive transformation' (or x (and a b)) --> (and (or x a) (or x b))"
 (let (and-form)
   (cond ((atom form) form)
         ((cl:and (or-p* form) pred-p (setf and-form (find-if #'and-p (cdr form))))
          (let ((ors (delete and-form (cdr form) :test #'eq :count 1)))
            (append (list 'ku::|and|)
                    (mapcar #'(lambda (x) (append (list 'ku::|or|) (copy-tree ors) (list x))) ; can't be nconc
                            (cdr and-form)))))
         (t (reuse-cons (clause-distributive (car form) t) ; can't be rplaca. WHY?
                        (clause-distributive (cdr form) (pred-p (second form)))
                        form)))))

;;; -- * -- It does not recurse on arguments, (due to mapcar) thus might have 
;;; to be called several times.
(defun clause-and/or/not-simplify (form pred-p)
  "Replace all subforms (and (and a) (and b c) d) --> (and a b c d) etc..."
  (cond ((atom form) form)
        ((cl:and (and-p* form) pred-p (cl:not (third form))) ; (and a)
         (clause-and/or/not-simplify (second form) (pred-p (second form))))
        ((cl:and (and-p* form) pred-p (member-if #'and-p (cdr form))) ; (and a (and b))
         (nconc (list 'ku::|and|) (mapnconc #'(lambda (x) (if (and-p x) (cdr x) (list x))) (cdr form))))
        ((cl:and (or-p* form) pred-p (cl:not (third form)))  ; (or a)
         (clause-and/or/not-simplify (second form) (pred-p (second form))))
        ((cl:and (or-p* form) pred-p (member-if #'or-p (cdr form))) ; (or a (or b))
         (nconc (list 'ku::|or|) (mapnconc #'(lambda (x) (if (or-p x) (cdr x) (list x))) (cdr form))))
        ((cl:and (not-p* form) pred-p (consp (second form)) (not-p* (cadr form))) ; (not (not a))
         (clause-and/or/not-simplify (cadadr form) (pred-p (second form))))
        (t (rplaca form (clause-and/or/not-simplify (car form) t))
           (rplacd form (clause-and/or/not-simplify (cdr form) (pred-p (second form)))))))


;;; Utilities
(let ((var-cnt 0))
  (defun clear-unique-var () (setf var-cnt 0))
  (defun unique-var ()
    (let ((var (intern (format nil "?X~A" (incf var-cnt))
                       +kif-internal+)))
      (setf (get var 'kvar) t)
      var))
  (defun unique-var* (ignore)
    "This one so that you can map over it."
    (declare (ignore ignore))
    (let ((var (intern (format nil "?X~A" (incf var-cnt))
                       +kif-internal+)))
      (setf (get var 'kvar) t)
      var))
)

(defun relative-unique-vars (n &key (name "?VAR~A-"))
  (loop for i from 1 to n
        for var = (intern (format nil name i) +kif-internal+)
        do (setf (get var 'kvar) t)
        collect var))

; 2012 This was commented out 'pod8' Now I need it!
(defun var-versions (v num &key (type :variable))
  (loop for i from 1 to num
        for var = (intern (format nil "~A~A" v i)
                          (case type 
                            (:variable +kif-internal+)
                            (:qvar (find-package "TRIE"))))
        do (case type
             (:variable (setf (get var 'kvar) t))
             (:qvar     (setf (get var 'trie::qvar) t)))
        collect var))


#| Test forms

(clausify '(ku::instance ku::or ku::VariableArityRelation) nil)
(clausify '(ku::instance ku::and ku::VariableArityRelation) nil)
(clausify '(ku::instance ku::exists ku::LogicalOperator) nil)
(clausify '(ku::instance ku::forall ku::LogicalOperator) nil)
(clausify '(ku::instance ku::not ku::LogicalOperator) nil)

(pprint 
 (clause-skolemize '(OR (NOT (IMMEDIATEINSTANCE ?ENTITY ?CLASS)) 
                        (EXISTS (?SUBCLASS) 
                                (AND (SUBCLASS ?SUBCLASS ?CLASS) 
                                     (INSTANCE ?ENTITY ?SUBCLASS))))))

(pprint 
 (clause-skolemize '(<=> (intermittent ?rel @ROW ?ti)
                         (and (exists (?t1)
                                      (and
                                       (TimeInterval ?t1)
                                       (temporallyBetween (BeginFn ?ti) (BeginFn ?t1) (EndFnd ?ti))
                                       (holdsDuring ?t1 ?f)))
                              (exists (?t1)
                                      (and
                                       (TimeInterval ?t1)
                                       (temporallyBetween (BeginFn ?ti) (BeginFn ?t1) (EndFnd ?ti))
                                       (holdsDuring ?t1 (not (holds ?f)))))))))
|#
  
