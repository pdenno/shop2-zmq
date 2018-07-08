;;; -*- Mode: Lisp; Package: KIF; -*-

;;; Purpose: Utilities for KIF.
;;; Peter Denno

(in-package :K)

(defclass kif-forms ()
  ((forms-ht :accessor forms-ht :initform (make-hash-table))
   (lines-read :accessor lines-read :initform 0)
   (files-read :accessor files-read :initform nil)))

#-debug(declaim (inline and-p and-p* or-p or-p* not-p not-p* exists-p exists-p*
                        forall-p forall-p* =>-p =>-p* <=>-p <=>-p* equal-p equal-p* 
                        pred-p predicate-p))
(defun and-p  (form) (cl:and (consp form) (eql 'ku::|and| (car form))))
(defun and-p* (form) (eql 'ku::|and| (car form)))
(defun or-p   (form) (cl:and (consp form) (eql 'ku::|or| (car form))))
(defun or-p*  (form) (eql 'ku::|or| (car form)))
(defun not-p  (form) (cl:and (consp form) (eql 'ku::|not| (car form))))
(defun not-p* (form) (eql 'ku::|not| (car form)))
(defun exists-p (form) (cl:and (consp form) (eql 'ku::|exists| (car form))))
(defun exists-p* (form) (eql 'ku::|exists| (car form)))
(defun forall-p (form) (cl:and (consp form) (eql 'ku::|forall| (car form))))
(defun forall-p* (form) (eql 'ku::|forall| (car form)))
(defun =>-p  (form) (cl:and (consp form) (eql '=> (car form))))
(defun =>-p* (form) (eql '=> (car form)))
(defun <=>-p  (form) (cl:and (consp form) (eql '<=> (car form))))
(defun <=>-p* (form) (eql '<=> (car form)))
(defun equal-p (form) (cl:and (consp form) (eql 'ku::|equal| (car form))))
(defun equal-p* (form) (eql 'ku::|equal| (car form)))
(defun pred-p (form) (consp form))
(defun predicate-p (form) 
  (when-bind (f (cl:and (consp form) (car form)))
    (cl:and (cl:not (eql f 'ku::|and|)) (cl:not (eql f 'ku::|or|)) (cl:not (eql f 'ku::|not|))
            (cl:not (eql f 'ku::|exists|)) (cl:not (eql f 'ku::|forall|))
            f)))

(defun find-forall-p (form)
  (cond ((cl:and (atom form) (eql form 'ku::|forall|)) t)
        ((atom form) nil)
        (t (cl:or (find-forall-p (car form))
                  (find-forall-p (cdr form))))))

;;; The * ones are for thing not read by this reader (which does set-variable-property
;;; and sets the property 'variable). Forms typed in to the listener, during debugging,
;;; will not have this property set. Hence use of #+debug.
(declaim (inline kvar-p kvar-p* rowvar-p rowvar-p* kvariable-p kvariable-p* skolem-p))
(defun kvar-p* (x) (cl:and (symbolp x) (char= #\? (char (symbol-name x) 0))))
#-debug(defun kvar-p (x) (cl:and (symbolp x) (get x 'kvar)))
#+debug(defun kvar-p (x) (var-p* x))

(defun rowvar-p* (x) (cl:and (symbolp x) (char= #\@ (char (symbol-name x) 0))))
#-debug(defun rowvar-p (x) (cl:and (symbolp x) (get x 'row-variable)))
#+debug(defun rowvar-p (x) (rowvar-p* x))

(defun kvariable-p* (x) (cl:or (kvar-p* x) (rowvar-p* x)))
#-debug(defun kvariable-p (x) (cl:or (kvar-p x) (rowvar-p x)))
#+debug(defun kvariable-p (x) (cl:or (kvar-p* x) (rowvar-p* x)))

(declaim (inline documentation-p))
;;; These are SUMO or Sigma specific
(defun documentation-p (form) (cl:and (consp form) (eql 'ku::|documentation| (car form))))

#-debug(declaim (inline negate-form))
(defun negate-form (form)
  (if (cl:and (consp form) (not-p* form))
      (cadr form)
    (list 'ku::|not|  form)))

(defmethod kif-pprint ((ht hash-table) &key (stream *standard-output*))
  (loop for i from 1 to *kif-line*
        for form = (gethash i ht)
        when form
        do (format stream "~%~A: " i)
        (kif-pprint form :stream stream)))

(defmethod kif-pprint ((form list) &key (stream *standard-output*))
  ;(*pprint-miser-width* 20)
  (let ((*package* (find-package :ki)))
    (pprint form stream)))

(defmethod kif-pprint ((vec vector) &key (stream *standard-output*))
  ;(*pprint-miser-width* 20)
  (let ((*package* (find-package :ki)))
    (loop for clause across vec 
	  do (pprint clause stream))))

#+uno
(defmethod kif-pprint ((arr array) &key (stream *standard-output*))
  (loop for i from 0 to (1- (fill-pointer arr))
        for clause = (aref arr i) with last-ix = 0
        when (m:clause-parents clause) do
        (unless (= last-ix (m:clause-parents clause))
          (setf last-ix (m:clause-parents clause))
          (format stream "~%~A:" last-ix))
        do (format stream "~A" clause)))

(defun kif-weight (f)
  (cond ((null f) 0)
        ((atom f) 1)
        (t
         (+ (kif-weight (car f))
            (kif-weight (cdr f))))))

;;; POD There is a way to do clause-rename-variables without consing!!! DO IT!
(defun kif-collect-vars (f)
  (let (vars)
    (labels ((kcv (x)
               (cond ((kvar-p x) (pushnew x vars))
                     ((atom x) nil)
                     (t (kcv (car x))
                        (kcv (cdr x))))))
      (kcv f)
      vars)))

(defun kif-collect-rowvars (f)
  (let (vars)
    (labels ((kcv (x)
               (cond ((rowvar-p* x) (pushnew x vars))
                     ((atom x) nil)
                     (t (kcv (car x))
                        (kcv (cdr x))))))
      (kcv f)
      (reverse vars))))







               
                       





