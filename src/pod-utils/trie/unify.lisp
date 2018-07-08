;;; -*- Mode: Lisp; -*-

;;; Copyright Peter Denno, 2005

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the
;;; Software, and to permit persons to whom the Software is furnished
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
;;; ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;-----------------------------------------------------------------------


;;; Purpose: Unification Algorithm, mostly from Peter Norvig's
;;;          "Paradigms of Artificial Intelligence Programming"
;;; 
;;; Unification = a association between variables an expressions where:
;;;        (1) each variable is bound with at most one expression
;;;        (2) no variable bound appears in any bound expression
;;; ... (and the expressions are made to be the same ;^)

;;;  2005-12-01 There are 5 references of qvar-p here. They were references to qvar-p.
;;;                I'm afraid this breaks UNO. 
;;;
;;;  2007-11-17 Added unify-equal and unify-variable-equal. Nothing modified.

(in-package :trie)

;;; Norvig Page 353...
(defparameter *occurs-check* t "Should we do the occurs check?")

(defconstant no-bindings 
  (if (boundp 'no-bindings)
      (symbol-value 'no-bindings)
    '((t . t)))
  "Indicates pat-match success, with no variables.")

(declaim (inline get-binding))
(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(declaim (inline binding-val))
(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(declaim (inline lookup))
(defun lookup (var bindings)
  "Get the value part (for var) from the binding list."
  ;(format t "~% looking for ~A" var)
  (binding-val (get-binding var bindings)))

(declaim (inline extend-bindings))
(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
       ;; Once we add a "real" binding,
       ;; we can get rid of the dummy no-bindings
        (if (cl:and (eq bindings no-bindings))
          nil
          bindings)))

#|
(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t :fail))))
|#

(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eql bindings :fail) :fail)
        ((eql x y) bindings)
        ((qvar-p x) (unify-variable x y bindings))
        ((qvar-p y) (unify-variable y x bindings))
        ((cl:and (consp x) (consp y))
          (unify (rest x) (rest y)
                 (unify (first x) (first y) bindings)))
        (t :fail)))

(defun unify-equal (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eql bindings :fail) :fail)
        ((equal x y) bindings)
        ((qvar-p x) (unify-variable-equal x y bindings))
        ((qvar-p y) (unify-variable-equal y x bindings))
        ((cl:and (consp x) (consp y))
          (unify-equal (rest x) (rest y)
                 (unify-equal (first x) (first y) bindings)))
        (t :fail)))

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((cl:and (qvar-p x) (get-binding x bindings)) ; new (a) if both bound variables, unify against
         (unify var (lookup x bindings) bindings))     ; bound value (might be same as var).
        ((cl:and *occurs-check* (occurs-check var x bindings))
         :fail)
        (t (extend-bindings var x bindings))))

(defun unify-variable-equal (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify-equal (lookup var bindings) x bindings))
        ((cl:and (qvar-p x) (get-binding x bindings)) ; new (a) if both bound variables, unify against
         (unify var (lookup x bindings) bindings))     ; bound value (might be same as var).
        ((cl:and *occurs-check* (occurs-check var x bindings))
         :fail)
        (t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((cl:and (qvar-p x) (get-binding x bindings))
         (occurs-check var (lookup x bindings) bindings))
        ((consp x)
         (cl:or (occurs-check var (first x) bindings)
                (occurs-check var (rest x) bindings)))
        (t nil)))

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
   taking recursively bound variables into account."
  (cond ((eq bindings :fail) :fail)
        ((eq bindings no-bindings) x)
        ((and (qvar-p x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (pod:reuse-cons (subst-bindings bindings (car x))
			   (subst-bindings bindings (cdr x))
			   x))))

