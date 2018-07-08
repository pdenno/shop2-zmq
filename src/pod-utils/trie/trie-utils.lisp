;;; -*- Mode: Lisp; -*-

;;; Copyright 2004, Peter Denno

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


;;; Purpose: Utilities for the reasoning engine.
;;; Date: 2004-08-27


(in-package :trie)

(eval-when (:compile-toplevel :load-toplevel)
  (defstruct trie
    (value nil :type list) 
    (clauses nil :type list)
    (arcs nil :type (or list hash-table))))

(defmacro lt-query ((&rest vars) body)
  "Arrange to call either trie-query-all or trie-query. If variables, sort them out."
  (with-gensyms (full-form query-form)
    `(let* ((,full-form ,body) ; This is NEEDED to handle backquote in the arguments.
	    (,query-form (if (eql 'and (car ,full-form)) (cdr ,full-form) (list ,full-form))))
      ,(if vars
	   `(loop for bindings in (lt-query-aux ,query-form)
	     collect (list ,@(mapcar #'(lambda (var) `(cdr (assoc ',var bindings))) vars)))
	   `(if (eql 'and (car ,full-form))
	     (lt-query-aux ,query-form)
	     (trie-query ,full-form))))))

(defun lt-query-aux (formulas)
  "FORMULAS is a conjunctive list of formulas."
  (flet ((unify* (&rest args)
	   (let ((result (apply #'unify-equal args)))
	     (if (eql result :fail) (return-from lt-query-aux nil) result))))
    (loop for s-query in formulas
	  for new-binds = (loop for result in (trie-query-all s-query) collect (unify* s-query result)) then
	  (loop for bset in new-binds
		for query = s-query do
		(loop for b in bset do (setf query (subst (cdr b) (car b) query)))
		append (loop for new-bset in (loop for result in (trie-query-all query) collect (unify* query result))
			     collect (append new-bset bset)))
	  finally (return new-binds))))
