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

;;; Purpose: Implements perfect discrimination trees. Started with Norvig, Pg 344.
;;; Date: 2004-09-04

;;; TODO:
;;;   DONE - Probably an error to do clear-tries-reslist!!!
;;;        - Clean up function bind-val
;;;        - Does navigate-next need to think about unbalanced paren?
;;;   DONE - 2005-03-13 Undid the close-mark !
;;;   DONE - 2005-03-12 find-trie loops not recurses, close-mark not :c on tries (:c on user key).
;;;   DONE - consider destructive version
;;;   DONE - put clause references on the leaf
;;;   UNTESTED - write trie-delete to remove clause references, (only ???)
;;;   NEEDS WORK - ability to store clauses
;;;   DONE - variable arity 
;;;   DONE - trie-query-next
;;;   DONE - break off find-trie extend=T from query trie (save one argument and one test).
;;;   DONE - make the two follow-arcs in-line.
;;;   DONE - switch to hash-tables for trie-arcs when number exceeds some threshold.
;;;   - flatterms
;;;   DONE - alloc of tries from a free list
;;;   WORTHLESS - use vector to record clause index in trie, rather than list. 
;;;   - if *preds* sticks around, it might be a good way to put a root tries 
;;;     on the property lists (passive, active). Otherwise duplicating some work.
;;;   DONE - Replace the use of serialize with something that doesn't require consing. 
;;;   - ??? Need a "quoting" thing that prevents a form from being decomposed during serialization.
;;;   DONE - I think I'd like (p ?x) to find (p (b)).
;;;   - The cmucl manual suggest lists are almost never the right choice for speed. 
;;;     Where else could I get rid of them? 
;;;       -  The stack used for backtracking 
;;;       -  binding-rec-path, 
;;;       -  key in trie-arcs-lookup
;;;   - Use negative clause numbers to signify a negated clause. 
;;;   - (How to show deleted clauses?) Looking for tautologies means looking for n and -n in 
;;;     the term tree. Could have a delta register to record what in the d-trees has changed 
;;;     (to check for tautologies, unit conflict and contradiction). 
;;;   - 2005-12-01 : Substitute +special-value-nil+ instead of nil storage and retrieval functions
;;;   - 2006-05-18 : Mods for storing strings (don't use eql). 
;;;   - 2007-04-27 : trie-remove.
;;;   - 2007-11-04 : trie database and hash tables for roots. 
;;;   - 2007-11-11 : Oops. Should have bound *trie-db* in with-trie-db!
;;;   - 2008-02-05 : Replaced use of :c and :o with trie::close-paren trie::open paren 
;;;                  (What made me think I could get away with that!)            
;;;   - 2010-02-25 : Added zero-ary case for show-db
;;;
;;; On Lispworks, higher levels of safety appear to not inline things declaimed inline.
;;; When querying a big trie, that can result in a stack overflow. 
;;; Next day (2005-03-13): I remove this because the new find-trie loop might make
;;; the optimization level inconsequential. 
;;;(declaim (optimize (safety 1) (speed 3)))

;(proclaim '(optimize (safety 1) (speed 3)))
#-sbcl(proclaim '(optimize (safety 3) (speed 1))) ; new problem with get-trie at higher optimizations
#+sbcl(proclaim '(optimize (safety 3) (speed 0)))

(in-package :trie)

;;; ======= Trie DB management ===============================
;;; An object that holds the resources of a 'trie database,' a tree of tries
;;; with a common root. See the corresponding special variables for descriptions 
;;; of the slots. 
(defclass trie-db ()
  ((name :reader name :initarg :name)
   (free-tries 
    :initform 
    (pod:new-reslist #-sbcl 10000 #+sbcl 1000 :element-type 'trie :stock-fn #'make-trie :resize 1000))
   (tries-in-use
    :initform
    (pod:new-reslist #-sbcl 10000 #+sbcl 1000 :element-type 'trie :resize #-sbcl 5000 #+sbcl 500))
   (trie-hts
    :initform
    (pod:new-reslist 50 :element-type 'hash-table ;; 2006-05-18, storing strings
		     :stock-fn #'(lambda () (make-hash-table :test #'equal)) :resize 50))
   ;; A hash table whose keys are predicates (the car symbol of everylist pushed into the DB).
   ;; and whose values are the root trie for the symbol.
   (roots :reader roots :initform (make-hash-table))))

(defparameter *trie-db* (make-instance 'trie-db :name :default-tries))
(defparameter *the-databases* (list *trie-db*))

(defun clear-all-trie-dbs ()
  "Start from scratch, as though you just loaded the code."
  (setf *trie-db* (make-instance 'trie-db :name :default-tries))
  (setf *the-databases* (list *trie-db*)))

(defparameter *free-tries* (slot-value *trie-db* 'free-tries) 
  "Dynamically bound to the free-tries reslist (or use this default).")

(defparameter *tries-in-use* (slot-value *trie-db* 'tries-in-use) 
  "Dynamically bound to the tries in use reslist (or use this default).")

(defparameter *trie-hts* (slot-value *trie-db* 'trie-hts) 
  "Dynamically bound to the hashtables that may be used by the tries (or use this default).")

;;; 2013-02-04
(defvar *locks* (make-hash-table :test #'equal))

;;; 2013-02-04
(defun ensure-lock (name)
  (or (gethash (string name) *locks*)
      (setf (gethash (string name) *locks*)
	    (bt:make-lock (string name)))))

(defmacro with-trie-db ((db-name) &body body)
  "Macro to bind temporarily use the argument trie database, DB"
  (with-gensyms (db)
    `(bt:with-lock-held ((ensure-lock ,db-name)) ; 2013-02-04
       (let ((,db (find ,db-name *the-databases* :key #'name :test #'equal)))
	 (unless ,db (error "No such trie db: ~S" ,db-name))
	 (with-slots (free-tries tries-in-use trie-hts) ,db
	   (let ((*trie-db* ,db)
		 (*free-tries* free-tries)
		 (*tries-in-use* tries-in-use)
		 (*trie-hts* trie-hts))
	     (declare (special *trie-db* *free-tries* *tries-in-use* *trie-hts*))
	     ,@body))))))

(defun find-db (db-name)
  (find db-name *the-databases* :key #'name))

(defun ensure-trie-db (db-name)
  "Make a trie database, or if one already exists by this DB-NAME, clear it.
   Returns (values)."
  (if (find db-name *the-databases* :key #'name :test #'equal)
      (with-trie-db (db-name) (clear-tries))
      (push (make-instance 'trie-db :name db-name) *the-databases*))
  (values))

(defmethod print-object ((obj trie-db) stream)
  (with-slots (name roots) obj
      (format stream "#<Trie-db ~S (~A predicates)>" name (hash-table-count roots))))

;;;====================== Trie Implementation (the hard stuff) ==================

#-sbcl(declaim (inline arc-key arc-trie))
(defun arc-key (arc) (car arc))
(defun arc-trie (arc) (cdr arc))

#-sbcl(declaim (inline trie-add-clause))
(defun trie-add-clause (trie clause)
  (pushnew clause (trie-clauses trie)))

#-sbcl(declaim (inline get-trie get-trie-ht free-trie))
(defun get-trie () 
  (let ((trie (pod:reslist-pop *free-tries*)))
    (pod:reslist-push trie *tries-in-use*)
    (unless trie (error "get-trie failed."))
    trie))

(defun get-trie-ht () (pod:reslist-pop *trie-hts*))

(defun free-trie (trie) 
  "Free an unused trie. Disassemble it for easier GC."
  (when (hash-table-p (trie-arcs trie))
    (pod:reslist-push (clrhash (trie-arcs trie)) *trie-hts*))
  (setf (trie-value trie) nil)
  (setf (trie-clauses trie) nil)
  (setf (trie-arcs trie) nil)
  (pod:reslist-push trie *free-tries*))

(defun clear-tries-reslist () 
  (let ((used (pod:reslist-arr *tries-in-use*)))
    (declare (type (vector trie *) used))
    (loop for i from 0 to (1- (pod:reslist-fillptr *tries-in-use*))
	  for trie = (aref used i)
          when trie do (free-trie (aref used i))) ; 2007-02-14 added when trie 
    (setf (pod:reslist-fillptr *tries-in-use*) 0)))

(declaim (inline longer-than))
(defun longer-than (lis n)
  (declare (type cons lis) (type fixnum n))
  (nth n lis))

(defun substitute-svn (key)
  "Can't use subst here because it puts svn on the null cdr of a list."
  (cond ((null key) 'special-value-nil)
	((not (consp key)) key)
	(t (loop for k in key collect (substitute-svn k)))))

(eval-when (:compile-toplevel :load-toplevel)
(defstruct termbuf
  (fill-pointer 0 :type integer) ; 2013-02-04 2000? (It is max number of tokens in expression.)
  (arr (make-array 2000 :initial-element t :element-type 'symbol) :type simple-vector)))

;2013-02-04(defconstant +termbuf+ 
;2013-02-04  (if (boundp '+termbuf+)
;2013-02-04      (symbol-value '+termbuf+)
;2013-02-04    (make-termbuf)))
(defvar +termbuf+ (make-termbuf))

;;; POD This is horrible! See clojure version!
(defun serialize* (input tb)
  "Flatten out a list replacing open and close parentheses with constants :o and :c."
  (declare (type list input) (type termbuf tb))
  (macrolet ((tcons (x) `(progn (setf (aref arr iptr) ,x)
				(when (> iptr 1999) (error "Overran termbuf!"))
				(incf iptr))))
    (let ((arr (termbuf-arr tb))
	  (iptr 0))
      (declare (type integer iptr) (type simple-vector arr))
      (setf (termbuf-fill-pointer tb) 0)
      ;;(loop for i from 0 to 1999 do (setf (aref arr i) t)) ; useless and costly!
      (labels ((s-internal (input new)
                 (declare #|(type (or list symbol number string k:skolem-fn) input)|#
		  (type (member nil t) new))
                 (cond ((null input) (tcons 'close-paren))
                       ((atom input) (tcons input))
                       (new
			(tcons 'open-paren)
			(s-internal (first input) t)
			(s-internal (rest input) (consp (first input))))
                       (t
			(s-internal (first input) t)
			(s-internal (rest input) nil)))))
        (s-internal input t)
        (setf (aref arr (1- iptr)) t) ; ignore outer parentheses (by skipping last :c & Starting at 1?). 
        (setf (termbuf-fill-pointer tb) (1- iptr))))
  tb))

#-sbcl(declaim (inline trie-arcs-lookup))
#+sbcl(declaim (notinline trie-arcs-lookup))
#+sbcl(declaim (notinline trie-arcs-lookup))
(defun trie-arcs-lookup (key trie)
  "Find the trie at key in the argument trie. Trie arcs might be an alist or ht.
   Key can be a object or list of objects with balanced :o and :c."
  (let ((arcs (when trie (trie-arcs trie))))
    (cond ((null trie) nil)
          ((null key) trie)
          ((atom key)
            (if (listp arcs)
              (arc-trie (assoc key arcs :test #'equal)) ; pod 2006-05-18, storing strings...
              (gethash key arcs)))
          (t (trie-arcs-lookup (cdr key)
                      (trie-arcs-lookup (car key) trie))))))

(defun trie-arcs-push (child-trie key parent-trie)
   "Add a new trie at KEY. Switch to ht if exceeds max for assoc-list."
  (declare (type trie child-trie parent-trie) #|(type (or list symbol number string k:skolem-fn) key)|#)
  (let ((arcs (trie-arcs parent-trie)))
    (declare (type (or hash-table list) arcs))
    (cond ((hash-table-p arcs) (setf (gethash key arcs) child-trie))
          ((null arcs) (setf (trie-arcs parent-trie) (list (cons key child-trie))))
          ((and (consp arcs) (longer-than arcs 5)) ; threshold
            (let ((ht (get-trie-ht)))
              (loop for arc in arcs do (setf (gethash (arc-key arc) ht) (arc-trie arc)))
              (setf (gethash key ht) child-trie)
              (setf (trie-arcs parent-trie) ht)))
          (t
            (push (cons key child-trie) (trie-arcs parent-trie))))))

;;; POD I don't bother to switch back to assoc lists.
(defun trie-arcs-pop (key trie)
  "Remove the arc at KEY."
  (declare (type trie trie))
  (let ((arcs (trie-arcs trie)))
    (declare (type (or hash-table list) arcs))
    (if (hash-table-p arcs)
	(remhash key arcs) 
	(setf (trie-arcs trie)
	      (remove key (trie-arcs trie) :test #'equal :key #'car))))
  (values))

(defun clear-tries ()
    "Remove all the tries for all the predicates."
    (format t "~%Clearing trie DB ~A" *trie-db*)
    (clear-tries-reslist)
    (clrhash (roots *trie-db*)))

(defun qvar-versions (n &key (prefix "?X") (package *package*))
  "Exported function to return N qvar symbols named ?PREFIX, (defaults to 'X') 
   interned in PACKAGE (defaults to *package*)."
  (loop for i from 1 to n
	for var = (intern (format nil "~A~A" prefix i) (find-package package))
	do (setf (get var 'qvar) t)
	collect var))
  
(defun db-predicates (&key symbol.arity)
  "Exported function returning the predicates hash table or a list of predicates
   and their arity (of course, arity isn't fixed, but often constant)."
  (unless symbol.arity (return-from db-predicates (roots *trie-db*)))
  (loop for pred being the hash-key of (roots *trie-db*)
	collect (cons pred
		      (loop for i from 1 to 6
			    when (trie-query `(,pred ,@(qvar-versions i :package (find-package :trie))))
			    return i))))

(defun show-db (&key pred (stream *standard-output*) (pre-write "--- ") (post-write ""))
  "Write content to STREAM."
  (let ((query-vars (mapcar #'(lambda (n) (qvar-versions n :package (find-package :trie))) '(0 1 2 3 4 5 6))))
    (if pred
	(loop for qvars in query-vars
	      for query = `(,pred ,@qvars) do
	      (loop for term in (trie-query-all query) 
		    do (format stream "~%~A~S~A" pre-write term post-write)))
	(let ((preds (sort (loop for key being the hash-key of (roots *trie-db*) collect key) #'string<)))
	  (loop for pred in preds do
		(loop for qvars in query-vars
		      for query = `(,pred ,@qvars) do
		      (loop for term in (trie-query-all query) 
			    do (format stream "~%~A~S~A" pre-write term post-write))))))))

(defun write-db (&key (stream *standard-output*))
  "Write a trie-db to STREAM, suitable for re-reading."
  (format stream ";;; Output of trie-db content generated ~A" (now))
  (format stream "~2%(ensure-trie-db ~S)"  (name *trie-db*))
  (format stream "~%(with-trie-db (~S)" (name *trie-db*))
  (show-db :stream stream :pre-write "  (trie-add '" :post-write ")")
  (format stream ")"))

(defun ensure-trie (predicate)
  "Get or make a trie for this predicate (the symbol at the car of the list)."
  (declare (type symbol predicate))
  (or (gethash predicate (roots *trie-db*))
      (setf (gethash predicate (roots *trie-db*)) (get-trie))))

#-sbcl(declaim (inline predsym))
(defun predsym (relation) (first relation))

;;; ----------   Adding ---------------------------
(defun trie-add (key &optional clause-ix) ; POD 2005-11-30 &optional is new. OK???
  "Toplevel function: Set the value of the key in trie."
  (declare (type list key))
  (unless (symbolp (car key)) (error "Malformed predicate: ~A" key))
  (let* ((root (ensure-trie (predsym key)))
	 (key (substitute-svn key))
         (tbuf (serialize* key +termbuf+))
         (trie (find-trie-adding tbuf root)))
    (declare (type termbuf tbuf))
    (if trie ; added when 2013-02-04 Should not be necessary! Concurrency problem???
	(progn (setf (trie-value trie) key)
	       (trie-add-clause trie clause-ix))
	(error "trie-add failed on key=~A, clause-ix=~A" key clause-ix)))
  (values))

(defvar *trie-trace* nil "collects path of tries for delete processing")

;;; ----------   Removing ---------------------------
;;; POD 2007-04-27 guessing....
(defun trie-remove (key) ; something more will be necessary for removing clause index ? see trie-delete below
  "Toplevel function to delete a term." 
  (when-bind (root (gethash (predsym key) (roots *trie-db*)))
    (setf *trie-trace* nil)
    (let* ((key (substitute-svn key))
	   (tbuf (serialize* key +termbuf+))
	   (array (termbuf-arr tbuf)))
      (declare (type termbuf tbuf))
      (when-bind (trie (find-trie-tracing tbuf root)) ; else no such thing stored.
	(unless (equal key (trie-value trie)) (error "tries inconsistent.")) ; pod useless test?
	(pop *trie-trace*) ; last one is trie with value = key. 
	(loop for i from (1- (termbuf-fill-pointer tbuf)) downto 0
	      for val = (aref array i) 
	      for trie in *trie-trace* do 
	      (trie-arcs-pop val trie)
	      when (trie-arcs trie) return nil))))) ; trie-arcs non-nil means other uses.

#-sbcl(declaim (inline follow-arc-adding))
#+sbcl(declaim (notinline follow-arc-adding))
(defun follow-arc-adding (component tr)
  "Find the trie node for this component of the key. Make a new node if needed."
  (declare #|(type trie tr) (type (or symbol string number k:skolem-fn) component)|#)
  (or
    (trie-arcs-lookup component tr)
    (let ((child-trie (get-trie)))
      (trie-arcs-push child-trie component tr)
      child-trie)))

(defun find-trie-adding (tbuf root)
  "Find the trie node for this key. Make a new node if needed."
  (declare (type trie root) (type termbuf tbuf)) ; 2013-02-04 I remove comments from (type trie root).
  (declare (values trie))
  (let ((arr (termbuf-arr tbuf)))
    (loop for i from 1 to (1- (termbuf-fill-pointer tbuf)) 
          for tr = (follow-arc-adding (aref arr 1) root) then 
                   (follow-arc-adding (aref arr i) tr)
	  do (unless (typep tr 'trie) ; 2013-02-04 added
	       (error "find-trie-adding: i=~A fill-pointer=~A" i (termbuf-fill-pointer tbuf)))
          finally (return tr))))

#-sbcl(declaim (inline follow-arc-tracing))
#+sbcl(declaim (notinline follow-arc-tracing))
(defun follow-arc-tracing (component tr)
  "Find the trie node for this component of the key. Make a new node if needed."
  (let ((tr (trie-arcs-lookup component tr)))
    (push tr *trie-trace*)
    tr))

(defun find-trie-tracing (tbuf root)
  "Find the trie node for this key. Make a new node if needed."
  (declare #|(type trie root)|# (type termbuf tbuf))
  (declare (values trie))
  (let ((arr (termbuf-arr tbuf)))
    (loop for i from 1 to (1- (termbuf-fill-pointer tbuf)) 
          for tr = (follow-arc-tracing (aref arr 1) root) then 
                   (follow-arc-tracing (aref arr i) tr)
          finally (return tr))))

;;; ----------   Query ---------------------------
(defvar +trie-bindings+ nil "An assoc list of a variable and a bind-rec. 
                             Represents the binding of that variable.")

(defstruct bind-rec
  (path nil)       ;; a list of indecies (fixnum or arcs-position) that navigate from the root to the binding value.
  (tbuf-ix nil)    ;; the index in the key where this navigation starts
  (root nil))      ;; the trie where navigation starts.

(defstruct arcs-position
  "A structure that represents a value in the list of positions of a 
   bind-rec-path slot. It is used for this value only in cases 
   where the trie's arcs stores a hash table."
  (arr nil)  ;; array of keys from trie-arcs
  (ptr 0))   ;; a position in the arr array, selecting a key.

(defun new-arcs-position (arcs)
  (make-arcs-position 
   :arr (make-array (hash-table-count arcs)
                    :initial-contents
                    (loop for k being the hash-key of arcs collect k))))

;;; NOTE: This is not thread-safe. The arcs-position thing assumes the arcs 
;;; have not been update during its existence.
;;; 2013-02-04 I'm assuming the locking on with-trie-db will take care of this.
(defun nth-arc (n arcs)
  "Return the 'nth arc' (that is (key . trie)) from the assoc list or hashtable arcs.
   If arcs is a hash-table n will be a arcs-position, and return
   ((arcs-position-ptr n) . (gethash (arcs-position-ptr n))) where the car of that cons 
   is a key and the cdr is a trie -- just like lisp nth on trie-arcs."
  (declare (type (or fixnum arcs-position) n) (type (or list hash-table) arcs))
  (if (hash-table-p arcs)
    (when (< (arcs-position-ptr n) (array-total-size (arcs-position-arr n)))
      (let ((key (aref (arcs-position-arr n) (arcs-position-ptr n))))
        (cons key (gethash key arcs))))
    (nth n arcs)))

#-sbcl(declaim (inline path-incf))
(defun path-incf (n)
  "Increment the argument, which is a position in a trie navigation."
  (cond ((numberp n) (1+ n))
        (t (incf (arcs-position-ptr n)) n)))

#-sbcl(declaim (inline path-n+1-p))
(defun path-n+1-p (n arcs)
  "Return true if the path is not exhausted at position n."
  (declare (type (or fixnum arcs-position) n))
  (if (hash-table-p arcs) ; don't really care
    (> (array-total-size (arcs-position-arr n)) (1+ (arcs-position-ptr n)))
    (nth (1+ n) arcs)))

(declaim (inline qvar-p))
(defun qvar-p (x) (and (symbolp x) (get x 'qvar)))

(declaim (inline trie-clear-history))
(defun trie-clear-history ()
  (setf +trie-bindings+ nil))

#-sbcl(declaim (inline trie-query-pre))
(defun trie-query-pre (key)
  "Preprocess a trie query by setting a property on variables, 
   and substituting special-value-nil for nil."
  (declare (type list key))
  (declare (values termbuf))
  (trie-clear-history)
  (labels ((qvar-it (k)
             (cond ((symbolp k)
                     (when (and #+kif(not (get k 'kvar)) ; see kif/clausify.lisp
                                (not (get k 'qvar))
                                (char= #\? (char (symbol-name k) 0)))
                       (setf (get k 'qvar) t)))
                   ((consp k)
                     (qvar-it (car k)) (qvar-it (cdr k))))))
  (qvar-it key)
  (setf key (substitute-svn key))
  (serialize* key +termbuf+)))

(defvar *last-trie* nil "Used to hold position in search in case continuing.")

(defun trie-query (key)
  "Toplevel function: If the key identifies a trie, return it."
  (declare (type list key))
  (let* ((pred (predsym key))
         (tr (gethash pred (roots *trie-db*)))
         (tbuf (trie-query-pre key)))
    (declare (type (or trie null) tr) (type termbuf tbuf))
    (when tr
      (when-bind (found (catch 'query-done (find-trie tbuf 1 tr)))
                 (setf *last-trie* found)
                 (subst nil 'special-value-nil (trie-value found))))))

;;; POD Messy. Use of +termbuf+
(defun trie-query-next ()
  "Get the next trie. Use it after calling trie-query or itself."
  (when-bind (found (catch 'query-done (find-trie +termbuf+ 1 *last-trie*)))
             (setf *last-trie* found)
             (subst nil 'special-value-nil (trie-value found))))

(defun trie-query-all (key)
  "Toplevel function: return a list of all values found."
  (declare (type list key))
  (let* ((pred (predsym key))
         (root (gethash pred (roots *trie-db*)))
         (tbuf (trie-query-pre key)))
    ;; By starting at iptr = 1 you skip the outer :o (outer :c was not added)
    ;; so a termbuf-arr for (a ?x) looks like #(:o a ?x t t t ...)
    (loop for tr = (catch 'query-done (find-trie tbuf 1 root))
              then (catch 'query-done 
                     (mvb (iptr trie) (trie-pop-state (caar +trie-bindings+))
                       (find-trie tbuf iptr trie)))
              while tr collect (subst nil 'special-value-nil (trie-value tr)))))

(declaim (inline trie-var-bound-p))
(defun trie-var-bound-p (key)
  "Return true if the variable is bound."
  (assoc key +trie-bindings+))

(defun balance-parens (root path)
  "The argument trie opened count parentheses at index ix. Navigate chosing
   the first arc of every node until parentheses are balanced."
  (flet ((ix-first-arc (tr)
           (if (listp (trie-arcs tr)) 0 (new-arcs-position (trie-arcs tr)))))
    (mvb (followed-to count) (follow-path root path)
      (let ((path-adds
              (loop until (zerop count)
                 for ix = (ix-first-arc followed-to) then (ix-first-arc tr)
                 for (val . tr) = (nth-arc ix (trie-arcs followed-to)) then (nth-arc ix (trie-arcs tr))
                 if (eql val 'open-paren) do (incf count)
                 else if (eql val 'close-paren) do (decf count)
                 collect ix into result
                 finally (return result))))
        (append path path-adds)))))

#-sbcl(declaim (inline trie-push-state))
#+sbcl(declaim (notinline trie-push-state))
(defun trie-push-state (var iptr root)
  "Save the state consisting of a variable (pointed at by tbuf-ix), the trie
   where the program is about to choose to bind the variable, and a path from
   that point that balances the parentheses."
  (declare (type symbol var) (type trie root))
  (declare (values))
  (when-bind (arcs (trie-arcs root)) ; otherwise this root is terminal, will have to backtrack.
    (let ((first-step (if (hash-table-p arcs) (new-arcs-position arcs) 0)))
      (push (cons var (make-bind-rec  
                       :path (balance-parens root (list first-step))
                       :tbuf-ix iptr
                       :root root))
            +trie-bindings+))))

(defun follow-path (root path)
  "Return (values <destination trie> <open paren count>) that is 
   the result of following the path."
  (if (null path) 
    (values root 0)
    (let ((open-count 0))
      (loop for step in path
         for (val . tr) = (nth-arc step (trie-arcs root)) 
         then (nth-arc step (trie-arcs tr))
         if (eql val 'open-paren) do (incf open-count)
         else if (eql val 'close-paren) do (decf open-count)
         finally (return (values tr open-count))))))

#-sbcl(declaim (inline navigate-next))
#+sbcl(declaim (notinline navigate-next))
(defun navigate-next (root path)
  "Return the index (list of positions) to the next value in the arcs of the argument trie, or nil."
  (when-bind (newpath
              (loop for short-path on path by #'butlast ; progressively shorten path
                 for old-last = (pod:last1 short-path)
                 for path-end = (follow-path root (butlast short-path))
                 when (path-n+1-p old-last (trie-arcs path-end))
                 return (append (butlast short-path) (list (path-incf old-last)))))
    (balance-parens root newpath)))
            
;;; The current binding for the variable failed. (current = top of stack)
;;; If there are more values to try for variable, pop the failed one off b-rec-vals and continue,
;;; otherwise, pop the whole variable and continue.    inline new 2005-03-12
#-sbcl(declaim (inline trie-pop-state))
#+sbcl(declaim (notinline trie-pop-state))
(defun trie-pop-state (var) 
  (let (nav)
    (cond ((null +trie-bindings+) (throw 'query-done nil)) ; failure (the only exit in code)
          ((and (null var)
                (when-bind (br (cdar +trie-bindings+))
                  (setf nav (navigate-next (bind-rec-root br) (bind-rec-path br))))) ; values remain
	   (when-bind (br (cdar +trie-bindings+)) ; 2013-02-04 was a let -- actual problem is probably concurrency!
              (setf (bind-rec-path br) nav)
              (values (bind-rec-tbuf-ix br) 
                      (bind-rec-root br))))
          (var ; pop all the way back to argument variable
            (loop for binding-rest on +trie-bindings+
                  when (eql var (car binding-rest))
                  return (setf +trie-bindings+ binding-rest))
            (trie-pop-state nil))
          (t ; All bindings of top variable failed...
            (pop +trie-bindings+) 
            (trie-pop-state nil)))))

;;; POD This could probably be cleaned up to use follow path
#-sbcl(declaim (inline bind-val))
#+sbcl(declaim (notinline bind-val))
(defun bind-val (var)
  "Return the value currently bound to the argument variable.
   If the variable is bound to a list, (indicated by the first value being :o, return the 
   whole :o blah blah :c thing."
  (when-bind (br (cdr (assoc var +trie-bindings+))) ; 2013-02-04 was part of let*
    (let* ((root (bind-rec-root br))
	   (path  (bind-rec-path br))
	   (result
	    (loop with count = 0
	       for ix in path
	       for (key . tr) = (nth-arc ix (trie-arcs root)) then (nth-arc ix (trie-arcs tr))
	       collect key
	       if (eql key 'open-paren) do (incf count)
	       else if (eql key 'close-paren) do (decf count)
	       until (zerop count))))
      (if (eql (first result) 'open-paren)
	  result ; it is bound to a list
	  (pod:last1 result))))) ; it is bound to an atom.

#+sbcl
(defun diag-current-bindings ()
  (loop for bind in +trie-bindings+ do
        (format t "~%Bound ~S to ~S"
                (first bind) (bind-val (first bind)))))

(defun find-trie (tbuf iptr trie)
  "Find the trie node for this key."
  (declare (type termbuf tbuf) (type integer iptr) (type (or trie null) trie))
  (unless trie (throw 'query-done nil))
  (loop 
     (let ((key (aref (termbuf-arr tbuf) iptr)))
       (cond ((= iptr (termbuf-fill-pointer tbuf)) ; reached end of terms and key.
               (if (trie-value trie) ; since this is nil unless fully closed, good test.
                 (throw 'query-done trie)
                 (mvb (niptr ntrie) (trie-pop-state nil) ; backtrack (variable arity?)
                  ;(break "backtracking") (format t "~% backtracking") 
                   (setf iptr niptr trie ntrie))))
             ((not (qvar-p key))
               (if-bind (answer (trie-arcs-lookup key trie))
                        (setf trie answer iptr (1+ iptr))
                        (mvb (niptr ntrie) (trie-pop-state nil) ; Backtrack to new value for last key.
                          (setf iptr niptr trie ntrie))))
             ((not (trie-var-bound-p key))
               (cond ((trie-push-state key iptr trie) 
		      #|(diag-current-bindings)|#)
                     (t
                       (mvb (niptr ntrie) (trie-pop-state key) ; ...backtrack to new value of that key.
   		       ;;(format t "~%backtracking iptr = ~A" iptr) (break "backtracking") 
			 (setf iptr niptr trie ntrie)))))
             ((when-bind (found (trie-arcs-lookup (bind-val key) trie)) ;It is a var key. Check one val.
                (incf iptr) (setf trie found)))
             (t ; It is a var key (usage) , but val check (above) failed...
               (mvb (niptr ntrie) (trie-pop-state key) ; ...backtrack to new value of that key.
               ;;(format t "~%backtracking iptr = ~A" iptr) (break "backtracking") 
                 (setf iptr niptr trie ntrie)))))))


;;; -------  deleting ------------------------------------
#|
(defun trie-delete (term list clause-ix) 
  "Toplevel function to delete a term (or at least one clause referencing it)."
  (declare (type list term) (type symbol list))
  (let* ((pred (predsym term))
         (root (ecase list (:passive (get pred 'passive)) (:active (get pred 'active))))
         (trie (find-trie term term root)))
    (if clause-ix
      (delete clause-ix (trie-clauses trie))
      (setf (trie-clauses trie) nil))))
|#

	  
