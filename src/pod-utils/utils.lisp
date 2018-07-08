
;;; Copyright (c) 2004 Peter Denno
;;;
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

;;;
;;; Peter Denno
;;;  Date: 12/2/95 - on going.
;;;
;;; Generally applicable utilities. Some from Norvig's "Paradigms of
;;; Artificial Programming," Some from Kiczales et. al. "The Art of the
;;; Metaobject Protocol," some from Graham's "On Lisp," some from Sam Steingold.
;;;
(in-package :pod-utils)

(defvar *null-stream* (make-two-way-stream (make-concatenated-stream) (make-broadcast-stream)))

;;; Purpose: Return the combinations possible when selecting one item
;;;          from each of the argument sets.
;;;         Example: (combinations '(a) '(b c) '(d e))
;;;                   => ((A B D) (A B E) (A C D) (A C E))
;;; Arg: sets - lists
;;; Value: a list of lists. If the argument is nil, it returns nil.
(defun combinations (&rest sets)
  (cond ((null sets) nil)
	(t 
	 (flet ((combinations-aux (aset bset)
		  (cond ((not aset) bset)
			((not bset) aset)
			(t (loop for a in aset
				 append (loop for b in bset
					      collect (list a b)))))))
	   (loop for set in (reduce #'combinations-aux sets)
		 collect (flatten set))))))

(defun flatten (input &optional accumulator)
  "Return a flat list of the atoms in the input.
   Ex: (flatten '((a (b (c) d))) => (a b c d))"
  (cond ((null input) accumulator)
	((atom input) (cons input accumulator))
	(t (flatten (first input)
		    (flatten (rest input) accumulator)))))

(declaim (inline kintern))
(defun kintern (string &rest args)
  "Apply FORMAT to STRING and ARGS, upcase the resulting string and
 intern it into the KEYWORD package."
  (intern (string-upcase (apply #'format nil (string string) args))
	  (find-package "KEYWORD")))

#| POD lighter weight, more likely 
(declaim (inline kintern))
(defun kintern (string)
  "Apply FORMAT to STRING and ARGS, upcase the resulting string and
 intern it into the KEYWORD package."
  (intern (string-upcase (string string)) (find-package "KEYWORD")))
|#

(declaim (inline sintern))
(defun sintern (string &rest args)
  "Apply FORMAT to STRING and ARGS, upcase the resulting string and
 intern it into the current (*PACKAGE*) package."
  (intern (string-upcase (apply #'format nil (string string) args))))

(defun mapappend (fun &rest args)
  "Append results of mapping."
  (loop until (some #'null args)
	append (apply fun (loop for largs on args
				collect (pop (first largs))))))

(defun mapnconc (fun &rest args)
  "Nconc results of mapping."
  (loop until (some #'null args)
	nconc (apply fun (loop for largs on args
				collect (pop (first largs))))))

;;; Purpose: Return a list of pairs of elements from the argument list:
;;; Ex: (pairs '(a b c d)) => ((a b) (a c) (a d) (b c) (b d) (c d))
;;;
;;; Args: inlist - a list
(defun pairs (inlist)
  (loop for sublist on inlist
	while (cdr sublist)
	append
	(loop for elem in (cdr sublist)
	      collect `(,(first sublist) ,elem))))

;;; Purpose: Called by memoize, below. This returns
;;;          the memoized function. Norvig, Page 270.
;;; When you want to use this on &rest args use :test #'equal :key #'identity
;;; When you want to use it on a function that has more than one argument use :key #'unique
;;; Args: fn - the function object.
;;;       name - the function symbol.
;;;       key - On what argument the result is indexed.
;;;       test - Either eql or equal, the :test of the hash table.
(defun memo (fn name key test)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
	(let ((k (funcall key args)))
	  (multiple-value-bind (val found-p)
	      (gethash k table)
	    (if found-p
		val
	      (setf (gethash k table) (apply fn args))))))))


(defun debug-memo (fn name key test)
  "Like memo but prints *hit* on every hit."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
	(let ((k (funcall key args)))
	  (multiple-value-bind (val found-p)
	      (gethash k table)
	    (if found-p
		(progn (princ " *HIT*") val)
	      (progn
		(princ " *miss*")
		(setf (gethash k table) (apply fn args)))))))))

;;; Purpose: memoize the argument function.
;;; Arguments as those in memo.
(defun memoize (fn-name &key (key #'first) (test #'eql) (debug nil))
  "Replace fn-name's global definition with a memoized version."
	  #-Allegro-V4.3 (format t "~%;;; Memoizing (~a) ~a ****" test fn-name)
	  #+Allegro-V4.3 (format t "~%;;; Memoizing ~a ****" fn-name)
  (if debug
      (setf (symbol-function fn-name)
	    (debug-memo (symbol-function fn-name) fn-name key test))
    (setf (symbol-function fn-name)
	  (memo (symbol-function fn-name) fn-name key test))))

;;; Clear the hash table from the function.
(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

;;; Purpose: define a function and memoize it.
;;; Limitations: only useful for default arguments, i.e.,
;;;              key on first and test eql. In all other
;;;              cases call (memoize <fn> :key <key> :test <test>).
(defmacro defun-memoize (fn args &body body)
  `(memoize (defun ,fn ,args ,body)))

;;; Stuff to use when you have a serious number of memoized functions,
;;; and you have a notion of "starting over." 
;;; POD Limitation: Doesn't use the keys of memoize!
(defmacro defmemo (fname &body body)
  `(progn (defun ,fname ,@body)
     (eval-when (:load-toplevel)
       (memoize ',fname)
       (system-add-memoized-fn ',fname))))

;;; I most use this one with :key #'unique :test #'eq
;;; There is not point in a separate 'tool chain' for unique-ified lists
;;; because memo defines a function (lambda (&rest args) ...)
(defmacro defmemo! (fname (&key (key #'first) (test #'eql) debug) &body body)
  `(progn (defun ,fname ,@body)
     (eval-when (:load-toplevel)
       (memoize ',fname :key ,key :test ,test :debug ,debug)
       (system-add-memoized-fn ',fname))))

;;; Of course specifying :test equal should have been possible with defmemo,
;;; but it is a little late now. 
;;; "Somewhat deprecated." Use defmemo (:key #'unique :test #'eq) except where
;;; the argument is a string. 
(defmacro defmemo-equal (fname &body body)
  `(progn (defun ,fname ,@body)
     (eval-when (:load-toplevel)
       (memoize ',fname :test #'equal)
       (system-add-memoized-fn ',fname))))

(let ((+memoized-fns+ nil))
  (defun system-clear-memoized-fns (&key suppress-warning)
    (mapcar #'(lambda (x) 
                (unless suppress-warning (warn "Clearing memoized ~A" x))
                (clear-memoize x))
            +memoized-fns+))
  (defun system-add-memoized-fn (fname)
    (pushnew fname +memoized-fns+))
  (defun system-list-memoized-fns ()
    +memoized-fns+)
  (defun system-forget-memoized-fns ()
    (setf +memoized-fns+ nil))
)

;;; Purpose: Diagnostic (From Howard Stearns) that does
;;; (vars a b c) => (FORMAT *TRACE-OUTPUT* "~&a = ~S b = ~S c = ~S ~%" A B C)
(defmacro VARS (&rest variables)
  `(format *trace-output*
           ,(loop with result = "~&"
                  for var in variables
                  do
                  (setf result
                        (if (and (consp var)
                                 (eq (first var) 'quote))
                            (concatenate 'string result " ~S ")
                          (concatenate 'string result (string-downcase var) " = ~S ")))
                  finally (return (concatenate 'string result "" #|"~%"|#)))
           ,@variables))

;;; The most essential macro building tool.
(defmacro mac (macro)
  `(pprint (macroexpand-1 ',macro)))

;;; Similar, but used on 'subtype' macros. 
(defmacro mac2 (macro)
  `(pprint (macroexpand-1 (macroexpand-1 ',macro))))

;;; Dirk H.P. Gerrits' "Lisp Code Walker" slides, ALU Meeting, Amsterdam, 2003. 
;;; With additional corrections (beyond that in his notes). 
(defvar *mea-hooks* (make-hash-table :test #'eq))
(defun macroexpand-all (form &optional env)
  "Macroexpand FORM recursively until none of its subforms can be further expanded."
  (multiple-value-bind (expansion macrop)
      (macroexpand-1 form env)
    (declare (ignore macrop))
    (let* ((key (and (consp form) (car form)))
           (hook (gethash key *mea-hooks*)))
      (cond (hook (funcall hook form env))
            ((and (consp form) (symbolp (car form)) (macro-function (car form)))
             (macroexpand-all expansion env))
            ((consp form) (cons (car form)
                                (mapcar #'(lambda (arg)
                                            (macroexpand-all arg env))
                                        (cdr form))))
            (t expansion)))))

(defun load-ht (ht key-value-pairs)
  "Load the argument hash table with the argument values
   provided in a flat list of <key> <value>. "
  (loop while key-value-pairs
	do
	(setf (gethash (pop key-value-pairs) ht)
	      (pop key-value-pairs)))
  ht)
  
(defmacro when-bind ((var expr) &body body)
  "Paul Graham 'On LISP' pg 145. when+let"
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

;;; Anaphoric macros from Paul Graham ON LISP, page 191.
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(aif ,test (progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

;;; Better to call it 'bif' like KT. 
(defmacro if-bind ((var expr) then else)
  `(let ((,var ,expr))
     (if ,var
         ,then 
       ,else)))

(defmacro when-bind* (binds &body body)
  "Paul Graham ON LISP pg 145. when+let*"
  (if (null binds)
      `(progn ,@body)
    `(let (,(car binds))
       (if ,(caar binds)
	   (when-bind* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  "Paul Graham ON LISP pg 145. Used in macros to avoid variable capture."
  `(let ,(mapcar #'(lambda (s) 
		     `(,s (gensym)))
	  syms)
     ,@body))

(declaim (inline substring))
(defun substring (str1 str2)
  "Returns the place in str1 where str2 begins or nil if str2 is not in str1"
  (search str2 str1 :test #'string=))

(defun remove-extra-spaces (string)
  "Leave only one space between non-space characters of argument string."
  (if (string= string "") 
      ""
      (let* ((len (length string))
	     (new-string (make-array len :element-type 'character :fill-pointer 0)))
	(vector-push (char string 0) new-string)
	(loop for i from 1 to (1- len)
	   unless (and (char= #\Space (char string i))
		       (char= #\Space (char string (1- i))))
	   do (vector-push (char string i) new-string))
	new-string)))

(defun break-line-at (string break-bag position)
  "Return the argument STRING with linefeeds inserted at some position past POSITION
   where a character in the break-bag is encountered."
  (let* ((len (length string))
         (new-string (make-array (* 2 len) :element-type 'character :fill-pointer 0)))
    (loop for ix from 0 to (1- (length string))
          with count = 0
          do (vector-push (char string ix) new-string)
          (incf count)
          when (and (> count position)
                    (find (char string ix) break-bag))
          do (vector-push #\Linefeed new-string)
          (setf count 0)
          finally (return new-string))))

(defun read-string-to-list (string)
  (loop with val = nil and start = 0
	do (multiple-value-setq (val start)
	     (read-from-string string nil :eof :start start))
	until (eql val :eof)
	collect val))

;;; (cl-ppcre:split "\\s+" "foo   bar baz frob")
;;; ("foo" "bar" "baz" "frob") 
;;; http://weitz.de/cl-ppcre/#split .... but a bit slow if you don't need a pattern. CHECK THIS!
(defun split (string c &key min-size end)
  "Like the perl split, split the string using the character. Return
   a list of substrings."
  (let ((result
         (loop for i from 0 to (1- (length string))
	       for cnt from 0
               with start = 0 with size = 0
	       do (incf size)
               when (and (char= c (char string i))
			 (or (not min-size)
			     (> size min-size)))
               collect (subseq string start i) into result
               and do (setf start (1+ i) size 0)
	       when (and end (> cnt end)) return result
               finally (return (append result (list (subseq string start)))))))
    (if (zerop (length (first (last result))))
        (butlast result)
	 result)))

;;;(defun tryme () (loop for i from 1 to 1000000 do (cl-ppcre:split "x" "axbxcxdx")))
;;;(defun tryme1 () (loop for i from 1 to 1000000 do (split "axbxcxdx" #\x)))
;;;(defun tryme1 () (loop for i from 1 to 1000000 do (split2 "axbxcxdx" #\x)))

#| Jonathan's. Mine is faster, except when you want a vector. 
(defun split2 (a-string split-character)
  (let ((substrings (make-array 64 :adjustable t :fill-pointer 0)) (last-index 0))
    (dotimes (n (length a-string))
      (when (char= (char a-string n) split-character)
	(vector-push-extend (subseq a-string last-index n) substrings)
	(setf last-index (+ n 1))))
    (vector-push-extend (subseq a-string last-index (length a-string)) substrings)
	substrings))
|#

(defun name2initials (string)
  "For 'abc' return 'a'. For 'product_definition_formation' return 'pdf', etc."
  (unless (stringp string) (setf string (string string)))
  (let ((result (make-array 31 :element-type 'character :fill-pointer 0))
        (len (length string)))
    (vector-push (char string 0) result)
    (loop for i from 1 to (1- len)
          when (and (char= (char string i) #\_) (< i (1- len))) do
          (vector-push (char string (+ i 1)) result)
          (incf i))
    result))

;;; POD What do you want for WriteIThis ? I'm getting "Write IT This"
#+nil ; not yet used. 
(defun camel2spaced (str)
  "WriteThis like this: 'Write This'"
  (with-output-to-string (s)
    (loop with len = (length str)
	  with spaced = nil
	  for i from 0 to (1- len)
	  for c1 = (aref str i)
	  for c2 = (unless (= i (1- len)) (aref str (1+ i))) 
	  for up1-p = (upper-case-p c1)
	  for up2-p = (and c2 (upper-case-p c2)) do 
	  (cond ((or spaced
		     (zerop i) 
		     (not c2))
		 (write-char c1 s) (setf spaced nil))
		((or (and up1-p (not up2-p))
		     (and up2-p (not up1-p))) ;helloWo
		 (write-char c1 s) (write-char #\Space s) (setf spaced t))
		(t (write-char c1 s))))))


;;; POD should compare the efficiency of these array-based implementations with
;;; that of string-stream ones.
(defun c-name2lisp (c-string &key (dash #\-))
  "aNameLikeThis --> a-name-like-this"
  (setf c-string (string c-string)) ; 2008-01-23
  (let* ((len (length c-string))
         (result (make-array (* 2 len) :element-type 'character :fill-pointer 0)))
    (vector-push (char c-string 0) result)
    (loop for i from 1 to (1- len)
          for char = (char c-string i) do		  
          (when (upper-case-p char) (vector-push dash result))
          (vector-push char result))
    (string-downcase result)))

(defun string-squeeze (str char-bag)
  "Return a string based on STR but with all chars for CHAR-BAR removed."
  (let* ((len (length str))
         (result (make-array len :element-type 'character :fill-pointer 0)))
    (loop for i from 0 to (1- len)
	  for c = (char str i)
	  unless (member c char-bag :test #'char=)
	  do (vector-push c result))
    result))

;;; POD make-array where should use string-stream
(defun lisp-name2c (in-string &aux (lisp-string (string-downcase in-string)))
  "a-name-like-this --> aNameLikeThis"
  (let* ((len (length lisp-string))
	 (result (make-array len :element-type 'character :fill-pointer 0)))
    (vector-push (char lisp-string 0) result)
    (loop for i from 1 to (1- len)
	  for char = (char lisp-string i)
          with upper-next = nil do		  
	  (cond ((or (char= char #\-) (char= char #\_))
                 (setf upper-next t))
                (t (vector-push (if upper-next (char-upcase char) char) result)
                   (setf upper-next nil))))
    result))

(defun dash-to-camel (str &key (separator #\_) (capitalize-first t))
  "change_this to ChangeThis."
  (with-output-to-string (stream)
    (let ((len (length str)))
      (unless (char= (char str 0) separator) 
	(if capitalize-first
	    (write-char (char-upcase (char str 0)) stream)
	    (write-char (char str 0) stream)))
      (loop for i from 1 to (1- len)
	    for c = (char str i) do
	    (unless (char= c separator)
	      (if (char= (char str (1- i)) separator)
		  (write-char (char-upcase c) stream)
		  (write-char c stream)))))))
	    

;;;=============================================
;;; A bunch more from Paul Grahams's "On Lisp."
;;;=============================================
(declaim (inline single-p last1 mklist))

(defun single-p (lst)
  "List contains just one thing."
  (and (consp lst) (not (cdr lst))))

(defun last1 (lst)
  (car (last lst)))

(defun mklist (obj)
  "Make the argument a list if it isn't already."
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  "Return true if x longer than y -- only for lists."
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
      (> (length x) (length y)))))

(defun string-integer-p (str)
  "Returns the integer if the string is one, otherwise nil."
  (when (stringp str)
    (let ((c1 (char str 0)))
      (and (or (digit-char-p c1) 
	       (char= #\+ c1)
	       (char= #\- c1))
	   (every #'digit-char-p (subseq str 1))
	   (read-from-string str)))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                 (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun prune (test tree)
  " (prune #'oddp '(1 2 (3 4) (5 6 (7 8 (9 10 (11)) 12) 13))) ==> (2 (4) (6 (8 (10 NIL) 12)))"
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc 
                             (cons (car tree) acc)))))))
    (rec tree nil)))

(defun find2 (fn lst)
  "Like find but returns value from function too."
  (if (null lst)
      nil
    (let ((val (funcall fn (car lst))))
      (if val 
          (values (car lst) val)
        (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  "Returns like member when x before y."
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  "Returns like member when x after y. x must be in y."
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  "Returns like member when a second copy of obj is in lst.
   > (duplicate 'a '(a b c a d) ==> (A D)"
  (member obj (cdr (member obj lst :test test)) :test test))

(defun split-if (fn lst)
  "Splits a list into two where fn returns true.
   (split-if #'(lambda (x) (> x 4)) '(1 2 3 4 5 6 7) ==> (1 2 3 4) (5 6 7)"
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

;;; Why waste your life typing?
(defmacro mvb (vars form &body body)
  `(multiple-value-bind ,vars ,form ,@body))

(defmacro mvs (vars form &body body)
  `(multiple-value-setq ,vars ,form ,@body))

(defmacro dbind (vars form &body body)
  `(destructuring-bind ,vars ,form ,@body))

;;; Returns multiple values.
;;; seconds minutes hours days
(defun decode-time-interval (time)
  (multiple-value-bind (days tm1) (floor time 86400) ; (* 60 60 24)
    (multiple-value-bind (hrs tm2) (floor tm1 3600) ; (* 60 60)
      (multiple-value-bind (min sec) (floor tm2 60)
	(values sec min hrs days)))))

(defmacro strcat (&rest strings)
  "Because the body is too long to type!"
  `(concatenate 'string ,@strings))

(defmacro strcat* (place &rest strings)
  "Destructive concatenate, sets PLACE to concatenation of PLACE and STRINGS."
  `(setf ,place (concatenate 'string ,place ,@strings)))

#+nil
(defun strcat+ (&rest strings)
  "Function version of macro strcat. #'concatenate string) "
  (let ((result ""))
    (reduce #'(lambda (&optional x y)
		(if (null x) "" (setf result (concatenate 'string x y))))
	    strings :initial-value "")
    result))

;(declaim (inline strcat))
;(defun strcat (&rest strings)
;  (apply #'concatenate 'string strings))


;;; "...Thus the most general form of ~D is ~mincol,padchar,commachar,comma-intervalD."
;;; Note also that padchar "must be a character" --> put a ' before it. 
(defun now (&key (form :iso-8601) pretty-p)
  (multiple-value-bind (s m h d month y) (decode-universal-time (get-universal-time))
    (case form
      (:iso-8601 (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" y month d h m s))
      (:mdy
       (if pretty-p
	   (format nil "~A ~A, ~A"
		   (nth (1- month) '("January" "February" "March" "April" "May" "June"
				 "July" "August" "September" "October" "November" "December"))
		   d y)
	 (format nil "~D.~2,'0D.~2,'0D ~2,'0D:~2,'0D:~2,'0D" month d y h m s)))
      (:yyyymmddhhmmss
	 (format nil "~D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D" y month d h m s))
      (:yyyymmddhhmmss-colon
	 (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" y month d h m s))
      (:yyyymmdd-colon-hhmm
	 (format nil "~D~2,'0D~2,'0D:~2,'0D~2,'0D" y month d h m))
      (:yymmdd
	 (format nil "~2,'0D~2,'0D~2,'0D" (- y 2000) month d))
      (:yyyymmdd
	 (format nil "~D~2,'0D~2,'0D"  y month d))
      (:hhmm (format nil "~2,'0D~2,'0D" h m)))))

			
; Norvig's search routines, with do-fn
(defun tree-search (states goal-p successors combiner do-fn)
  "Find a state that satisfies goal-p. Start with STATES, 
   and search according to SUCCESSORS and COMBINERS."
  (cond ((null states) :fail)
        ((funcall goal-p (first states))
	 (when do-fn (funcall do-fn (first states)))
	 (first states))
        (t 
	 (when do-fn (funcall do-fn (first states)))
	 (tree-search
	  (funcall combiner
		   (funcall successors (first states))
		   (rest states))
	  goal-p successors combiner do-fn))))


(let (+search-path+)
  (defun set-search-path (val) (setf +search-path+ val))
  (defun tree-search-path () (mapcar #'car +search-path+))
  (defun backout ()
    (loop while (and +search-path+ (null (first +search-path+))) do
	  (when (and (single-p +search-path+) (null (first +search-path+)))
	    (setf +search-path+ nil)
	    (return-from backout :fail))
	  (pop +search-path+)
	  (setf (first +search-path+) (cdr (first +search-path+)))))
  (defun depth-search-tracking (goal-p successors &optional (do #'identity))
    "Search depth-first, return path when successful."
    ;;(VARS +search-path+)
    (cond ((eql (backout) :fail) (return-from depth-search-tracking :fail))
	  ((funcall goal-p (caar +search-path+))
	   (when do (funcall do (caar +search-path+)))
	   (mapcar #'car +search-path+))
	  (t 
	   (when do (funcall do (caar +search-path+)))
	   (let ((children 
		  (when successors ; 2011-05-10 should be pointless.
		    (funcall successors (caar +search-path+)))))
	     (if children
		 (push children +search-path+)
	       (setf (first +search-path+) (cdr (first +search-path+))))
	     (depth-search-tracking goal-p successors do)))))
)

#| Don't remove these; they substitute for documentation!
(defclass node ()
  ((name :reader name :initarg :name)
   (children :reader children :initarg :children)))

(defmethod print-object ((obj node) stream)
  (format stream "[node ~A]" (name obj)))

; a b e f h
(defun tryme (goal)
  (flet ((mn (n &rest c) (make-instance 'node :name n :children c)))
    (let ((a
	   (mn 'a
	       (mn 'b
		   (mn 'd)
		   (mn 'e 
		       (mn 'f 
			   (mn 'h))
		       (mn 'g)))
	       (mn 'c))))
  (depth-first-search a #'(lambda (x) (eql (name x) goal)) #'children 
		      :tracking t
		      :start-state (list (list a))))))
|#
#| Don't remove these either!
(defclass node ()
  ((name :reader name :initarg :name)
   (children :reader children :initarg :children)))

(defmethod print-object ((obj node) stream)
  (format stream "[node ~A]" (name obj)))

; a b e f h
(defun tryme (goal)
  (flet ((mn (n &rest c) (make-instance 'node :name n :children c)))
    (let ((a
	   (mn 'a
	       (mn 'b
		   (mn 'd)
		   (mn 'a)
		   (mn 'e 
		       (mn 'f 
			   (mn 'h))
		       (mn 'g)))
	       (mn 'c))))
  (depth-first-search a 
		      #'(lambda (x) (declare (ignore x)) (gather-duplicates (tree-search-path) :key #'name))
		      #'children 
		      :tracking t
		      :start-state (list (list a))))))
|#

(defun depth-first-search (start goal-p successors 
			   &key do tracking start-state (on-fail :fail))
  "Search new states first until goal returns T."
  (let ((result
	 (if tracking
	     (progn
	       (set-search-path (or start-state (list (list start))))
	       (depth-search-tracking goal-p successors do))
	     (tree-search (or start-state (list start)) goal-p successors #'append do))))
    (if (eql result :fail) on-fail result)))


(defun prepend (x y) "Prepend y to start of x" (append y x))

(defun breadth-first-search (start goal-p successors &key do (on-fail :fail))
  "Search old states first until goal is reached."
  (let ((result (tree-search (list start) goal-p successors #'prepend do)))
    (if (eql result :fail) on-fail result)))

(defun fail (arg)
  "Useful in tree searches, when you want to navigate the entire tree."
  (declare (ignore arg))
  nil)

(defmacro update (place object &key (key '#'identity) (test '#'eql))
  "Argument object may have same key as another in the list. If so, replace it with the argument.
   If not, push it. Only replaces first found. (Keys are assumed to be unique)."
  (with-gensyms (obj pos)
    `(let ((,obj ,object))
       (if-bind (,pos (position (funcall ,key ,obj) ,place :key ,key :test ,test))
                (setf ,place (substitute ,obj (nth ,pos ,place) ,place))
                (push ,obj ,place)))))

;;; The body should be a mp:process-run-function.
(defmacro with-stack-size ((size) &body body)
  `(let (#+Lispworks(sys:*sg-default-size* ,size) 
         #-Lispworks())
     ,@body))

;;;   "If in the body you really want double quotes, use escape (e.g. "\"abc\"")
(defmacro pprint-without-strings (&body body)
  `(unwind-protect 
       (progn 
         (set-pprint-dispatch 'string #'(lambda (s x) (let ((*print-pretty* nil)) (format s "~a" x))))
         ,@body)
    (set-pprint-dispatch 'string nil)))

(defmacro pprint-symbols (&body body)
  `(unwind-protect 
       (progn 
         (set-pprint-dispatch 'symbol #'(lambda (s x) (let ((*print-pretty* nil)) (format s "~a" (symbol-name x)))))
         ,@body)
    (set-pprint-dispatch 'symbol nil)))

(defun chop (str)
  "Like perl."
  (if (zerop (length str))
      ""
    (subseq str 0 (1- (length str)))))

(defmacro setx (var val)
  "For use at toplevel to avoid annoying stuff from sbcl and cmucl."
  `(defparameter ,var ,val "A variable set at the top level."))

;;; Sam Steingold
(defun pophash (object ht)
  "Remove the value and return it>"
  (multiple-value-bind (value present-p) (gethash object ht)
    (when present-p (remhash object ht))
    (values value present-p)))

;;; Sam Steingold
(defmacro ensure-gethash (object ht default)
  "Just like GETHASH with the default argument, but DEFAULT is only 
   evaluated when OBJECT is not found and in that case the value of 
   DEFAULT is placed into (GETHASH OBJECT HT)."
  (with-gensyms (obj tab)
   `(let ((,obj ,object) (,tab ,ht))
      (or (gethash ,obj ,tab)
	  (setf (gethash ,obj ,tab) ,default)))))

;;; Sam Steingold
(defmacro map-in (fn seq &rest seqs)
  "`map-into' the first sequence, evaluating it once.
  (map-in F S) == (map-into S F S)"
  (with-gensyms (mi)
    `(let ((,mi ,seq)) (map-into ,mi ,fn ,mi ,@seqs))))

;;; Peter Norvig
(declaim (inline reuse-cons))
(defun reuse-cons (x y x-y)
  "Return (cons x y), or just x-y if it is equal to (cons x y)."
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
    (cons x y)))

;;; SBCL manual, to avoid error when evaluated multiply.
;;; (It is an error if evaluated multiply and the old and new values are not eql).
;;; POD : So...not sure this is really valid. Compiler is permitted to use EQL to compare
;;; Nyef: Use for 'symbols and fixnums in an immediate-fixnum system'
;;;  "The consequences are undefined if there are any bindings of the variable named 
;;;   by name at the time defconstant is executed or if the value is not eql to the 
;;;   value of initial-value" ... consider using a make-load-form.
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
    ,@(when doc (list doc))))

;;; Resource lists -- An array that is fast in SBCL (:fill-pointer nil :adjustable nil)
;;; that can be stocked with some element and automatically restocked when that element
;;; is depleted. It is also an 'adjustable' array of sorts. 
;;; NOTE: :adjustable-p t arrays are adjustable-array-p = t. This only means that 
;;; adjust-array might return an array identical to the argument. The returned array
;;; need not be adjustable.
(defstruct reslist
  (fillptr 0 :type integer)
  (resize 2.0 :type (or single-float integer))
  (stock-fn nil :type (or null function))
  (arr nil :type simple-vector))

(defun new-reslist (size &key (element-type t) (resize 2.0) stock-fn)
  "Make a resource list of SIZE. Initialize with elements provided by STOCK-FN, 
   if provided."
  (declare (values reslist))
  (let ((reslist (make-reslist
                 :arr (make-array size :element-type element-type)
                 :resize resize :stock-fn stock-fn))) ; pod compile it???
    (when stock-fn
      (let ((arr (reslist-arr reslist)))
        (declare (type simple-vector arr))
        (loop for i from 0 to (1- size) do (setf (aref arr i) (funcall stock-fn))))
      (setf (reslist-fillptr reslist) size))
    reslist))

(declaim (inline reslist-push))
(defun reslist-push (val reslist)
  "Push an item VAL onto RESLIST adjusting array if necessary."
  (declare (type reslist reslist))
  (declare (values integer))
  (let* ((arr (reslist-arr reslist))
         (size (array-total-size arr))
         (ptr (reslist-fillptr reslist)))
    (declare (type integer ptr size) (type simple-vector arr))
    (when (= ptr size)
      (let ((resize (reslist-resize reslist)))
        (declare (type (or single-float integer) resize))
        (setf (reslist-arr reslist)
                (setf arr 
                        (adjust-array arr 
                           (if (floatp resize) 
                             (floor (* size resize))
                             (+ size resize)))))))
    (setf (aref arr ptr) val)
    (incf (reslist-fillptr reslist))))

(declaim (inline reslist-pop))
(defun reslist-pop (reslist)
  "Get the next free item from RESLIST. Restock the list if RESLIST has a stocking function."
  (declare (type reslist reslist))
  (let ((arr (reslist-arr reslist)))
    (declare (type simple-vector arr))
    (when (zerop (reslist-fillptr reslist)) 
      (if-bind (fn (reslist-stock-fn reslist))
       (let ((size (array-total-size arr)))
         (declare (type integer size))
         (loop for i from 0 to (1- size) do
               (setf (aref arr i) (funcall fn)))
         (setf (reslist-fillptr reslist) size))
       (error "Reslist: Nothing left to pop.")))
    (let ((ptr (decf (reslist-fillptr reslist))))
      (declare (type integer ptr))
      (prog1 
        (aref arr ptr)
        (setf (aref arr ptr) nil)))))

(defun intersect-predicates (fn &rest fns)
  "Paul Graham's. Return a predicate which is the AND of the arguments."
  (if (null fns)
      fn
    (let ((chain (apply #'intersect-predicates fns)))
      #'(lambda (x)
          (and (funcall fn x) (funcall chain x))))))

(proclaim '(inline ht2list))
(defun ht2list (ht)
  "Return a list of the values in a hashtable."
  (loop for val being the hash-value of ht collect val))

(defun file-size (path)
  (with-open-file (s path)
     (file-length s)))

(defmacro declare-ignore (&rest vars)
  "Used to use variable, where (declare (ignore ...)) cannot be used."
  `(progn ,@vars))

#+windows(defun usr-bin-file (filename) :xml)


;;; As of SLED11 file --brief returns:
;;;  XML:  XML  document text
;;;  Text: UTF-8 Unicode text
#+linux 
(defun usr-bin-file (filename)
  "Return results from the /usr/bin/file command as a keyword. Quick and Dirty -- test it!"
  (when (pathname filename) (setf filename (namestring (truename filename))))
  (let ((types `(("XML" . :xml) ("Zip" . :zip) ("ASCII" . :text) ("UTF-8" . :text)
		 ("\"\\011XML" . :xml)  ; This one for version of /usr/bin/file on amber.omg.org
		 ("HTML" . :html)))
	(stream #+Lispworks(sys:open-pipe (format nil "/usr/bin/file --brief '~A'" filename))
		#-Lispworks(error 'unknown-platform :string "Don't know how to start a process.")))
    (flet ((find-type (line)
	      (loop for type in types do 
		    (when-bind (pos (search (car type) line))
			       (when (zerop pos) (return-from find-type (cdr type)))))))
      (loop for line = (read-line stream nil nil) with result = nil
	    while line when (setf result (find-type line)) return result))))

#|
(defun tryme ()
  (with-input-from-string (stream "\"\\011XML")
    (let ((types `(("XML" . :xml) ("Zip" . :zip) ("ASCII" . :text) ("UTF-8" . :text)
		   ("\"\\011XML" . :xml)  ("HTML" . :html))))
      (flet ((find-type (line)
	       (loop for type in types do 
		    (when-bind (pos (search (car type) line))
		      (when (zerop pos) (return-from find-type (cdr type)))))))
	(loop for line = (read-line stream nil nil) with result = nil
	   while line when (setf result (find-type line)) return result)))))
|#


(defun vector-count-if (pred vector)
  "Like count-if, usually signifies that you should have used a list ;^)"
  (loop for v across vector with count = 0
	when (funcall pred v) do (incf count)
	finally (return count)))

;;; This ia a metaobject. There exists only one instance of such classes.
(defclass singleton (standard-class)
  ((the-instance :reader the-instance :initform nil)))

(defmethod validate-superclass ((class singleton)
                                (superclass standard-class))
  t)

(defmethod validate-superclass ((class t)
                                (superclass singleton))
  t)

;;;(defmethod validate-superclass ((class singleton)
;;;                                (superclass standard-object))
;;;  t)

(defmethod make-instance :around ((class singleton) &rest initargs &key)
  (declare (ignore initargs))
  (or (the-instance class)
      (setf (slot-value class 'the-instance) (call-next-method))))

(defmethod reinit-singleton ((class symbol))
  "Start over, without an instance. Used in developement."
  (setf (slot-value (find-class class) 'the-instance) nil))

(defun substitute-string (new old str)
  (let ((len (length old)) pos)
    (loop while (setf pos (search old str)) do  
         (setf str (strcat (subseq str 0 pos) new (subseq str (+ len pos)))))
    str))

(defun copy-file (in-path out-path)
  "Copy file from IN-PATH to OUT-PATH. Assumes latin-1 with linefeed, at least."
  (with-open-file (in in-path :direction :input)
    (with-open-file (out out-path :direction :output :if-exists :supersede)
      (loop 
       (mvb (line no-newline-p) (read-line in nil nil)
	 (when (null line) (return nil))
	 (write-string line out)
	 (unless no-newline-p (terpri out)))))))

(defmacro push-last (val place)
  "Like push but put VAL onto the last position of PLACE."
  `(setf ,place (append ,place (list ,val))))

(defmacro pushnew-last (val place &key (key '#'identity) (test '#'eql))
  "Like pushnew but put VAL onto the last position of PLACE."
  `(if (member ,val ,place :test ,test :key ,key)
    ,place
    (setf ,place (append ,place (list ,val)))))

(defun basic-ascii-string (str replace-char &key (warn nil))
  "Replace all charcters of STR that are not between ascii 32 and 126 inclusive
   with REPLACE-CHAR. If REPLACE-CHAR is nil, just skip the character."
  (with-output-to-string (s)
    (loop for c across str
	  for code = (char-code c)
	  do (cond ((and (> code 31) (< code 127)) 
		    (write-char c s))
		   (t 
		    (when replace-char (write-char replace-char s))
		    (when warn (warn "Not basic ascii: ~A" code)))))))

(defun basic-ascii-string-file (filename replace-char)
  "Replace all charcters of STR that are not between ascii 32 and 126 inclusive
   with REPLACE-CHAR. If REPLACE-CHAR is nil, just skip the character."
  (with-open-file (out "/home/pdenno/clean.xml" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-open-file (s filename :direction :input)
      (loop for line = (read-line s nil nil)
	 while line do
	   (loop for c across line
	      for code = (char-code c)
	      do (cond ((or (and (> code 31) (< code 127))
			    (= code 9))
			(write-char c out))
		       (t 
			(if replace-char 
			    (write-char replace-char out)
			    (warn "Skipping char ~S (code ~A)" c (char-code c))))))
	   (write-char #\Linefeed out)))))

(defun basic-ascii-string-file+ (filename 
				 &key (replace-tab
				       '((13 . #\NewLine) (194 . #\A) (195 . #\A) (160 . #\Space)
					 (130 . #\Space) (123 . #\Space) (162 . #\Space) (128 . #\Space)
					 (156 . #\Space) (157 . #\Space) (153 . #\Space))))
  "Replace all charcters of STR that are not between ascii 32 and 126 inclusive
   with REPLACE-CHAR. If REPLACE-CHAR is nil, just skip the character."
  (with-open-file (out "/home/pdenno/clean.xml" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-open-file (s filename :direction :input)
      (loop for line = (read-line s nil nil) with line-cnt = 0
	 while line do
	   (incf line-cnt)
	   (loop for c across line
	      for code = (char-code c)
	      do (cond ((or (and (> code 31) (< code 127))
			    (= code 9))
			(write-char c out))
		       (t 
			(if-bind (replace-char (cdr (assoc code replace-tab)))
			    (write-char replace-char out)
			    (warn "Line ~A: Skipping char ~S (code ~A)" line-cnt c (char-code c))))))
	   (write-char #\Linefeed out)))))


(defun find-non-ascii (filename)
  "Report lines containing non-ascii characters."
  (with-open-file (s filename :direction :input)
    (loop for line = (read-line s nil nil)
          for i from 1
	  while line do
	 (loop for c across line 
	       for j from 1
 	       for code = (char-code c)
 	       unless (or (and (> code 31) (< code 127))
			  (= code 9)) ; #\Tab
               do (format t "~%Line ~A.~A: ~A" i j line)
	       (return nil)))))

(defun find-non-utf-8 (filename)
  "Report lines containing non-utf-8 characters."
  (with-open-file (s filename :direction :input)
    (loop for line = (read-line s nil nil)
          for i from 1
	  while line do
	 (loop for c across line 
	       for j from 1
 	       for code = (char-code c)
 	       unless (or 
		       (<= #x20 code #x7e)
		       (<= #xa0 code #xff)
		       (<= #x93 code #x9d) ; probably temporary
		       (= code #x80)  ; probably temporary
		       (= code #x09)) ; probably temporary
               do (format t "~%Code x~x Line ~A.~A: ~A" code i j line)
	       (return nil)))))

(declaim (inline elip))
(defun elip (str &optional (max-len 18))
  "For STR 'this is fairly long' return 'this is...' where STR is chopped at MAX-LEN
   and terminated with elipsis, EXCEPT, return whole string if (+ 3(length STR)) < MAX-LEN."
  (let ((len (length str)))
    (if (<= len max-len)
	str
	(format nil "~A..." (subseq str 0 (- max-len 3))))))

(defun ordinal (n)
  "Return a string that is the ordinal number (e.g. '1st') 
   corresponding to N, an integer."
  (when (integerp n)
    (cond ((<= 11 n 19) (format nil "~Ath" n))
	  (t 
	   (format nil "~A~A" 
		   n
		   (case (rem n 10) 
		     (1 "st") (2 "nd") (3 "rd") ((or 4 5 6 7 8 9 0) "th")))))))

;;; POD would be nicer if it didn't use hash tables! Coersion not so nice either
;;; POD rewrite to use plists ???
;;;
;;; Because of the use of hash-tables, here, :test (NYI) has to be one of
;;; eq, eql, equal, or equalp. That puts more requirements on the key. 
;;; I should probably switch to assoc, etc. as some point. 
(defun equiv-classes (objects &key (key #'identity))
  "OBJECTS is a list. Return a list of list of all things that are the same
   with respect to the test function. If x and y are in an equivalence class, 
   and x was before y in OBJECTS, x will be before y in the sublist representing
   that equivalence class."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for obj in (coerce objects 'list)
	  for k = (funcall key obj) do
	 (setf (gethash k ht) 
	       (append (gethash k ht) (list obj))))
    (loop for key being the hash-key of ht collect (gethash key ht))))

(defun set-p (lis &key (test 'eql))
  "Return T unless list LIS has duplicates."
  (loop for sub on lis
       when (member (car sub) (cdr sub) :test test)
       return nil
       finally (return t)))

 (defun gather-duplicates (lis &key (test 'eql) (key 'identity))
   "Return of list of all elements in the argument LIS that are there multiply."
   (remove-duplicates
    (loop for sub on lis
       when (member (funcall key (car sub)) (cdr sub) :key key :test test)
       collect (car sub))
    :test test :key key))

(defun shadow-for-model (name)
  "Shadow a symbol if it conflicts with that in any USEd package. 
   Warn that you are doing it and return the new symbol. 
   Run this function with *package* being the model package."   
  (let* ((name-string (string name))
	 (symbol? (intern name-string)))
    (if (eql *package* (symbol-package symbol?))
	symbol?
	(progn 
	  (warn "Shadowing package ~A's symbol ~S in model ~A." 
		(package-name (symbol-package symbol?))
		name-string 
		(package-name *package*))
	  (shadow name-string)
	  (find-symbol name-string)))))

;;; "2010-12-08T17:30:00.000"
(defun date-to-utime (date &key (error-p t))
  "Convert ISO 8601 DATE of form 2010-12-08T17:30:00.000 to CL universal time"
  (flet ((fail ()
	   (if error-p (error "~A does not look like a 8601 date." date)
	       (return-from date-to-utime nil))))
    (mvb (success vec)
	(cl-ppcre:scan-to-strings 
	 "^([0-9]\{4,4\})-([0-9]\{2,2\})-([0-9]\{2,2\})(T([0-9]\{2,2\}):([0-9]\{2,2\})(:([0-9]\{2,2\}))?(.[0-9]\{1,3})?)?$"
	 date)
      (if success
	(encode-universal-time 
	 (if-bind (sec (aref vec 7))   (read-from-string sec) 0)
	 (if-bind (min (aref vec 5))   (read-from-string min) 0)
	 (if-bind (hour (aref vec 4))  (read-from-string hour) 0)
	 (if-bind (day (aref vec 2))   (read-from-string day) (fail))
	 (if-bind (month (aref vec 1)) (read-from-string month) (fail))
	 (if-bind (year (aref vec 0))  (read-from-string year) (fail)))
	(fail)))))

(defun utime-to-date (time)
  "Convert universal time value TIME to a string of form 2011-03-05T18:33:00"
  (mvb (sec min hour day month year) 
      (decode-universal-time time)
    (format nil "~A-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
	    year month day hour min sec)))

(defun gethash-inv (value ht &key (test #'identity) all-p)
  "Return the first key in HT (or all keys if ALL-P=T) which is equal to VALUE."
  (loop for val being the hash-value of ht using (hash-key key) 
     when (and (eql val value) (funcall test key)) 
     if all-p collect key
     else return key))

(defun format-to-size (str length &optional (prefix " "))
  "Returns the string STR reformated: Remove #\Newlines and place new ones
   so that no lines except last are shorter than LENGTH. If PREFIX is specifed,
   start every line with that string."
  (flet ((pull-out (&optional target str)
	   (if str (cl-ppcre:regex-replace-all str target " ") str)))
    (if str
	(let ((long-str (string-trim '(#\Space) (cl-ppcre:regex-replace-all "[\\r\\n]" str " "))))
	  (setf long-str (reduce #'pull-out (list long-str "<html>" "</html>" "<body>" "</body>" "<head>" "</head>")))
	  (format nil (format nil "~~{~A ~~A~~^~~%~~}" prefix)
		  (mapappend #'(lambda (l) (split l #\Space :min-size length)) 
			     (split long-str #\Newline))))
      "")))

(defvar *lpath-ht* (make-hash-table) "Typically, load.lisp will redefine this.")

(defun  lpath-init (alist)
  "Typically, load.lisp will redefine this."
  (loop for (key . val) in alist 
       do (setf (gethash key *lpath-ht*) val)))

(defun lpath (logical-host path) 
  "Return a pathname merging 'LOGICAL HOST' (CL logical pathname concept) to PATH."
  (let ((default (gethash logical-host *lpath-ht*)))
    (if default
	(merge-pathnames path default)
	(error "Unknown logical host: ~A" logical-host))))

(defvar *uniq-cons-table* (make-hash-table :test #'eq))
(defvar *uniq-atom-table* (make-hash-table :test #'equal))

;;; From Norvig Paradigms of AI programming, pg 335.
(defun unique-cons (x y)
  "Return a cons s.t. (eq (ucons x y) ucons x2 y2)) is true
   whenever (equal x x2) and (equal y y2) are true."
  (let (ux uy)
    (let ((car-table
	    (or (gethash x *uniq-cons-table*)
		(gethash (setf ux (unique x)) *uniq-cons-table*)
		(setf (gethash ux *uniq-cons-table*)
		      (make-hash-table :test #'eq)))))
      (or (gethash y car-table)
	  (gethash (setf uy (unique y)) car-table)
	  (setf (gethash uy car-table) (cons ux uy))))))

(defun unique (exp)
  "Return a canonical representation that is EQUAL to exp.
   such that (equal x y) implies (eq (unique x) (unique y))."
  (typecase exp
    (symbol exp)
    (fixnum exp)
    (atom (or (gethash exp *uniq-atom-table*)
	      (setf (gethash exp *uniq-atom-table*) exp)))
    (cons (unique-cons (car exp) (cdr exp)))))

(defun ulist (&rest args)
  "A uniquified list."
  (unique args))

(defun uappend (x y)
  "A unique list equal to (append x y)."
  (if (null x)
      (unique y)
      ;; I think unique-cons and not ucons (pg 334) is what is intended here.
      (unique-cons (first x) (uappend (rest x) y)))) 

#+nil
(defmacro with-package-renamed ((old-name new-name) &body body)
  "Rename a package within the scope of body."
  `(unwind-protect
	(progn 
	  (rename-package ,old-name ,new-name)
	  ,@body)
     (rename-package ,new-name ,old-name)))

(eval-when (:compile-toplevel :load-toplevel :execute)    
  (unintern 'pod-utils::str))

;;; http://en.wikipedia.org/wiki/Universally_unique_identifier
;;; 
;;; A UUID is a 16-byte (128-bit) number. The number of theoretically possible UUIDs is therefore 
;;; about 3 × 10^38. In its canonical form, a UUID consists of 32 hexadecimal digits, displayed in 
;;; 5 groups separated by hyphens, in the form 8-4-4-4-12 for a total of 36 characters 
;;; (32 digits and 4 hyphens).
;;;;
;;; Version 4 (random)
;;; 
;;; Version 4 UUIDs use a scheme relying only on random numbers. This algorithm sets the version number 
;;; as well as two reserved bits. All other bits are set using a random or pseudorandom data source. 
;;; Version 4 UUIDs have the form xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx where x is any hexadecimal digit 
;;; and y is one of 8, 9, A, or B.

;;; (format nil "~8,'0X" (random (expt 16 8)))
;;; ==> "725A27D7"

(defun new-uuid ()
  "Return a uuid string, using an application random constant 725A27D7,
   universal time and 14 random hexadecimal digits."
  (let ((utime (format nil "~8,'0X" (get-universal-time))))
    ;; 725A27D7-fourDigitsOfUniversal-4threeRandom-9threeRandom-fourDigitsOfUniversal8random"
    (format nil "725A27D7-~A-4~A-9~A-~A~A"
	    (subseq utime 0 4)
	    (format nil "~3,'0X" (random 4096))
	    (format nil "~3,'0X" (random 4096))
	    (subseq utime 4)
	    ;; POD 4294... is 16^8 -- larger than most-postive-fixnum. OK?
	    (format nil "~8,'0X" (random 4294967296)))))
