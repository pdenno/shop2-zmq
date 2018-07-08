
;;; Purpose: Read a file of kif into a hashtable indexed by the line number at which
;;;          each toplevel form appears. 

(in-package :kif)

(defparameter *kif-readtable* (copy-readtable nil))
(defvar *kif-std-readtable* (copy-readtable nil))
(defvar *kif-line* 1 "Line at which we are reading")
(defparameter *kif-forms-ht* (make-hash-table)) ; set to the ht in a kif-forms object, usually. 

(defun set-variable-property (x)
  (cond ((atom x) 
         (cl:and (symbolp x)
              (cl:or (when (char= #\? (char (symbol-name x) 0))
                    (setf (get x 'kvar) t))
                  (when (char= #\@ (char (symbol-name x) 0))
                    (setf (get x 'row-variable) t)))))
        ((consp x)
         (set-variable-property (car x))
         (set-variable-property (cdr x))))
  x)

(let ((paren-cnt 0))
(defun clear-paren-cnt () (setf paren-cnt 0))
(defun kif-lparen (stream char)
  (incf paren-cnt)
  (let ((save-line *kif-line*)
        (result (funcall (get-macro-character #\( *kif-std-readtable*) stream char)))
    ;; everything else in this file just counts lines.
    (when (= paren-cnt 1) (setf (gethash save-line *kif-forms-ht*) result))
    (decf paren-cnt)
    result))
)

(defun kif-newline (stream char)
  (declare (ignore stream char))
  (incf *kif-line*)
  (values))

(defun kif-comment (stream char)
  (funcall (get-macro-character #\; *kif-std-readtable*) stream char)
  (incf *kif-line*)
  (values))

(defun kif-string (stream char)
  (let ((str (funcall (get-macro-character #\" *kif-std-readtable*) stream char)))
    (incf *kif-line* (count #\Newline str))
    str))

(defun kif-word-token (stream char)
  (unread-char char stream)
  (loop with string = (make-array 100 :element-type 'character :adjustable t)
	and strlen = 100 and stridx = 0
	for ch = (read-char stream t nil t)
	while (cl:or (digit-char-p ch)
		     (char<= #\a ch #\z)
		     (char<= #\A ch #\Z)
		     (char= ch #\?)
		     (char= ch #\@)
		     (char= ch #\-)
		     (char= ch #\_)
		     (char= ch #\+))
	finally (progn (adjust-array string stridx)
		       (unread-char ch stream)
		       (cl:return (intern string)))
	do (setf (aref string stridx) ch)
	   (incf stridx)
	   (when (= stridx strlen)
	     (incf strlen 40)
	     (setf string (adjust-array string strlen :element-type 'character)))))

(defun kif-ignore (stream char)
  (declare (ignore stream char))
  (values))

(let ((*readtable* *kif-readtable*))
  (set-macro-character #\( #'kif-lparen)
  (set-macro-character #\Newline #'kif-newline)
  (set-macro-character #\; #'kif-comment)
  (set-macro-character #\" #'kif-string)
  (loop for ch in '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
		    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
		    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
		    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z 
                    #\? #\@ #\-)
	do (set-macro-character ch #'kif-word-token))
  (loop for code from 160 to 255
	do (set-macro-character (code-char code) #'kif-ignore)))

(defun kif-read (&rest args)
  (let ((*readtable* *kif-readtable*)
        (*package* (find-package :ku)))
    (set-variable-property (apply #'read args))))

(defmethod kif-readfile (filename (kif-forms kif-forms) &key continue)
  (with-slots (forms-ht files-read lines-read) kif-forms
    (unless continue 
      (setf lines-read 1) ; actually current line ???
      (clrhash forms-ht))
    (setf *kif-forms-ht* forms-ht)
    (setf *kif-line* lines-read)
    (pushnew filename files-read :test #'string=)
    (clear-paren-cnt)
    (with-open-file (stream filename :direction :input)
      (loop for form = (kif-read stream nil :eof)
         when (eql form :eof) return *kif-line*))
    (setf lines-read *kif-line*)))

(defun kif-read-from-string (str)
  (setf str (strcat str " ")) ; POD get around a bug...
  (clear-paren-cnt)
  (with-input-from-string (stream str)
    (kif-read stream nil :eof)))

(defun kif-n2form (n &key ht)
  (gethash n ht))

(defun kif-form2n (form &key ht)
  (loop for f being the hash-value of ht using (hash-key key)
        when (eql f form) return key))

(defun kif-export-safe-symbols ()
  " 'Safe' means it has lowercase and it isn't a variable." 
  (let ((package (find-package :ku)))
    (do-symbols (s package)
      (when (cl:and (cl:not (kvar-p s))
                 (some #'lower-case-p (symbol-name s)))
;        (format t " |~A|" (symbol-name s))
        (export s package)))))






  


