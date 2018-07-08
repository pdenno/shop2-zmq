
;;; Copyright (c) 2001 Logicon, Inc.
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

(in-package :pod-utils)

(export '(*debug-stream* show-debugging set-debugging dbg-VARS
	  get-debugging clear-debugging full-debugging dbg-message dbg-msg dbg-pprint
	  dbg-funcall when-debugging if-debugging with-debugging))

(defvar *debugging* nil "Holds a property list where the property name is the 
 subsystem and the value is the debugging level.")

(defparameter *dbg-tags* '(:scm :sim :inv :match :qvt :validate :readers  :any :data :query :propagate
			   :infer :post :time :search :cmof :tform :exp :exx :lexer :parser :triples))

(defvar *debug-stream* nil "Set this to some stream object.")

(defun show-debugging (&optional (stream *standard-output*))
  (loop for tag in *dbg-tags*
	as lvl = (getf *debugging* tag)
	do (format stream "~&~S ==> ~A~%" tag lvl)))

(defun set-debugging (tag level)
  (assert (member tag *dbg-tags*))
  (check-type level (integer 0 5))
  (if (zerop level)
    (remf *debugging* tag)
    (progn (setf (getf *debugging* tag) level)
	   (setf (getf *debugging* :any) 1)))
  (values *debugging* *dbg-tags*))

(defun get-debugging (&optional tag)
  (if tag
      (values (getf *debugging* tag)
	      *debugging*
	      *dbg-tags*)
    (values *debugging* *dbg-tags*)))

(defun clear-debugging (&optional tag)
  (assert (or (null tag) (member tag *dbg-tags*)))
  (if (null tag)
      (setf *debugging* nil)
    (remf *debugging* tag))
  (values *debugging* *dbg-tags*))

;;; deprecated
(defun dbg-message (tag level string &rest args)
  "m e s s a g e    is too much to type."
  (apply #'dbg-msg tag level string args))

(defun dbg-msg (tag level string &rest args)
  (declare (special *debug-stream*))
  (when-bind (lvl (getf *debugging* tag))
    (when (>= lvl level)
      (write-string (apply #'format nil string args) *debug-stream*)
      (force-output *debug-stream*))))

(defmacro dbg-VARS (tag level &rest vars)
  `(when-bind (lvl (getf *debugging* ,tag))
     (when (>= lvl ,level)
       (VARS ,@vars))))

(defun dbg-pprint (tag level obj)
  (declare (special *debug-stream*))
  (when-bind (lvl (getf *debugging* tag))
    (when (>= lvl level)
      (pprint obj *debug-stream*)
      (terpri *debug-stream*))))

(defmacro dbg-funcall (tag level fun &rest args)
  (assert (member tag *dbg-tags*))
  (check-type level (integer 1 5))
  (let ((result (make-symbol "RESULT")))
    `(let (,result)
      (dbg-message ,tag ,level "~2%Calling ~S~%  with ~S" ,fun ',args)
      (setf ,result (funcall ,fun ,@args))
      (dbg-message ,tag ,level "~%  ==> ~S" ,result)
      ,result)))

(defmacro when-debugging ((tag level) &body body)
  (assert (member tag *dbg-tags*))
  (check-type level (integer 1 5))
  `(when (let ((lvl (getf *debugging* ,tag)))
	   (and lvl (> lvl ,(1- level))))
     ,@body))

(defmacro if-debugging ((tag level) then else)
  (assert (member tag *dbg-tags*))
  (check-type level (integer 1 5))
  (with-gensyms (lvl)
    `(let ((,lvl (getf *debugging* ,tag)))
       (if (and ,lvl (>= ,lvl ,level)) ,then ,else))))

(defmacro with-debugging ((tag level) &body body)
  "Establishes an additional debugging tag and level during the body of this form."
  `(let ((*debugging* *debugging*))
    (set-debugging ,tag ,level)
    ,@body))
