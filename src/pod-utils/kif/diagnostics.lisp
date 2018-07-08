;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: KIF; -*-

;;; Purpose: Diagnostic functions to test clausifier.

(in-package :kif)

#+nil
(defun not-p (form)
  "Cannot use this during clausifier debugging cuz e.g. (domain not 1 formula)"
  (let ((result (cl:and (consp form) 
                        (eql 'not (car form)))))
    (when result (assert (cl:not (caddr form))))
    result))
#+nil
(defun not-p* (form)
  "Cannot use this during clausifier debugging cuz e.g. (domain not 1 formula)"
  (let ((result (eql 'not (car form))))
    (when result (assert (cl:not (caddr form))))
    result))

(defun which-ones? (form)
  (unless (cl:equal form (clause-not-inward-demorgans form))
    (format t "~% demorgans"))
  (unless (cl:equal form (clause-not-inward-quantifiers form))
    (format t "~% quantifiers"))
  (unless (cl:equal form (clause-and-simplify form t))
    (format t "~% AND simplify"))
  (unless (cl:equal form (clause-or-simplify form t))
    (format t "~% OR simplify"))
  (unless (cl:equal form (clause-not-simplify form t))
    (format t "~% NOT simplify"))
  (unless (cl:equal form (clause-distributive form t))
    (format t "~% distributive"))
  (unless (cl:equal form (clause-skolemize form))
    (format t "~% skolemize"))
  (unless (cl:equal form (clause-remove-universals form t))
    (format t "~% universals")))

(defun clause-and-simplify (form pred-p)
  "Replace all subforms (and (and a) (and b c) d) with (and a b c d) etc.."
  (cond ((atom form) form)
        ((cl:and (and-p* form) pred-p (cl:not (third form))) ; (and a)
         (clause-and-simplify (second form) (pred-p (second form))))
        ((cl:and (and-p* form) pred-p)
         (nconc (list 'and) (mapappend #'(lambda (x) (if (and-p x) (cdr x) (list x))) (cdr form))))
        (t (cons (clause-and-simplify (car form) t)
                 (clause-and-simplify (cdr form) (predicate-p (second form)))))))

(defun clause-or-simplify (form pred-p)
  "Replace all subforms (or (or a) b) with (or a b) etc.."
  (cond ((atom form) form)
        ((cl:and (or-p* form) pred-p (cl:not (third form)))  ; (or a)
         (clause-or-simplify (second form) (pred-p (second form))))
        ((cl:and (or-p* form) pred-p) ; (or a (or b))
         (nconc (list 'or) (mapappend #'(lambda (x) (if (or-p x) (cdr x) (list x))) (cdr form))))
        (t (cons (clause-or-simplify (car form) t)
                 (clause-or-simplify (cdr form) (predicate-p (second form)))))))

(defun clause-not-simplify (form pred-p)
  "Replace all subforms (not (not a)) with a etc.."
  (cond ((atom form) form)
        ((cl:and (not-p* form) pred-p (consp (second form)) (not-p* (cadr form))) ; (not (not a))
         (clause-not-simplify (cadadr form) (pred-p (second form))))
        (t (cons (clause-not-simplify (car form) t)
                 (clause-not-simplify (cdr form) (predicate-p (second form)))))))

#+DEBUG
(defun clausify-no-renames (form)
  "This is used in diagnostics."
  (let (cform)
    (setf cform (copy-tree form))
    (setf form  (copy-tree form))
    
    (setf form (clause-not-inward-quantifiers form))
    (unless (cl:equal form cform)
      (setf cform (copy-tree form))
      (format t "~% inward quantifiers"))

    (setf form (clause-not-inward-demorgans form))  
    (unless (cl:equal form cform)
      (setf cform (copy-tree form))
      (format t "~% inward demorgans"))

    (setf form (clause-and/or/not-simplify form (consp form)))
    (unless (cl:equal form cform)
      (setf cform (copy-tree form))
      (format t "~% and/or/not"))

    (setf form (clause-skolemize form))
    (unless (cl:equal form cform)  
      (setf cform (copy-tree form))
      (format t "~% skolemize"))

    (setf form (clause-remove-universals form (consp form)))
    (unless (cl:equal form cform)  
      (setf cform (copy-tree form))
      (format t "~% universals"))

    (setf form (clause-distributive form (consp form)))
    (unless (cl:equal form cform)  
      (setf cform (copy-tree form))
      (format t "~% distributive"))

    form))

(defun nclausify (form n)
  (setf form (clausify form))
  (loop for i from 1 to n
        do (setf form (clausify-no-renames form)))
  form)


