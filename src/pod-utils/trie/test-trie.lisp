;;; -*- Mode: Lisp; -*-

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


(in-package :trie)

;;; Purpose: Test trie code 
;;; Date: 2004-10-11
;;; Copyright 2004, Peter Denno

#+nil ; sbcl
(eval-when (:compile-toplevel)
  (require :sb-sprof)) 

;;; (time (kif:kif2clause-readfile #P"ccts:data;kb;amis.kif"))
;;; (time (suo-test-tries))

#+nil ; sbcl
(defun suo-profile ()
  (sb-sprof:start-profiling)
  (suo-test-tries)
  (sb-sprof:stop-profiling)
  (sb-sprof:report :type :graph))

#+nil ; suo...
(defun suo-test-tries ()
  (macrolet ((do-for-everything (&body body)
               `(loop for clause across (passive) do
                 (loop for lit in (clause-val clause) do
                       (with-literal (pred poslit) lit
                         (let ((term (if poslit lit (second lit))))
                           (unless (var-p pred)
                             ,@body)))))))
    (loop for i from 1 to 40 do 
          (clear-tries)
          (do-for-everything term (setf (get pred 'tries) nil)) ;; because recompilation loses 
          (do-for-everything 
              (trie-add term (clause-index clause)))
          ;; predicates closure
          (do-for-everything 
              (unless (trie-query term)
                (error "~%Cannot find term ~A" term))))))
    #|(do-for-everything
        (trie-delete term)|#

#+nil ; suo 
(defun suo-test-serialize ()
  (declare (special *termbuf*))
  (macrolet ((do-for-everything (&body body)
               `(loop for clause across (passive) do
                 (loop for lit in (clause-val clause) do
                       (with-literal (pred poslit) lit
                         (let ((term (if poslit lit (second lit))))
                           (unless (var-p pred)
                             ,@body)))))))
    (let ((cnt 0))
      (do-for-everything  (incf cnt) (serialize* term *termbuf*))
      (format t "~% ~A terms" cnt))))

#+nil ; suo 
(defun test-tries-load (data)
  "Diagnostic function for loading data."
  (macrolet ((do-for-everything (&body body)
              `(loop for lit in data do
                    (with-literal (pred poslit) lit
                       (let ((term (if poslit lit (second lit))))
                         (unless (var-p pred) 
                           ,@body))))))
    (let ((cnt 0))
      (clear-tries)
      (do-for-everything (setf (get pred 'tries) nil))
      (do-for-everything (trie-add term (incf cnt))))))

#| Old code, navigate-next not right arity.
(defun tryme ()
  (setf (get 'p 'tries) nil)
  (trie-add '(p (a (c))) 1) ; 0 3 2 0 0 0
  (trie-add '(p (a c)) 1)   ; 0 3 1 0 
  (trie-add '(p (a b c)) 1) ; 0 3 0 0 0
  (trie-add '(p (x)) 2) ; 0 2 0
  (trie-add '(p (y)) 3) ; 0 1 0
  (trie-add '(p (z)) 4) ; 0 0 0 
  (let ((br (make-bind-rec
             :root (cdar (trie-arcs (get 'p 'tries)))
             :path '(0 0 0)))
        next)
    (loop while (setf next (navigate-next br))
          do (VARS next)
          (setf (bind-rec-path br) next))))
|#

(defvar *trie-tests-ht* (make-hash-table) "test case hash table")

(defclass trie-test-case ()
  ((data :initarg :data)
   (query :initarg :query)
   (expected :initarg :expected)
   (purpose :initarg :purpose)))

(defmacro def-trie-test (name body)
  `(setf (gethash ,(if (symbolp name) `',name name) *trie-tests-ht*)
    (make-instance 'trie-test-case
     :data ',(second (assoc :data body))
     :query ',(second (assoc :query body))
     :expected ',(second (assoc :expected body))
     :purpose ',(second (assoc :purpose body)))))

(def-trie-test 1
  ((:data ((p a b) (p a c) (p w w) (p (z) b) (p y y)))
   (:query (p ?x ?x))
   (:expected ((p y y) (p w w)))))

(def-trie-test 2
  ((:data ((p x x z) (p w w w) (p a a b)))
   (:query (p ?x ?x ?x))
   (:expected ((p w w w)))
   (:purpose "Simple backtracking")))

(def-trie-test 3
    ((:data ((p x x y z) (p w w u u) (p a a b c)))
     (:query (p ?x ?x ?y ?y))
     (:expected ((p w w u u)))
     (:purpose "Backtracking")))

(def-trie-test 4
    ((:data ((p a a b d) (p w w u u 5) (p w w u u) (p a a b c)))
     (:query (p ?x ?x ?y ?y ?z))
     (:expected ((p w w u u 5)))
     (:purpose "Backtracking with variable arity")))

(def-trie-test 5
    ((:data ((p (c)) (p a b) (p (b))))
     (:query (p ?x ?y))
     (:expected ((p a b)))
     (:purpose "Structure")))

(def-trie-test 5.1
    ((:data ((p (w))))
     (:query (p ?x ?y))
     (:expected nil)
     (:purpose "Structure")))

(def-trie-test 6
    ((:data ((p c d) (p (b)) (p a b)))
     (:query (p ?x ?x))
     (:expected nil)
     (:purpose "Structure")))

(def-trie-test 7
    ((:data ((p (b)) (p a b)))
     (:query (p (?x)))
     (:expected ((p (b))))
     (:purpose "Structure")))

(def-trie-test 8
    ((:data ((p (pred  a b) (pred c d))
             (p (pred  (a) b) (pred c d))
             (p (pred  a b) (pred a d))
             (p (pred  (a) b) (pred a d))
             (p (pred  m n) (pred o p))))
     (:query (p (pred ?x ?y) (pred ?x ?z)))
     (:expected ((p (pred a b) (pred a d))))
     (:purpose "Structure")))

(def-trie-test 9
    ((:data ((p (y)) (p (a) (x))))
     (:query (p ?x))
     (:expected ((p (y))))
     (:purpose "Structure")))

(def-trie-test 10
    ((:data ((p a b) (p (y) (y)) (p (a) (x))))
     (:query (p ?x ?x))
     (:expected ((p (y) (y))))
     (:purpose "Structure")))

(def-trie-test 11
  ((:data ((p a b) (p a c) (p w w) (p z b) (p y y) (p x x)))
   (:query (p ?x ?x))
   (:expected ((p x x) (p y y) (p w w)))))


(defun test-tries ()
  (loop for test being the hash-value of *trie-tests-ht* using (hash-key name) do
        (with-slots (data query expected purpose) test
          (test-tries-load data)
          (let ((result (trie-query-all query)))
            (when (set-exclusive-or result expected :test #'equal)
              (format t "~2%Test ~A (~A):~%~{~A~%~} --> ~A ~%Returns: ~{~A~%~} ~%Expected ~{~A~%~}"
                      name purpose (reverse data) query result expected))))))

(defun test-one (name)
  (with-slots (data query expected purpose) (gethash name *trie-tests-ht*)
    (test-tries-load data)
    (let ((result (trie-query-all query)))
      (format t "~{~A~%~}  ~%==> ~A ~%Returns:~{~%~A~}" (reverse data) query result)
      (if (set-exclusive-or result expected :test #'equal)
        (format t "~%Failure!")
        (format t "~%OK.")))))


#| A trie looks like this: (unless its arcs are hash-tables.)

E> (get 'p 'tries)

#S(TRIE
   :VALUE NIL
   :CLAUSES NIL
   :ARCS ((P
           . #S(TRIE
                :VALUE NIL
                :CLAUSES NIL
                :ARCS ((Y
                        . #S(TRIE
                             :VALUE NIL
                             :CLAUSES NIL
                             :ARCS ((Y
                                     . #S(TRIE
                                          :VALUE (P Y Y)
                                          :CLAUSES (5)
                                          :ARCS NIL)))))
                       (:O
                        . #S(TRIE
                             :VALUE NIL
                             :CLAUSES NIL
                             :ARCS ((Z
                                     . #S(TRIE
                                          :VALUE NIL
                                          :CLAUSES NIL
                                          :ARCS ((:C
                                                  . #S(TRIE
                                                       :VALUE NIL
                                                       :CLAUSES NIL
                                                       :ARCS ((B
                                                               . #S(TRIE
                                                                    :VALUE (P
                                                                            (Z)
                                                                            B)
                                                                    :CLAUSES (4)
                                                                    :ARCS NIL)))))))))))
                       (W
                        . #S(TRIE
                             :VALUE NIL
                             :CLAUSES NIL
                             :ARCS ((W
                                     . #S(TRIE
                                          :VALUE (P W W)
                                          :CLAUSES (3)
                                          :ARCS NIL)))))
                       (A
                        . #S(TRIE
                             :VALUE NIL
                             :CLAUSES NIL
                             :ARCS ((C
                                     . #S(TRIE
                                          :VALUE (P A C)
                                          :CLAUSES (2)
                                          :ARCS NIL))
                                    (B
                                     . #S(TRIE
                                          :VALUE (P A B)
                                          :CLAUSES (1)
                                          :ARCS NIL))))))))))


;;;========================================================================


E> (get 'p 'tries)
#S(TRIE
   :VALUE NIL
   :CLAUSES NIL
   :ARCS ((P
           . #S(TRIE
                :VALUE NIL
                :CLAUSES NIL
                :ARCS ((A
                        . #S(TRIE
                             :VALUE NIL
                             :CLAUSES NIL
                             :ARCS ((B
                                     . #S(TRIE
                                          :VALUE (P A B)
                                          :CLAUSES (2)
                                          :ARCS NIL)))))
                       (:O
                        . #S(TRIE
                             :VALUE NIL
                             :CLAUSES NIL
                             :ARCS ((B
                                     . #S(TRIE
                                          :VALUE NIL
                                          :CLAUSES NIL
                                          :ARCS ((:C
                                                  . #S(TRIE
                                                       :VALUE (P (B))
                                                       :CLAUSES (3)
                                                       :ARCS NIL)))))
                                    (C
                                     . #S(TRIE
                                          :VALUE NIL
                                          :CLAUSES NIL
                                          :ARCS ((:C
                                                  . #S(TRIE
                                                       :VALUE (P (C))
                                                       :CLAUSES (1)
                                                       :ARCS NIL)))))))))))))


|#