;;; -*- Mode: Lisp; -*-

(in-package :user-system)

(defsystem :kif
  :serial t
  :depends-on (:pod-utils :trie)
  :components
  ((:file "packages")
   (:file "kif-utils")
   (:file "reader")
   (:file "clausify")
   #+DEBUG(:file "diagnostics")))

(pushnew :kif *features*)




