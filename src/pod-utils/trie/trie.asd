;;; -*- Mode: Lisp; -*-

(in-package :user-system)

(defsystem :trie
  :serial t
  :depends-on (:bordeaux-threads :pod-utils)
  :components
  ((:file "package")
   (:file "trie-utils")
   (:file "unify")
   (:file "trie")
   #+nil (:file "test-trie"))) ; suo...






