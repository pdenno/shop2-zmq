
(asdf:defsystem :pod-utils
  :name "pod-utils"
  :serial t
  :depends-on (:cl-ppcre)
  :components
  ((:file "packages")
   (:file "utils")
   (:file "debugging")))

