(defpackage :shop2-zmq
  (:use :common-lisp :asdf))

;;; I gave up on trying to get ASDF to work on shop2!
;;; Is it that "shop2-zmq" is also a package?
;;; That I have a ./src directory?
;;; That the ~/.config/common-lisp/source-registry.conf is messed up?
(asdf:defsystem "shop2-zmq"
  :long-description "A server of UMd's SHOP2 planner using ZeroMQ"
  :depends-on ("shop2" "zeromq" "kif")
  :serial t
  :components ((:file "package")
	       (:file "plan-server")))
