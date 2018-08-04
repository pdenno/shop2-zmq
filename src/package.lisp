(in-package :cl-user)

(defpackage :shop2-zmq
  (:nicknames :s2zmq)
  (:use :cl :shop2 :asdf/interface)
  (:export #:start-server))
