(in-package :cl-user)

(defpackage :shop2-zmq
  (:nicknames :s2zmq)
  (:use :cl :shop2)
  (:export #:start-server))
