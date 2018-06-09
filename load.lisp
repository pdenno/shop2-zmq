(in-package :cl-user)
(require :asdf)

;;; I gave up on trying to make ASDF work. 
(ql:quickload "fiveam")
(ql:quickload "bordeaux-threads")
(ql:quickload "zeromq")
(ql:quickload "shop2")

(pushnew :plan-server *features*)
(load (compile-file "/Users/pdenno/Documents/git/shop2-zmq/src/package.lisp"))
(load (compile-file "/Users/pdenno/Documents/git/shop2-zmq/src/plan-server.lisp"))
;(asdf:load-system :shop2-zmq)

(sb-ext:save-lisp-and-die "zeromq-shop2-2018-06-09" :purify t :executable t :toplevel 's2zmq:start-server)

