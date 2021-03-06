(in-package :cl-user)
(require :asdf)

(load "~/quicklisp/setup.lisp")

;;; I gave up on trying to make ASDF work. 
(ql:quickload "fiveam")
(ql:quickload "bordeaux-threads")
(ql:quickload "zeromq")
(ql:quickload "shop2")
(ql:quickload "cl-ppcre")

(pushnew :plan-server *features*)
(load (compile-file "~/Documents/git/shop2-zmq/src/package.lisp"))
;(load (compile-file "~/Documents/git/shop2-zmq/src/plan-server.lisp")) ; I think this was used before load-system worked.
(load (compile-file "~/Documents/git/shop2-zmq/src/pod-utils/packages.lisp"))
(asdf:load-system :kif)
(asdf:load-system :shop2-zmq)

; I'm going to try it from the shell with save-lisp.sh
;(sb-ext:save-lisp-and-die "zeromq-shop2-kif-2018-08-04" :purify t :executable t :toplevel 's2zmq:start-server)

