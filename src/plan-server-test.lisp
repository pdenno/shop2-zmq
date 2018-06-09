(in-package :s2zmq)

(defun try1 ()
  (ask-shop2
   "(defdomain basic-example (
     (:operator (!pickup ?a) () () ((have ?a)))
     (:operator (!drop ?a) ((have ?a)) ((have ?a)) ())
     (:method (swap ?x ?y)
       ((have ?x))
       ((!drop ?x) (!pickup ?y))
       ((have ?y))
       ((!drop ?y) (!pickup ?x)))))"))

(defun try2 ()
  (ask-shop2
   "(defproblem problem1 basic-example
      ((have banjo)) ((swap banjo kiwi)))"))

(defun try3 ()
  (ask-shop2 "(find-plans 'problem1 :verbose :plans)"))

(defun try4 ()
  (equal :bad-input (ask-shop2 "(/ 3 0)")))

(defvar *collected* nil)

;;; These are examples for clj.
(defun test-server-put ()
  "Test server by sending 3 requests"
  (sb-thread:make-thread
   (lambda ()
     (sleep 10)
     (zmq:with-context (ctx)
      (zmq:with-socket (sender ctx :push)
      (loop for x in '(:hello :silly :world)
   	    do (sleep 1)
                (zmq:connect sender *server-get*)
	   (zmq:send sender (format nil "~S" x))))))))

(defun test-server-get ()
  "Test what comes back; should be strings of keys sent."
  (setf *collected* nil)
  (sb-thread:make-thread
   (lambda ()
     (sleep 10)
     (zmq:with-context (ctx)
       (zmq:with-socket (receiver ctx :pull)
        (zmq:connect receiver *server-put*)
        (loop for i from 1 to 3
	   do (push (zmq:recv receiver 100000) *collected*)))))))

(defun run-server-test ()
  (test-server-put)
  (test-server-get)
  (start-server)
  (sleep 20)
  *collected*)
