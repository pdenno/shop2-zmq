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

;;; These are examples for clj.
(defun test-client-write ()
  "Test server by sending 3 requests"
  (log-msg "Test-client-write starting.")
  (sb-thread:make-thread
   (lambda ()
     (sleep 5)
     (zmq:with-context (ctx)
      (zmq:with-socket (sock ctx :push)
      (loop for x in '(:hello :silly :world)
   	    do (sleep 1)
           (zmq:connect sock *endpoint*)
	   (log-msg "Test-client-write: ~A" x)
	   (zmq:send sock (format nil "~S" x))))))))

(defvar *collected* nil)

;;; bind    - create an endpoint, accept connections on socket
;;; connect - create out-going connection to an endpoint
(defun test-client-read ()
  "Test what comes back; should be strings of keys sent."
  (log-msg "Test-client-read starting.")
  (sb-thread:make-thread
   (lambda ()
     (sleep 10)
     (zmq:with-context (ctx)
       (zmq:with-socket (sock ctx :pull)
        (zmq:connect sock *endpoint*)
        (loop for i from 1 to 3
	   do (let ((got (zmq:recv sock 100000)))
		(log-msg "--->Test-client-read: ~A" got)
		(push got *collected*))))))))

(defun kill-server ()
  "Kill the SHOP2-Server"
  (let ((kt (find-if #'(lambda (x) (equal "SHOP2-Server" (sb-thread:thread-name x)))
		     (sb-thread:list-all-threads)))
	(hb (find-if #'(lambda (x) (equal "SHOP2-heartbeat" (sb-thread:thread-name x)))
		     (sb-thread:list-all-threads))))
    (when kt
      (log-msg "Killing server ~A" kt)
      (sb-thread:terminate-thread kt))
    (when hb
      (log-msg "Killing heartbeat ~A" hb)
      (sb-thread:terminate-thread hb))))

(defun diag-start-server-for-repl ()
  "Start the server that listens on *endpoint*."
  (format t "~%Starting plan server at ~A~%" *endpoint*)
  (log-msg "======Starting server.")
  (start-heart-beat)
  (sb-thread:make-thread
   (lambda () (server-loop))
   :name "SHOP2-Server"))

(defun run-server-test ()
  (setf *collected* nil)
  (setf *msgs* nil)  
  (kill-server)
  (diag-start-server-for-repl)
  (test-client-write)
  (test-client-read)
  (sleep 10)
  (kill-server)
  (format t "~% *colllected* = ~S~% *msgs* = ~S" *collected* *msgs*)
  *collected*)
