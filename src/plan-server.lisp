(in-package :s2zmq)

;(defparameter *server-put* "tcp://127.0.0.1:31727")
(defparameter *endpoint* "tcp://127.0.0.1:31726")

(defun ask-shop2 (request-str)
  "Eval request string in :shop2-user package."
  (handler-case 
      (let* ((*package* (find-package :shop2-user))
	     (form (read-from-string request-str)))
	(eval form))
    (serious-condition () :bad-input)))

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

(defun log-msg (fmt &rest args)
  (let ((msg (apply #'format nil fmt args)))
    (multiple-value-bind
	  (second minute hour date month year day-of-week dst-p tz)
	(decode-universal-time (get-universal-time))
      (declare (ignore day-of-week dst-p tz))
      (with-open-file (out "/usr/local/tmp/plan-log.txt"
			   :if-does-not-exist :create
			   :direction :output :if-exists :append)
	(format out "~%~A-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d: ~A"
		year month date hour minute second msg)))))

(defun start-heart-beat ()
  (sb-thread:make-thread
   (lambda ()
     (loop 
	(log-msg "Alive.")
	(sleep 60)))
   :name "SHOP2-heartbeat"))

(defvar *msgs* nil)

;;; bind    - create an endpoint, accept connections on socket
;;; connect - create out-going connection to an endpoint
(defun start-server-old ()
  "Start the server that listens on *endpoint*."
  (format t "~%Starting plan server at ~A~%" *endpoint*)
  (log-msg "======Starting server.")
  (start-heart-beat)
  (sb-thread:make-thread
   (lambda ()
     (handler-case
	 (zmq:with-context (ctx)
	   (log-msg "...context.")
	   (zmq:with-socket (sock ctx :pull)
	     (log-msg "...socket.")
	     (zmq:bind sock *endpoint*)
	     (log-msg "...bind.")
	     (zmq:connect sock *endpoint*)
	     (log-msg "Server bind and connect.")
	     (loop
		(let ((got (zmq:recv sock 100000))) ; block, waiting for input.
		  (log-msg "Server receives: ~A" got)
		  (let ((result (ask-shop2 got)))
		    (log-msg "Server replying: ~A" result)
		    (push result *msgs*) ; more diagnostics
		    (zmq:send sock (format nil "'~S" result))))))) ; Note: quoted.
       (condition (c)
	 (log-msg "Server stopping: ~A" c))))
   :name "SHOP2-Server"))

(defun start-server ()
  "Start the server that listens on *endpoint*."
  (format t "~%Starting plan server at ~A~%" *endpoint*)
  (log-msg "======Starting server.")
  (start-heart-beat)
  (handler-case
      (zmq:with-context (ctx)
	(log-msg "...context.")
	(zmq:with-socket (sock ctx :pull)
	  (log-msg "...socket.")
	  (zmq:bind sock *endpoint*)
	  (log-msg "...bind.")
	  (zmq:connect sock *endpoint*)
	  (log-msg "Server bind and connect.")
	  (loop
	     (let ((got (zmq:recv sock 100000))) ; block, waiting for input.
	       (log-msg "Server receives: ~A" got)
	       (let ((result (ask-shop2 got)))
		 (log-msg "Server replying: ~A" result)
		 (push result *msgs*) ; more diagnostics
		 (zmq:send sock (format nil "'~S" result))))))) ; Note: quoted.
    (condition (c)
      (log-msg "Server stopping: ~A" c))))


