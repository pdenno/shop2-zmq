(in-package :s2zmq)

(defparameter *endpoint* "tcp://127.0.0.1:31726")

(defun ask-shop2 (request-str)
  "Eval request string in :shop2-user package."
  (handler-case 
      (let* ((*package* (find-package :shop2-user))
	     (form (read-from-string request-str)))
	(eval form))
    (serious-condition () :bad-input)))

;;; It seems to me that the meaning of bind and serve isn't accurately communicated in the documentation. 
;;; What really matters is the messaging pattern used. (e.g. :rep, :req, :pair).
;;; You can do both send and recv with server/client doing respectively bind/connect with :rep/:req.
(defun server-loop ()
  (handler-case
      (zmq:with-context (ctx)
	(zmq:with-socket (sock ctx :rep)
	  (zmq:bind sock *endpoint*)
	  (start-heart-beat sock)
	  (loop
	     (let ((got (zmq:recv sock 100000))) ; block, waiting for input. 2nd arg is max size. 
	       (log-msg "Server receives: ~S" got)
	       (let ((result (ask-shop2 got)))
		 (log-msg (format nil "Server replying: ~A" (fmt-msg result)) result)
		 (zmq:send sock (format nil (fmt-msg result) result)))))))
    (serious-condition (c)
      (log-msg "Server stopping: ~A" c))))

(defun fmt-msg (msg)
  "Return a format string for a message"
  (cond ((stringp msg) "~S")
	((keywordp msg) "~S")
	(t "~A")))

(defun start-server ()
  "Start the server that listens on *endpoint*."
  (format t "~%Starting plan server at ~A~%" *endpoint*)
  (log-msg "======Starting server.")
  (server-loop))

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

(defun start-heart-beat (sock)
  (sb-thread:make-thread
   (lambda ()
     (loop 
	(log-msg "Alive.")
	(zmq:send sock ":Alive")
	(sleep 120)))
   :name "SHOP2-heartbeat"))
