(in-package :s2zmq)

(defparameter *server-put* "tcp://127.0.0.1:31728") ; Server
(defparameter *server-get* "tcp://127.0.0.1:31727") ; Server

(defun ask-shop2 (request-str)
  "Eval request string in :shop2-user package."
  (handler-case 
      (let* ((*package* (find-package :shop2-user))
	     (form (read-from-string request-str))) ; zmq:recv is blocking
	(eval form))
    (serious-condition () :bad-input)))

(defvar *last-msg* nil)
(defvar *loop-count* 0)

(defun kill-server ()
  "Kill the SHOP2-Server"
  (let ((kt (find-if #'(lambda (x) (equal "SHOP2-Server" (sb-thread:thread-name x)))
		     (sb-thread:list-all-threads))))
    (when kt
      (sb-thread:terminate-thread kt))))

;;; bind    - create an endpoint, accept connections on socket (to send).
;;; connect - connect to an endpoint                           (to receive).
(defun start-server ()
  "Start the server that listens on *server-get*."
  (sb-thread:make-thread
   (lambda ()
     (zmq:with-context (ctx)
       (zmq:with-socket (listen-client ctx :pull)
        (zmq:with-socket (respond-client ctx :push)
	 (zmq:bind respond-client *server-put*)
	 (zmq:bind listen-client  *server-get*)
	 (loop
	    (let* ((got (zmq:recv listen-client 100000)) ; block, waiting for input. NOPE!
	           (result (ask-shop2 got))) ; block while planning
	      (zmq:send respond-client (format nil "~S" result))))))))
     :name "SHOP2-Server"))

  





     
		      
	      

       
  




