;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: mod-lisp.lisp,v 1.30 2005/10/11 14:29:22 sven Exp $
;;;;
;;;; Implementation of web server and request-response interface for mod_lisp
;;;;
;;;; Copyright (C) 2004,2005,2006 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(mod-lisp-server))

(defclass mod-lisp-server (web-app-server)
  ((keep-alive :accessor get-keep-alive :initarg :keep-alive :initform t)
   (server-process :accessor get-server-process :initform nil)
   (port :accessor get-port :initform 0)
   (client-processes :accessor get-client-processes :initform nil))
  (:documentation "mod-lisp implementation of the KPAX web application server protocol"))

(defclass mod-lisp-request-response (request-response)
  ((socket-stream :accessor get-socket-stream :initarg :socket-stream :initform nil)
   (headers :accessor get-headers :initarg :headers :initform nil)
   (parameters :initform :uninitialized))
  (:documentation "mod-lisp implementation of the KPAX web application request-response protocol"))

(defmethod init ((web-app-server mod-lisp-server) &optional options)
  (declare (ignore options))
  (call-next-method)
  web-app-server)

(defvar *restrict-mod-lisp-connections-to-localhost* t
  "When t, mod_lisp connections are only allowed to come form localhost")

(defun remote-host-is-localhost-p (client-stream)
  (equal (s-sysdeps:get-socket-stream-property client-stream :remote-host)
         "127.0.0.1"))

(defmethod startup ((web-app-server mod-lisp-server) &optional options)
  (call-next-method)
  (let ((counter 0))
    (destructuring-bind (&key (port 2001) (keep-alive t) &allow-other-keys)
        options
      (setf (get-keep-alive web-app-server) keep-alive
            (get-port web-app-server) port)
      (setf (get-server-process web-app-server)
            (s-sysdeps:start-standard-server
             :port port
             :name (format nil "mod-lisp server port ~d" port)
             :connection-handler #'(lambda (client-stream)
                                     (when (or (not *restrict-mod-lisp-connections-to-localhost*)
                                               (remote-host-is-localhost-p client-stream))
                                       (incf counter)
                                       (s-sysdeps:run-process (format nil "mod-lisp connection handler #~d" counter)
                                                              #'handle-connection
                                                              web-app-server
                                                              client-stream
                                                              counter)))))))
  web-app-server)

(defun get-request-response (stream web-app-server)
  (let ((headers (loop :for key = (read-line stream nil nil)
                       :while (and key (string-not-equal key "end"))
                       :for value = (read-line stream nil nil)
                       :collect (cons key value))))
    (if (null headers)
        nil
      (let* ((request-response (make-instance 'mod-lisp-request-response
                                              :server web-app-server
                                              :socket-stream stream
                                              :headers headers)))
        (set-standard-headers request-response)
        (setf (get-request-state request-response) :headers-read)
        request-response))))

(defmethod handle-connection ((web-app-server mod-lisp-server) client-stream id)
  (unwind-protect
      (handler-bind ((condition #'(lambda (condition)
                                    (log-debug web-app-server "handle-connection #~d encountered ~a" id condition))))
        (push (s-sysdeps:current-process) (get-client-processes web-app-server))
        (loop 
         :for request-response = (get-request-response client-stream web-app-server)
         :while request-response 
         :do (progn
               (log-debug web-app-server "handle-connection #~d dispatched ~s" id request-response)
               (handle-request-response (get-dispatcher web-app-server) request-response))
         ;; fix me: test also if the client wants to keep alive
         :while (get-keep-alive (get-server request-response))))
    (log-debug web-app-server "handle-connection #~d closing down" id)
    (close client-stream)
    (setf (get-client-processes web-app-server) 
          (remove (s-sysdeps:current-process) (get-client-processes web-app-server)))))

(defmethod shutdown ((web-app-server mod-lisp-server) &optional options)
  (destructuring-bind (&key (graceful t) &allow-other-keys)
      options
    (declare (ignore graceful))
    (with-slots (server-process client-processes) 
        web-app-server
      (dolist (process client-processes) ;; these should be removed when unwinding
        (s-sysdeps:kill-process process))
      (setf client-processes nil)
      (when server-process
        (s-sysdeps:kill-process server-process)
        (setf server-process nil)))
    (call-next-method))
  web-app-server)

(defmethod get-request-header-value ((request-response mod-lisp-request-response) header-name)
  (cdr (assoc header-name (get-headers request-response) :test #'string-equal)))

(defmethod get-request-headers ((request-response mod-lisp-request-response))
  (mapcar #'car (get-headers request-response)))

(defmethod get-uri-string ((request-response mod-lisp-request-response))
  (with-slots (uri-string)
      request-response
    (or uri-string
        (setf uri-string (puri:render-uri (get-uri request-response) nil)))))

(defmethod get-uri ((request-response mod-lisp-request-response))
  (with-slots (uri) 
      request-response
    (or uri
        (let* ((url (get-request-header-value request-response "url"))
               (question-mark-position (position #\? url))
               (host-header (get-request-header-value request-response "host"))
               ; (port-header (get-request-header-value request-response "server-ip-port"))
               (path url)
               (query nil))
          (when question-mark-position
            (setf path (subseq url 0 question-mark-position)
                  query (subseq url (1+ question-mark-position))))
          (setf uri (make-instance 'puri:uri
                                   :scheme :http
                                   :host host-header
                                   ; :port (parse-integer (or port-header "") :junk-allowed t)
                                   :path path
                                   :query query))))))

(defun write-header-line (stream key value)
  (write-string key stream)
  (write-char #\NewLine stream)
  (write-string value stream)
  (write-char #\NewLine stream))

(defun response-status-to-mod-lisp-status (status)
  (apply #'format nil "~d ~a" (response-status-translated status)))

(defmethod commit-headers ((request-response mod-lisp-request-response) &optional content-length)
  (with-slots (response-state response-status response-mime-type response-size socket-stream) 
      request-response
    (if (null response-state)
        (progn
          (setf response-size 
                content-length
                (get-response-header-value request-response "Content-Length") 
                (princ-to-string content-length)
                (get-response-header-value request-response "Content-Type") 
                response-mime-type
                (get-response-header-value request-response "Status") 
                (response-status-to-mod-lisp-status response-status)
                (get-response-header-value request-response "Keep-Socket") 
                (if (get-keep-alive (get-server request-response)) "1" "0"))
          (loop :for (header . value) :in (get-response-headers request-response)
                :do (write-header-line socket-stream header value))
          (write-string "end" socket-stream)
          (write-char #\NewLine socket-stream)
          (setf response-state :headers-written)
          (log-debug request-response "replied ~d bytes with status ~a" content-length response-status))
      (error "You can no longer commit headers since they were already committed."))))

(defmethod commit ((request-response mod-lisp-request-response))
  (with-slots (response-state content-stream socket-stream request-state) 
      request-response
    (unless (eql request-state :body-read)
      (get-request-body request-response))
    (unless (eql response-state :response-written)
      (let* ((output (when content-stream (get-output-stream-string content-stream)))
             (size (if output (length output) 0)))
        (commit-headers request-response size)
        (when (plusp size) (write-string output socket-stream))))
    (finish-output socket-stream)
    (setf response-state :response-flushed)
    (values (get-response-status-code request-response)
            (get-response-size request-response))))

(defun parse-query (query &key form-encoded)
  (mapcar #'(lambda (mapping)
              (let* ((tokens (s-utils:tokens mapping :separators (list #\=)))
                     (key (first tokens))
                     (value (second tokens)))
                (if value
                    (progn
                      (when form-encoded (setf value (substitute #\Space #\+ value)))
                      (cons key (uri-decode-for-query value)))
                  (cons key nil))))
          (s-utils:tokens query :separators (list #\&))))

(defmethod parse-parameters ((request-response mod-lisp-request-response))
  (with-slots (parameters) 
      request-response
    (when (eq parameters :uninitialized)
      (setf parameters (parse-query (puri:uri-query (get-uri request-response))
                                    :form-encoded t))
      (when (and (eq (get-request-method request-response) :post)
                 (string-equal (get-request-header-value request-response "Content-Type") 
                               "application/x-www-form-urlencoded")
                 (plusp (s-utils:parse-integer-safely (get-request-header-value request-response "Content-Length")
                                                      :default 0)))
        (setf parameters (append parameters
                                 (parse-query (get-request-body request-response)
                                              :form-encoded t)))))))

(defmethod get-request-parameter-value ((request-response mod-lisp-request-response) parameter-name)
  (parse-parameters request-response)
  (with-slots (parameters) 
      request-response
    (cdr (assoc parameter-name parameters :test #'string-equal))))

(defmethod get-request-parameter-values ((request-response mod-lisp-request-response) parameter-name)
  (parse-parameters request-response)
  (with-slots (parameters) 
      request-response
    (mapcar #'cdr 
            (remove parameter-name parameters 
                    :test-not #'string-equal :key #'car))))

(defmethod get-request-parameters ((request-response mod-lisp-request-response))
  (parse-parameters request-response)
  (with-slots (parameters) request-response
    (mapcar #'car parameters)))

(defmethod get-request-method ((request-response mod-lisp-request-response))
  (intern (string-upcase (get-request-header-value request-response "method")) :keyword))

(defmethod get-request-protocol ((request-response mod-lisp-request-response))
  (get-request-header-value request-response "server-protocol"))

(defmethod get-request-ip-address ((request-response mod-lisp-request-response))
  ;; mod-lisp stores the original client's address in a specific header
  ;; but it could be X-Forwarded-For somebody else
  ;; security note: the X-Forwarded-For header could be faked by the client,
  ;; and should be checked by requiring X-Forwarded-Host == remote-ip-address (ip/dns)
  (or (get-request-header-value request-response "X-Forwared-For")
      (get-request-header-value request-response "remote-ip-addr")))

(defmethod get-request-stream ((request-response mod-lisp-request-response))
  (call-next-method)
  (get-socket-stream request-response))  

(defmethod get-request-body ((request-response mod-lisp-request-response))
  (call-next-method)
  (with-slots (socket-stream body) request-response
    (if body
        body
      (let* ((content-length-string (get-request-header-value request-response "Content-Length"))
             (content-length (s-utils:parse-integer-safely content-length-string))
             (content-string (when content-length 
                               (make-string content-length))))
        (when content-string
          (read-sequence content-string socket-stream)
          (setf body content-string))))))

(defmethod get-response-stream ((request-response mod-lisp-request-response))
  (call-next-method)
  (get-socket-stream request-response))

(defmethod get-cookie ((request-response mod-lisp-request-response) name)
  (let* ((cookie-header (get-request-header-value request-response "Cookie"))
         (cookies (mapcar #'(lambda (mapping)
                              (s-utils:tokens mapping :separators (list #\=)))
                          (s-utils:tokens cookie-header :separators (list #\; #\space))))
         (cookie-value (second (assoc name cookies :test #'string-equal))))
    (if cookie-value
        (uri-decode-for-query cookie-value)
      nil)))

(defmethod set-cookie ((request-response mod-lisp-request-response) name value path &optional expires)
  (add-response-header-value request-response 
                             "Set-Cookie" 
                             (format nil 
                                     "~a=~a; path=~a~@[; expires=~a~]" 
                                     name 
                                     (uri-encode-for-query value) 
                                     path 
                                     (ecase expires ((:session nil) nil) (:never *far-in-the-future*)))))

;;;; eof
