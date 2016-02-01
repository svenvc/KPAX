;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: mod-lisp.lisp,v 1.30 2005/10/11 14:29:22 sven Exp $
;;;;
;;;; Implementation of web server and request-response interface using the S-HTTP-SERVER
;;;;
;;;; Copyright (C) 2005-2009 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(s-http-server
   get-s-http-server
   get-s-http-server-request))

;; Objects representing the bridge between KPAX and S-HTTP-SERVER

(defclass s-http-server (web-app-server)
  ((s-http-server :reader get-s-http-server :initarg :s-http-server :initform (s-http-server:make-s-http-server)))
  (:documentation "S-HTTP-SERVER implementation of the KPAX web application server protocol"))

(defgeneric get-s-http-server (server)
  (:documentation "Access the S-HTTP-SERVER object inside server"))

(defclass s-http-server-request-response (request-response)
  ((s-http-server-request :accessor get-s-http-server-request :initarg :s-http-server-request :initform nil)
   (socket-stream :accessor get-socket-stream :initarg :socket-stream :initform nil)
   (parameters :initform :uninitialized))
  (:documentation "S-HTTP-SERVER implementation of the KPAX web application request-response protocol"))

(defmethod get-port ((web-app-server s-http-server))
  (s-http-server:get-port (get-s-http-server web-app-server)))

;; initialization, startup and shutdown protocol

(defmethod init ((web-app-server s-http-server) &optional options)
  (declare (ignore options))
  (call-next-method)
  (flet ((s-http-server-handler (s-http-server handler http-request stream)
           (declare (ignore handler))
           (s-http-server:logm s-http-server :debug "Running KPAX s-http-server-handler")
           (let ((request-response (make-instance 's-http-server-request-response
                                                  :server web-app-server
                                                  :s-http-server-request http-request
                                                  :socket-stream stream)))
             (set-standard-headers request-response)
             (setf (get-request-state request-response) :headers-read)
             (multiple-value-bind (status size)
                 (handle-request-response (get-dispatcher web-app-server)
                                          request-response)
               (values t status size)))))
    (s-http-server:register-context-handler (get-s-http-server web-app-server)
                                            (get-dynamic-prefix web-app-server)
                                            #'s-http-server-handler)
    (setf (s-http-server:get-log-stream (get-s-http-server web-app-server)) nil
          (s-http-server:get-access-log-stream (get-s-http-server web-app-server)) nil))
  web-app-server)

(defmethod startup ((web-app-server s-http-server) &optional options)
  (call-next-method)
  (destructuring-bind (&key (port 2001) &allow-other-keys)
      options
    (let ((s-http-server (get-s-http-server web-app-server)))
      (setf (s-http-server:get-port s-http-server) port)
      (s-http-server:start-server s-http-server)))
  web-app-server)

(defmethod shutdown ((web-app-server s-http-server) &optional options)
  (destructuring-bind (&key (graceful t) &allow-other-keys)
      options
    (declare (ignore graceful))
    (s-http-server:stop-server (get-s-http-server web-app-server)))
  (call-next-method)
  web-app-server)

;; request-response protocol

(defmethod get-uri ((request-response s-http-server-request-response))
  (s-http-server:get-uri (get-s-http-server-request request-response)))

(defmethod get-uri-string ((request-response s-http-server-request-response))
  (with-slots (uri-string)
      request-response
    (or uri-string
        (setf uri-string (puri:render-uri (get-uri request-response) nil)))))

(defmethod get-request-method ((request-response s-http-server-request-response))
  (s-http-server:get-method (get-s-http-server-request request-response)))

(defmethod get-request-protocol ((request-response s-http-server-request-response))
  (s-http-server:get-http-version (get-s-http-server-request request-response)))

(defmethod get-request-ip-address ((request-response s-http-server-request-response))
  "If we are behind a proxy, use X-Forwarded-For if set"
  (or (get-request-header-value request-response "X-Forwarded-For")
      (s-sysdeps:get-socket-stream-property (get-socket-stream request-response) :remote-host)))

(defmethod get-request-header-value ((request-response s-http-server-request-response) header-name)
  (cdr (assoc header-name (s-http-server:get-headers (get-s-http-server-request request-response)) 
              :test #'string-equal)))

(defmethod get-request-headers ((request-response s-http-server-request-response))
  (mapcar #'car (s-http-server:get-headers (get-s-http-server-request request-response))))

(defmethod get-request-parameter-value ((request-response s-http-server-request-response) parameter-name)
  (parse-parameters request-response)
  (with-slots (parameters) request-response
    (cdr (assoc parameter-name parameters :test #'string-equal))))

(defmethod get-request-parameter-values ((request-response s-http-server-request-response) parameter-name)
  (parse-parameters request-response)
  (with-slots (parameters) request-response
    (mapcar #'cdr 
            (remove parameter-name parameters 
                    :test-not #'string-equal :key #'car))))

(defmethod get-request-parameters ((request-response s-http-server-request-response))
  (parse-parameters request-response)
  (with-slots (parameters) request-response
    (mapcar #'car parameters)))

(defmethod get-request-stream ((request-response s-http-server-request-response))
  (call-next-method)
  (get-socket-stream request-response))  

(defmethod get-request-body ((request-response s-http-server-request-response))
  (call-next-method)
  (with-slots (socket-stream body) request-response
    (if body
        body
      (let* ((content-length (get-request-header-value request-response "Content-Length"))
             (buffer (when content-length
                       (make-string (s-utils:parse-integer-safely content-length)))))
        (when buffer
          (read-sequence buffer socket-stream)
          (setf body buffer))))))

(defmethod get-response-stream ((request-response s-http-server-request-response))
  (call-next-method)
  (get-socket-stream request-response))

(defmethod get-cookie ((request-response s-http-server-request-response) name)
  (let* ((cookie-header (get-request-header-value request-response "Cookie"))
         (cookies (mapcar #'(lambda (mapping)
                              (s-utils:tokens mapping :separators (list #\=)))
                          (s-utils:tokens cookie-header :separators (list #\; #\space))))
         (cookie-value (second (assoc name cookies :test #'string-equal))))
    (if cookie-value
        (uri-decode-for-query cookie-value)
      nil)))

(defmethod set-cookie ((request-response s-http-server-request-response) name value path &optional expires)
  (add-response-header-value request-response 
                             "Set-Cookie" 
                             (format nil 
                                     "~a=~a; path=~a~@[; expires=~a~]" 
                                     name 
                                     (uri-encode-for-query value) 
                                     path 
                                     (ecase expires ((:session nil) nil) (:never *far-in-the-future*)))))

(defmethod start-web-app-internal ((web-app-server s-http-server) (web-app web-app) options)
  (declare (ignore options))
  (when (get-static-root web-app)
    (s-http-server:register-context-handler (get-s-http-server web-app-server)
                                            (get-complete-static-prefix :server web-app web-app-server)
                                            's-http-server:static-resource-handler
                                            :arguments (list (get-static-root web-app)))))

(defmethod stop-web-app-internal ((web-app-server s-http-server) (web-app web-app) options)
  (declare (ignore options))
  (when (get-static-root web-app)
    (s-http-server:unregister-context-handler (get-s-http-server web-app-server)
                                              (get-complete-static-prefix :server web-app web-app-server))))

(defmethod commit-headers ((request-response s-http-server-request-response) &optional content-length)
  (with-slots (response-state response-status response-mime-type response-size socket-stream) 
      request-response
    (if (null response-state)
        (let ((s-http-server-status (response-status-translated response-status))
              (request-procotol (get-request-protocol request-response)))
          (when content-length
            (setf response-size 
                  content-length
                  (get-response-header-value request-response "Content-Length") 
                  (princ-to-string content-length)))
          (setf (get-response-header-value request-response "Content-Type") 
                response-mime-type)
          (s-http-server:write-http-response-status-line socket-stream 
                                                         (first s-http-server-status) 
                                                         (second s-http-server-status)
                                                         request-procotol)
          (s-http-server:write-http-response-headers (get-response-headers request-response)
                                                     socket-stream)
          (let ((keep-alive (s-http-server:get-keep-alive (get-s-http-server-request request-response))))
            (when (and (equal request-procotol "HTTP/1.0") keep-alive)
              (s-http-server:write-http-response-headers `(("Connection" . "Keep-Alive")) socket-stream))
            (when (and (equal request-procotol "HTTP/1.1") (not keep-alive))
              (s-http-server:write-http-response-headers `(("Connection" . "Close")) socket-stream)))
          (s-http-server:write-http-response-line "" socket-stream)
          (setf response-state :headers-written)
          (log-debug request-response "replied ~d bytes with status ~a" content-length response-status))
      (error "You can no longer commit headers since they were already committed."))))

(defun s-http-server-commit-contents-handler (contents-string socket-stream request-response)
  (declare (ignore request-response))
  (write-string contents-string socket-stream))

(defun s-http-server-commit-length-handler (contents-string request-response)
  (declare (ignore request-response))
  (length contents-string))

(defmethod commit ((request-response s-http-server-request-response))
  (with-slots (response-state content-stream socket-stream request-state) 
      request-response
    (unless (eql request-state :body-read)
      (get-request-body request-response))
    (unless (eql response-state :response-written)
      (let* ((output (when content-stream (get-output-stream-string content-stream)))
             (commit-contents-handler (or (get-attribute request-response :commit-contents-handler)
                                          's-http-server-commit-contents-handler))
             (commit-length-handler (or (get-attribute request-response :commit-length-handler)
                                        's-http-server-commit-length-handler))
             (size (if output (funcall commit-length-handler output request-response) 0)))
        (commit-headers request-response size)
        (when (plusp size) (funcall commit-contents-handler output socket-stream request-response))))
    (finish-output socket-stream)
    (setf response-state :response-flushed)
    (values (get-response-status-code request-response)
            (get-response-size request-response))))

;; support

(defun s-http-server-parse-query-string (query)
  (mapcar #'(lambda (mapping)
              (let* ((tokens (s-utils:tokens mapping :separators (list #\=)))
                     (key (first tokens))
                     (value (second tokens)))
                (if value
                    (cons key (substitute #\Space #\+ value))
                  (cons key nil))))
          (s-utils:tokens query :separators (list #\&))))

(defun s-http-server-decode-parameter-handler (parameter-string request-response)
  (declare (ignore request-response))
  (uri-decode-for-query parameter-string))

(defmethod parse-parameters ((request-response s-http-server-request-response))
  (with-slots (parameters) 
      request-response
    (when (eq parameters :uninitialized)
      (setf parameters (s-http-server-parse-query-string (puri:uri-query (get-uri request-response))))
      (when (and (eq (get-request-method request-response) :post)
                 (string-equal (get-request-header-value request-response "Content-Type") 
                               "application/x-www-form-urlencoded")
                 (plusp (s-utils:parse-integer-safely (get-request-header-value request-response "Content-Length") 
                                                      :default 0)))
        (setf parameters (append parameters
                                 (s-http-server-parse-query-string (get-request-body request-response)))))
      (let ((decode-parameter-handler (or (get-attribute request-response :decode-parameter-handler)
                                          's-http-server-decode-parameter-handler)))
        (loop :for binding :in parameters :do 
              (setf (car binding)
                    (funcall decode-parameter-handler (car binding) request-response))
              (when (cdr binding)
                (setf (cdr binding) 
                      (funcall decode-parameter-handler (cdr binding) request-response))))))))

;;;; eof
