;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: request-response.lisp,v 1.31 2005/04/08 09:13:00 sven Exp $
;;;;
;;;; Superclass of all request-response objects, defining part of the server interface
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(request-response
   get-server
   get-application
   get-session
   get-uri
   get-uri-string
   get-request-state
   get-response-state
   get-request-sub-path
   get-request-header-value
   get-request-headers
   get-request-parameter-value
   get-request-parameter-values
   get-request-parameters
   get-request-stream
   get-request-body
   get-request-method
   get-request-protocol
   get-request-ip-address
   get-response-stream
   get-content-stream
   get-response-header-value
   get-response-status
   get-response-mime-type
   add-response-header-value
   get-cookie
   set-cookie
   commit-headers
   static-url
   dynamic-url))

(defclass request-response (attributes-mixin)
  ((uri-string :initform nil)
   (uri :initform nil)
   (server :accessor get-server :initarg :server :initform nil)
   (application :accessor get-application :initform nil)
   (session :accessor get-session :initform nil)
   (sub-path :accessor get-request-sub-path :initform nil)
   (request-state :accessor get-request-state :initform nil)
   (response-state :accessor get-response-state :initform nil)
   (response-status :accessor get-response-status :initform :ok)
   (response-mime-type :accessor get-response-mime-type :initform "text/html")
   (response-size :accessor get-response-size :initform 0)
   (response-headers :accessor get-response-headers :initform nil)
   (body :initform nil)
   (content-stream :initform nil))
  (:documentation "I represent a request-response from a server"))

(defgeneric get-application (request-response)
  (:documentation "Get the web application to which this request-response was directed"))

(defgeneric get-server (request-response)
  (:documentation "Get the web application server handling this request-response"))

(defgeneric get-session (request-response)
  (:documentation "Get the session bound to this request-response"))

(defgeneric get-request-state (request-response)
  (:documentation "One of - in order - nil, :request-line-read, :headers-read or :body-read"))

(defgeneric get-response-state (request-response)
  (:documentation "One of - in order - nil, :response-line-written, :headers-written, :response-written or :response-flushed"))

(defgeneric get-response-stream (request-response)
  (:documentation "Access the raw stream to write content to for this request-response (will commit headers if necessary)")
  (:method ((request-response request-response))
   (with-slots (content-stream response-state) 
       request-response
     ;; this method will fail unless that state is correct, but it won't do anything (subclass responsability)
     (cond ((member response-state '(:response-written :response-flushed))
            (error "You can no longer access the raw response stream since the response was already written"))
           (content-stream
            (error "You can no longer access the raw response stream since a buffered content-stream is in use"))
           ((member response-state '(nil :response-line-written :headers-written))
            (unless (eql response-state :headers-written)
              (commit-headers request-response))
            (setf response-state :response-written)
            t)))))

(defgeneric get-content-stream (request-response)
  (:documentation "Access the buffered stream to write content to for this request-response's reply")
  (:method ((request-response request-response))
   (with-slots (content-stream response-state) 
       request-response
     (if (null response-state)
         (or content-stream (setf content-stream (make-string-output-stream)))
       (error "You can no longer use a buffered content stream since ~a"
              (ecase response-state
                (:headers-written "headers were already written")
                (:response-written "the response was already written")
                (:response-flushed "the response was already flushed")))))))

(defgeneric (setf get-content-stream) (string-stream request-response)
  (:documentation "Set the buffered stream to write content to for this request-response's reply")
  (:method ((string-stream string-stream) (request-response request-response))
   (with-slots (content-stream response-state) 
       request-response
     (if content-stream
         (error "Content stream was already set")
       (if (null response-state)
           (setf content-stream string-stream)
         (error "You can no longer use a buffered content stream since ~a"
                (ecase response-state
                  (:headers-written "headers were already written")
                  (:response-written "the response was already written")
                  (:response-flushed "the response was already flushed"))))))))

(defgeneric get-response-mime-type (request-response)
  (:documentation "Get the mime-type for this request-response's reply"))

(defgeneric get-response-status (request-response)
  (:documentation "Get the statuc for this request-response's reply (:ok, :created, :not-found, :error or :moved)"))

(defgeneric get-uri-string (request-response)
  (:documentation "Get the URI string underlying this request-response"))

(defgeneric get-uri (request-response)
  (:documentation "Get the URI underlying this request-response")
  (:method ((request-response request-response))
   (with-slots (uri) request-response
     (or uri
         (setf uri (puri:parse-uri (get-uri-string request-response)))))))

(defgeneric get-request-stream (request-response)
  (:documentation "Get the raw request stream from request-response (fails if get-request-body was done first)")
  (:method ((request-response request-response))
   (with-slots (request-state) request-response
     ;; this method will fail unless that state is correct, but it won't do anything (subclass responsability)
     (if (null request-state)
         (error "You cannot yet access the raw request stream since no request line and headers were parsed yet") 
       (ecase request-state
         (:request-line-read (error "You cannot yet access the raw request stream since no headers were parsed yet"))
         (:headers-read (setf request-state :body-read) t)
         (:body-read (error "You can no longer access the raw request stream since the body was already read")))))))

(defgeneric get-request-body (request-response)
  (:documentation "Get the request data, 'body', from request-response as a (possibly empty) string")
  (:method ((request-response request-response))
   (with-slots (request-state body) request-response
     ;; this method will fail unless that state is correct, but it won't do anything (subclass responsability)
     (if (null request-state)
         (error "You cannot yet access the request body since no request line and headers were parsed yet") 
       (ecase request-state
         (:request-line-read (error "You cannot yet access the request body since no headers were parsed yet"))
         (:headers-read (setf request-state :body-read) t)
         (:body-read body))))))

(defgeneric get-request-method (request-response)
  (:documentation "Get the request HTTP method, a keyword symbol most likely :get or :post (but extendible)"))

(defgeneric get-request-protocol (request-response)
  (:documentation "Get the request HTTP protocol, a string like 'HTTP/1.1'"))

(defgeneric get-request-header-value (request-response header-name)
  (:documentation "Get the request header value for header-name in request-response"))

(defgeneric get-request-headers (request-response)
  (:documentation "Get the list of all header names in request-response"))

(defgeneric get-request-parameter-value (request-response parameter-name)
  (:documentation "Get the first request parameter value for parameter-name in request-response"))

(defgeneric get-request-parameter-values (request-response parameter-name)
  (:documentation "Get the list of request parameter values for parameter-name in request-response"))

(defgeneric get-request-parameters (request-response)
  (:documentation "Get the list of all request parameter names in request-response"))

(defgeneric get-request-ip-address (request-response)
  (:documentation "Get the ip address of the client of request-response as a dotted string"))

(defgeneric get-request-sub-path (request-response)
  (:documentation "Get the sub path list for this request (the uri components minus the server and web app prefixes)"))

(defgeneric get-response-header-value (request-response header-name)
  (:documentation "Get the (first) response header value for header-name in request-response")
  (:method ((request-response request-response) header-name)
   (cdr (assoc header-name (get-response-headers request-response) :test #'string-equal))))

(defgeneric get-response-header-values (request-response header-name)
  (:documentation "Get a list of all the response header values for header-name in request-response")
  (:method ((request-response request-response) header-name)
   (mapcar #'cdr (remove-if-not #'(lambda (header-name-value) (string-equal (car header-name-value) header-name))
                                (get-response-headers request-response)))))
 
(defgeneric (setf get-response-header-value) (value request-response header-name)
  (:documentation "Set the response header value for header-name in request-response to value")
  (:method (value (request-response request-response) header-name)
   (when (member (get-response-state request-response) '(:headers-written :response-written :response-flushed))
     (error "You can no longer set response headers since the headers were already committed")) 
   (let ((pair (assoc header-name (get-response-headers request-response) :test #'string-equal)))
     (if pair
         (setf (cdr pair) value)
       (push (cons header-name value) (get-response-headers request-response)))
     value)))

(defgeneric add-response-header-value (request-response header-name value)
  (:documentation "Add a response header named header-name equal to value to request-response")
  (:method ((request-response request-response) header-name value)
   (when (member (get-response-state request-response) '(:headers-written :response-written :response-flushed))
     (error "You can no longer set response headers since the headers were already committed"))
   (push (cons header-name value) (get-response-headers request-response))))

(defun response-date (&optional (universal-time (get-universal-time)))
  "Generate a GMT HTTP Response Date"
  (s-utils:format-universal-time universal-time
                                 :format '(:day-name ", " :date2 #\Space :month-name #\Space :year #\Space 
                                           :hour #\: :minute #\: :second " GMT")
                                 :decode-in-timezone 0))

(defun server-identification (request-response)
  (format nil "KPAX/~a ~a ~a" 
          (class-name (class-of (get-server request-response)))
          (lisp-implementation-type) (lisp-implementation-version)))

(defgeneric set-standard-headers (request-response)
  (:documentation "Set some standard response headers")
  (:method ((request-response request-response))
   (setf (get-response-header-value request-response "Server")
         (server-identification request-response)
         (get-response-header-value request-response "Date")
         (response-date))))

(defgeneric setup-for-web-app (request-response web-app sub-path)
  (:documentation "Setup the request-response in the context of a web-app")
  (:method ((request-response request-response) web-app sub-path)
   (setf (get-application request-response) web-app
         (get-request-sub-path request-response) sub-path
         (get-attribute request-response :dispatch-timestamp) (get-internal-real-time))
   (let ((request-setup-hook (get-request-setup-hook web-app)))
     (when request-setup-hook (funcall request-setup-hook request-response)))))

(defgeneric commit-headers (request-response &optional content-length)
  (:documentation "Partially commmit the request-response: response header line and response headers"))

(defgeneric commit (request-response)
  (:documentation "Commit the whole request-response: response header line, response headers and any buffered content"))

(defmethod print-object ((request-response request-response) stream)
  (print-unreadable-object (request-response stream :type t :identity t)
    (format stream "~s" (get-uri request-response))))

(defmethod reset-response ((request-response request-response))
  ;; XXX fix to take new [request|response]-state into account
  (setf (get-response-status request-response) :ok
        (get-response-mime-type request-response) "text/html"
        (get-response-headers request-response) nil)
  ;; read the output from the string stream to reset it
  (get-output-stream-string (get-content-stream request-response)))

(defgeneric static-url (request-response scope relative-url &rest args)
  (:documentation "Generate a static URL in the scope (:server :webapp) of request-response using relative-url and args")
  (:method ((request-response request-response) scope relative-url &rest args)
   (let* ((web-app (get-application request-response))
          (server (get-server request-response))
          (prefix (get-complete-static-prefix scope web-app server))
          (static-url-hook (when web-app (get-option web-app :static-url-hook))))
     (when static-url-hook 
       (setf prefix (funcall static-url-hook prefix)))
     (format nil "~a~?" prefix relative-url args))))

(defun write-keyword-value-list (keyword-value-list out)
  (when keyword-value-list
    (let ((key (car keyword-value-list))
          (value (cadr keyword-value-list))
          (tail (cddr keyword-value-list)))
      (write key :stream out :case :downcase :escape nil)
      (write-char #\= out)
      (write-string (uri-encode-for-query (princ-to-string value) :signal-errors nil) out)
      (when tail
        (write-char #\& out)
        (write-keyword-value-list tail out)))))

(defgeneric dynamic-url (request-response relative-url &rest args)
  (:documentation "Generate an escaped dynamic URL in the context of request-response based on relative-url and args")
  (:method ((request-response request-response) relative-url &rest args)
   (escape-string
    (apply #'dynamic-unescaped-url request-response relative-url args))))

(defgeneric dynamic-unescaped-url (request-response relative-url &rest args)
  (:documentation "Generate a dynamic URL in the context of request-response based on relative-url and args")
  (:method ((request-response request-response) relative-url &rest args)
   (let ((prefix (get-complete-dynamic-prefix (get-application request-response)
                                              (get-server request-response))))
     (with-output-to-string (out)
       (write-string prefix out)
       (append-session-id request-response out)
       (cond ((null relative-url))
             ((stringp relative-url) (format out "~?" relative-url args))
             ((symbolp relative-url) 
              (write relative-url :stream out :case :downcase :escape nil)
              (when args
                (write-char #\? out)
                (write-keyword-value-list args out)))
             (t (error "unknown relative-url type ~s" relative-url)))))))

(defmethod logm ((request-response request-response) level format-string &rest args)
  (apply #'logm (get-server request-response) level format-string args))

(defgeneric get-cookie (request-response name)
  (:documentation "Return the most specific string value of the cookie with name in request-response"))

(defgeneric set-cookie (request-response name value path &optional expires)
  (:documentation "Set the code with name and value for path in request-response (expires is :session or :never)"))

;; support

(defun response-status-translated (status)
  "Translate a symbol denoting a response state into a pair (<int> <string) (<code> <reason>)"
  (case status
    (:ok '(200 "OK"))
    (:created '(201 "Created"))
    (:unauthorized '(401 "Unauthorized"))
    (:not-found '(404 "Not Found"))
    (:error '(500 "KPAX Server Error"))
    (:moved '(301 "Moved Permanently"))
    (t '(500 "Unknown"))))

(defmethod get-response-status-code (request-response)
  (first (response-status-translated (get-response-status request-response))))

;;;; eof