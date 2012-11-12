;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: paserve.lisp,v 1.25 2005/05/31 08:38:05 sven Exp $
;;;;
;;;; Implementation of web server and request-response interface for Portable Allegro Serve
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(paserve))

(defclass paserve (web-app-server)
  ((wserver :accessor get-wserver :initarg :wserver :initform (make-instance 'net.aserve:wserver))
   (publish-static-files-p :accessor get-publish-static-files-p :initarg :publish-static-files-p :initform t))
  (:documentation "Portable allegro serve implementation of the KPAX web application server protocol"))

(defclass paserve-request-response (request-response)
  ((paserve-request :accessor get-paserve-request :initarg :paserve-request :initform nil)
   (paserve-entity :accessor get-paserve-entity :initarg :paserve-entity :initform nil))
  (:documentation "Portable allegro serve implementation of the KPAX web application request-response protocol"))

(defmethod init ((web-app-server paserve) &optional options)
  (declare (ignore options))
  (call-next-method)
  (flet ((paserve-handler (request entity)
           (let ((request-response (make-instance 'paserve-request-response
                                                  :server web-app-server
                                                  :paserve-request request
                                                  :paserve-entity entity)))
             (set-standard-headers request-response)
             (handle-request-response (get-dispatcher web-app-server)
                                      request-response))))
    (net.aserve:publish-prefix :server (get-wserver web-app-server)
                               :prefix (get-dynamic-prefix web-app-server) 
                               :function #'paserve-handler))
  web-app-server)

(defmethod startup ((web-app-server paserve) &optional options)
  (call-next-method)
  (destructuring-bind (&key (port 2001) (listeners 5) (keep-alive t) (chunking nil) &allow-other-keys)
      options
    (net.aserve:start :server (get-wserver web-app-server)
                      :port port
                      :listeners listeners
                      :chunking chunking
                      :keep-alive keep-alive))
  web-app-server)

(defmethod shutdown ((web-app-server paserve) &optional options)
  (destructuring-bind (&key (graceful t) &allow-other-keys)
      options
    (declare (ignore graceful))
    (net.aserve:shutdown :server (get-wserver web-app-server)))
  (call-next-method)
  web-app-server)

(defmethod get-port ((web-app-server paserve))
  (acl-compat.socket:local-port (net.aserve:wserver-socket (get-wserver web-app-server))))

(defmethod get-uri-string ((request-response paserve-request-response))
  (with-slots (uri-string)
      request-response
    (or uri-string
        ;; better go from uri to puri directly instead of rendering/parsing!
        (setf uri-string (net.uri:render-uri (net.aserve:request-uri (get-paserve-request request-response)) 
                                             nil)))))

(defmethod get-request-body ((request-response paserve-request-response))
  (net.aserve:get-request-body (get-paserve-request request-response)))

(defmethod get-request-method ((request-response paserve-request-response))
  (net.aserve:request-method (get-paserve-request request-response)))

(defmethod get-request-protocol ((request-response paserve-request-response))
  (net.aserve:request-protocol-string (get-paserve-request request-response)))

(defmethod get-request-ip-address ((request-response paserve-request-response))
  (let ((socket (net.aserve:request-socket (get-paserve-request request-response))))
    (when socket
      (acl-compat.socket:ipaddr-to-dotted (acl-compat.socket:remote-host socket)))))

(defmethod start-web-app-internal ((web-app-server paserve) (web-app web-app) options)
  (declare (ignore options))
  (when (and (get-static-root web-app)
             (get-publish-static-files-p web-app-server))
    (net.aserve:publish-directory :server (get-wserver web-app-server)
                                  :prefix (get-complete-static-prefix :server web-app web-app-server)
                                  :destination (namestring (truename (get-static-root web-app))))))

(defmethod stop-web-app-internal ((web-app-server paserve) (web-app web-app) options)
  (declare (ignore options))
  (when (and (get-static-root web-app)
             (get-publish-static-files-p web-app-server))
    (net.aserve:publish-directory :server (get-wserver web-app-server)
                                  :prefix (get-complete-static-prefix :server web-app web-app-server) 
                                  :remove t)))

(defun response-status-to-paserve-status (status)
  (case status
    (:ok net.aserve:*response-ok*)
    (:created net.aserve:*response-created*)
    (:error net.aserve:*response-internal-server-error*)
    (:unauthorized net.aserve:*response-unauthorized*)
    (:not-found net.aserve:*response-not-found*)
    (:moved net.aserve:*response-moved-permanently*)
    (t net.aserve:*response-internal-server-error*)))

(defmethod commit-headers ((request-response paserve-request-response) &optional content-length)
  (error "Committing the headers seperately is currently not supported under portable allegro serve"))

(defmethod commit ((request-response paserve-request-response))
  (with-slots (paserve-request paserve-entity content-stream 
               response-status response-mime-type response-size response-headers) 
      request-response
    (net.aserve:with-http-response 
        (paserve-request paserve-entity 
                         :content-type response-mime-type 
                         :response (response-status-to-paserve-status response-status))
      (net.aserve:with-http-body
          (paserve-request paserve-entity :headers response-headers)
        (let* ((output (when content-stream (get-output-stream-string content-stream)))
               (size (if output (length output) 0)))
          (setf (net.aserve:request-reply-content-length paserve-request) size
                response-size size)
          (when (plusp size) (write-string output net.aserve::*html-stream*))
          (log-debug request-response "replied ~d bytes with status ~a" size response-status)
          (values (get-response-status-code request-response)
                  size))))))

(defmethod get-request-header-value ((request-response paserve-request-response) header-name)
  (net.aserve:header-slot-value (get-paserve-request request-response)
                                (intern header-name :keyword)))

(defmethod get-request-headers ((request-response paserve-request-response))
  (let ((paserve-request (get-paserve-request request-response))
        all-headers)
    (flet ((add-headers (headers)
             (dolist (header headers)
               (when (net.aserve:header-slot-value paserve-request (car header)) 
                 (pushnew (car header) all-headers :test #'string-equal)))))
      (add-headers net.aserve::*fast-headers*)
      (add-headers (slot-value paserve-request 'net.aserve::headers))
      all-headers)))

(defmethod get-request-parameter-value ((request-response paserve-request-response) parameter-name)
  (net.aserve:request-query-value parameter-name 
                                  (get-paserve-request request-response)))

(defmethod get-request-parameter-values ((request-response paserve-request-response) parameter-name)
  (mapcar #'cdr 
          (remove parameter-name (net.aserve:request-query (get-paserve-request request-response)) 
                  :test-not #'string-equal :key #'car)))

(defmethod get-request-parameters ((request-response paserve-request-response))
  (mapcar #'car (net.aserve:request-query (get-paserve-request request-response))))

(defmethod get-cookie ((request-response paserve-request-response) name)
  (cdr (assoc name 
              (net.aserve:get-cookie-values (get-paserve-request request-response))
              :test #'string-equal)))

(defmethod set-cookie ((request-response paserve-request-response) name value path &optional expires)
  (net.aserve:set-cookie-header (get-paserve-request request-response)
                                :name name
                                :value value
                                :path path
                                :expires (ecase expires ((:session nil) nil) (:never :never))))

(defmethod get-request-stream ((request-response paserve-request-response))
  (error "Accessing the raw request stream is currently not supported under portable allegro serve"))

(defmethod get-response-stream ((request-response paserve-request-response))
  (error "Accessing the raw response stream is currently not supported under portable allegro serve"))

;;;; eof
