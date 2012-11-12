;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: debug.lisp,v 1.9 2005/05/31 11:47:37 sven Exp $
;;;;
;;;; Debugging tools
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(debug-request
   *last-request-response*
   *before-last-request-response*
   *last-debug-request*
   *before-last-debug-request*))

(defclass debug-server (web-app-server)
  ()
  (:documentation "A dummy web app server to support debug request-response objets"))

(defclass debug-request-response (request-response)
  ((uri-string :accessor get-uri-string :initarg :uri-string :initform "")
   (headers :accessor get-headers :initarg :headers :initform nil)
   (parameters :accessor get-parameters :initarg :parameters :initform nil)
   (session-id :initarg :session-id :initform nil)
   (output :accessor get-output :initform nil))
  (:documentation "An implementation of request-response to support debugging"))
    
(defvar *debug-server* (make-instance 'debug-server :debug-mode t)
  "Solitairy instance of the dummy debug web app server")
      
(defmethod get-request-header-value ((request-response debug-request-response) header-name)
  (cdr (assoc header-name (get-headers request-response) :test #'string-equal)))

(defmethod get-request-headers ((request-response debug-request-response))
  (mapcar #'car (get-headers request-response)))

(defmethod get-request-parameter-value ((request-response debug-request-response) parameter-name)
  (cdr (assoc parameter-name (get-parameters request-response) :test #'string-equal)))

(defmethod get-request-parameter-values ((request-response debug-request-response) parameter-name)
  (mapcar #'cdr 
          (remove parameter-name (get-parameters request-response) 
                  :test-not #'string-equal :key #'car)))

(defmethod get-request-parameters ((request-response debug-request-response))
  (mapcar #'car (get-parameters request-response)))

(defmethod get-session-id ((request-response debug-request-response))
  (with-slots (session-id) request-response
    session-id))
    
(defmethod set-session-id ((request-response debug-request-response) id path)
  (declare (ignore path))
  (with-slots (session-id) request-response
    (setf session-id id)))

(defmethod commit ((request-response debug-request-response))
  (let ((output (get-output-stream-string (get-content-stream request-response)))
        (status (get-response-status request-response)))
    (setf (get-output request-response) output)
    (log-debug request-response "replied ~d bytes with status ~a" (length output) status)))

(defmethod set-cookie ((request-response debug-request-response) name value path &optional expires)
  (log-info request-response "debug-request-response ignored Set-Cookie ~a=~a; path=~a~@[; expires=~a~]" 
            name 
            (uri-encode-for-query value) 
            path 
            (ecase expires ((:session nil) nil) (:never *far-in-the-future*))))

(defmethod get-cookie ((request-response debug-request-response) name)
  (log-info request-response "debug-request-response return nil for cookie ~a" name)
  nil)

(defun debug-request (uri-string-or-request-response &key parameters session-id headers)
  "Execute a request (uri-string or debug-request-response instance) directly on KPAX"
  (let (request-response uri)
    (cond ((stringp uri-string-or-request-response)
           (setf uri uri-string-or-request-response)
           (when (char/= (char uri 0) #\/)
             (setf uri (concatenate 'string (get-dynamic-prefix *debug-server*) uri)))
           (setf request-response (make-instance 'debug-request-response 
                                                 :uri-string uri
                                                 :session-id session-id
                                                 :server *debug-server*))
           (set-standard-headers request-response)
           (setf (get-parameters request-response) (maptree #'princ-to-string parameters)
                 (get-headers request-response) (maptree #'princ-to-string headers)))
          ((eq (class-of uri-string-or-request-response) (find-class 'debug-request-response))
           (setf request-response uri-string-or-request-response))
          (t 
           (error "~s is neither a string nor a debug-request-response" uri-string-or-request-response)))
    (handle-request-response (get-dispatcher *debug-server*)
                             request-response)
    (values (get-session-id request-response)
            (get-output request-response)
            request-response)))

(defun create-debug-request-response (request-response)
  "Copy the request part of an arbitrary request-response into a new debug-request-response"
  (make-instance 'debug-request-response
                 :uri-string (get-uri-string request-response)
                 :headers (let (list)
                            (dolist (header (get-request-headers request-response) list)
                              (push (cons header (get-request-header-value request-response header))
                                    list)))
                 :parameters (let (list)
                               (dolist (parameter (get-request-parameters request-response) list)
                                 (push (cons parameter (get-request-parameter-value request-response parameter))
                                       list)))
                 :session-id (let ((session (get-session request-response)))
                               (if session (get-session-id session) nil))
                 :server *debug-server*))

(defvar *last-request-response* nil
  "The complete, original last request-response handled by the server")

(defvar *before-last-request-response* nil
  "The complete, original before last request-response handled by the server")

(defvar *last-debug-request* nil
  "The request part of the last request-response handled by the server")

(defvar *before-last-debug-request* nil
  "The request part of the before last request-response handled by the server")

(defun register-request-response-for-debugging (request-response)
  "Store request part of request-response as last processed request-response for later debugging"
  (when (and (get-debug-mode (get-server request-response))
             (not (eq (class-of (get-server request-response)) (find-class 'debug-server))))
    (let ((debug-request-response (create-debug-request-response request-response)))
      (setf *before-last-request-response* *last-request-response*
            *last-request-response* request-response
            *before-last-debug-request* *last-debug-request*
            *last-debug-request* debug-request-response))))

;;;; eof
