;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: web-app.lisp,v 1.22 2004/10/05 13:56:23 sven Exp $
;;;;
;;;; KPAX web application implementation (code independent of the web server instance)
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(web-app
   get-name
   get-index
   get-static-root
   get-option
   logout
   note-inconsistent-request
   report-inconsistent-request
   standard-login-hook
   standard-request-hook))

;; a webapp definition and instance should be independent of the server instance(s) using it.
;; web-app-in-server groups the part of web-app that is server instance dependent,
;; in preparation of a better separation later (instead of just inheriting from it)

(defclass web-app (web-app-in-server)
  ((name :accessor get-name :initarg :name :initform nil)
   (static-prefix :accessor get-static-prefix :initarg :static-prefix :initform nil)
   (dynamic-prefix :accessor get-dynamic-prefix :initarg :dynamic-prefix :initform nil)
   (state :accessor get-state :initform :created)
   (option-list :accessor get-option-list :initarg :option-list :initform nil)
   (web-functions :accessor get-web-functions :initform :all)
   (index :accessor get-index :initform nil)
   (package :accessor get-package :initform nil)
   (session-tracking-style :accessor get-session-tracking-style :initform :cookie)
   (request-tracking :accessor get-request-tracking :initform nil)
   (session-validation :accessor get-session-validation :initform nil)
   (inconsistent-request-action :accessor get-inconsistent-request-action :initform #'note-inconsistent-request)
   (authorizer :accessor get-authorizer :initform nil)
   (message-hook :accessor get-message-hook :initform 'standard-message-hook)
   (login-hook :accessor get-login-hook :initform 'standard-login-hook)
   (error-hook :accessor get-error-hook :initform 'standard-error-hook)
   (request-setup-hook :accessor get-request-setup-hook :initform nil)
   (request-hook :accessor get-request-hook :initform #'standard-request-hook)
   (session-hook :accessor get-session-hook :initform nil)
   (static-root :accessor get-static-root :initarg :static-root :initform nil))
  (:documentation "The representation of a KPAX web application"))

(defgeneric get-index (web-app)
  (:documentation "Return the start function for this web application"))

(defgeneric get-name (object)
  (:documentation "Return the name of this object"))

(defgeneric get-static-root (web-app)
  (:documentation "Return the static root (filesystem directory) for this web application"))

(defmethod process-option-list ((web-app web-app))
  "Process options"
  (destructuring-bind (&key 
                       static-root unsecure index static-prefix dynamic-prefix package authorizer 
                       load-package load-truename (web-functions :all)
                       message-hook login-hook error-hook request-setup-hook request-hook session-hook
                       session-tracking-style request-tracking inconsistent-request-action
                       session-validation
                       &allow-other-keys) 
      (get-option-list web-app)
    (reset-cached-values web-app)
    (setf (get-static-root web-app) (when static-root
                                      (merge-pathnames static-root (s-utils:pathname-parent load-truename)))
          (get-web-functions web-app) web-functions
          (get-authorizer web-app) (or authorizer (if unsecure :all nil))
          (get-static-prefix web-app) (or static-prefix (string-downcase (symbol-name (get-name web-app))))
          (get-dynamic-prefix web-app) (or dynamic-prefix (string-downcase (symbol-name (get-name web-app))))
          (get-package web-app) (or package load-package)
          (get-index web-app) index)
    (when message-hook (setf (get-message-hook web-app) message-hook))
    (when login-hook (setf (get-login-hook web-app) login-hook))
    (when error-hook (setf (get-error-hook web-app) error-hook))
    (when request-setup-hook (setf (get-request-setup-hook web-app) request-setup-hook))
    (when request-hook (setf (get-request-hook web-app) request-hook))
    (when session-hook (setf (get-session-hook web-app) session-hook))
    (when session-tracking-style (setf (get-session-tracking-style web-app) session-tracking-style))
    (when request-tracking (setf (get-request-tracking web-app) request-tracking))
    (when inconsistent-request-action (setf (get-inconsistent-request-action web-app) inconsistent-request-action))
    (when session-validation (setf (get-session-validation web-app) session-validation))))

(defmethod print-object ((web-app web-app) stream)
  (print-unreadable-object (web-app stream :type t :identity t)
    (format stream "~s" (get-name web-app))))

(defgeneric get-option (object option-name)
  (:documentation "Returns the value of a named option in object")
  (:method ((web-app web-app) option-name)
   (let ((tail (member option-name (get-option-list web-app))))
     (when tail
       (second tail)))))

(defmethod find-applicable-web-function ((web-app web-app) (request-response request-response))
  (if (null (get-request-sub-path request-response))
      (get-index web-app)
    (let ((web-function-name (string-upcase (first (get-request-sub-path request-response))))
          (web-functions (get-web-functions web-app)))
      (multiple-value-bind (web-function-symbol status)
          (intern web-function-name (or (get-package web-app) *package*))
        ;; either all web functions are allowed but then we reject inherited symbols
        ;; or the web function is in the list of explicitely allowed functions
        ;; the web app index function is always permitted (doesn't have to be in the list)
        ;; in either case the function must be bound
        (cond ((not (fboundp web-function-symbol)) 
               nil)
              ((and (eq web-functions :all)
                    (not (eq status :inherited)))
               web-function-symbol)
              ((or (eq web-function-symbol (get-index web-app)) 
                   (eq web-function-symbol 'not-yet-implemented))
               web-function-symbol)
              ((and (listp web-functions)
                    (member web-function-symbol (get-web-functions web-app)) )
               web-function-symbol)
              (t nil))))))

(defmethod authorize ((request-response request-response))
  (let* ((session (get-session request-response))
         (user (when session (get-attribute session :user)))
         (username (get-request-parameter-value request-response "username"))
         (web-app (get-application request-response))
         (authorizer (get-authorizer web-app)))
    (cond (user t)
          ((eq authorizer :all) t)
          ((and (listp authorizer) username)
           (let ((username-password (assoc username authorizer :test #'equal))
                 (password (get-request-parameter-value request-response "password")))
             (if (and username-password
                      password
                      (equal password (cdr username-password)))
                 (setf (get-attribute session :user) username)
               nil)))
          ((or (functionp authorizer) 
               (and (symbolp authorizer) 
                    (fboundp authorizer)))
           (funcall authorizer request-response))
          (t nil))))

(defun standard-login-hook (request-response &optional logout)
  "Standard login hook called to present a login page"
  (declare (ignore logout))
  (let ((web-app (get-application request-response))
        (username (get-request-parameter-value request-response "username")))
    (html-page (out request-response)
      (:html 
       (:head
        (:title "Login")
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
       (:body :style "padding:20px" 
              :onload (format nil "document.forms[0].elements[~d].focus();" (if username 1 0))
        (:div :align "center"
         (:div :class "NX_login_panel"
          (:form :action (dynamic-url request-response (get-index web-app)) :method :post
           (:div :class "NX_title" "Login")
           (:div :class "NX_fields"
            (:table
             (:tr 
              (:td (:label "Username:"))
              (:td (:input :type "text" :name "username" :size 24 :value (or username ""))))
             (:tr
              (:td (:label "Password:"))
              (:td (:input :type "password" :name "password" :size 24 :value "")))))
           (:div :class "NX_buttons"
            (:a :class "NX_button" :href "#" :onclick "document.forms[0].submit();" :title "Login" 
             :style "key-equivalent: esc ctrl-x;"
             "Login"))))))))))

(defun logout (request-response)
  "Generic logout hook"
  (setf (get-attribute (get-session request-response) :user) nil)
  (funcall (get-login-hook (get-application request-response)) request-response t))

(defun note-inconsistent-request (reqeust-response)
  "Standard inconsistent request recording (setting request attribute :inconsistent-request to t)"
  (setf (get-attribute reqeust-response :inconsistent-request) t))

(defun report-inconsistent-request (request-response)
  "Standard inconsistent request reporter"
  (html-message request-response 
                "Error" 
                "Page already submitted (don't go backwards). ~a"
                (with-html-output-to-string (out)
                  (htm (:a :href (dynamic-url request-response nil) "OK"))))
  nil)

(defun handle-inconsistent-request (request-response)
  "Returns nil when handling done, t when processing can continue"
  (funcall (get-inconsistent-request-action (get-application request-response))
           request-response))

(defun redirect-to-self (request-response)
  "Reply on the current request with a redirect to self (after url session binding)"
  (let ((url (dynamic-url request-response "~{~a~^/~}" (get-request-sub-path request-response))))
    (setf (get-response-status request-response) :moved
          (get-response-header-value request-response "Location") url)
    (html-page (out request-response)
      (:html
       (:header (:title "Moved"))
       (:body (:h1 "Moved")
        (:p "The URL you requested moved and is now at" (:a :ref url "this location")))))))

(defun standard-request-hook (request-response web-function)
  "Standard request hook simply applies web-function on request-response"
  (funcall web-function request-response))

(defmethod handle-request-response ((web-app web-app) (request-response request-response))
  (multiple-value-bind (new-session-p consistent-request-p)
      (bind-session request-response)
    (if (and new-session-p (eq (get-session-tracking-style web-app) :url))
        (redirect-to-self request-response)
      (when (or consistent-request-p
                (handle-inconsistent-request request-response))
        (if (authorize request-response)
            (let ((web-function (find-applicable-web-function web-app request-response)))
              (if (null web-function)
                  (error "No web function found for ~a" (get-uri-string request-response))
                (funcall (get-request-hook web-app) request-response web-function)))
          (funcall (get-login-hook web-app) request-response))))))

;;;; eof
