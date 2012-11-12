;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: server.lisp,v 1.19 2004/10/05 13:56:23 sven Exp $
;;;;
;;;; Superclass of all server objects, defining part of the server interface
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(web-app-server
   get-dynamic-prefix
   get-static-prefix
   get-hostname
   get-port
   get-scheme
   get-logstream
   get-debug-mode
   get-debug-stream
   get-dispatcher
   get-state
   init
   startup
   shutdown
   pause))

(defclass web-app-server (session-manager attributes-mixin logger)
  ((name :accessor get-name :initarg :name :initform nil)
   (dynamic-prefix :accessor get-dynamic-prefix :initarg :dynamic-prefix :initform "/kpax/dynamic/")
   (static-prefix :accessor get-static-prefix :initarg :static-prefix :initform "/kpax/static/")
   (hostname :accessor get-hostname :initarg :hostname :initform "localhost")
   (web-apps :accessor get-web-apps :initform :all)
   (server-id :accessor get-server-id :initform "KPAX1")
   (dispatcher :accessor get-dispatcher :initform (make-instance 'dispatcher))
   (debug-mode :accessor get-debug-mode :initarg :debug-mode :initform nil)
   (stream-appender)
   (debug-stream-appender)
   (state :accessor get-state :initform :created))
  (:documentation "I represent a server delivering requests-response objects"))

(defgeneric get-dynamic-prefix (server)
  (:documentation "Get this server's dynamic prefix"))

(defgeneric get-static-prefix (server)
  (:documentation "Get this server's static prefix"))

(defgeneric get-hostname (server)
  (:documentation "Get this server's hostname"))

(defgeneric get-port (server)
  (:documentation "Get this server's port"))

(defgeneric get-scheme (server)
  (:documentation "Get this server's scheme")
  (:method ((server web-app-server))
   "http"))

(defgeneric get-dispatcher (server)
  (:documentation "Access this server's dispatcher"))

(defgeneric get-state (object)
  (:documentation "Get the current state of object"))

(defgeneric get-debug-mode (object)
  (:documentation "Return whether object is in debug mode or not"))

(defmethod initialize-instance :after ((web-app-server web-app-server) 
                                       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (stream-appender debug-stream-appender)
      web-app-server
    (setf stream-appender (make-instance 'stream-appender))
    (add-appender web-app-server stream-appender)
    (setf debug-stream-appender (make-instance 'debug-stream-appender))
    (add-appender web-app-server debug-stream-appender)))

(defmethod (setf get-debug-mode) :after (value (web-app-server web-app-server))
  (with-slots (debug-stream-appender)
      web-app-server
    (setf (get-enabled debug-stream-appender) value)))

(defgeneric get-debug-stream (web-app-server)
  (:documentation "Get the debug stream of this web application server")
  (:method ((web-app-server web-app-server))
   (with-slots (debug-stream-appender)
       web-app-server
     (get-stream debug-stream-appender))))

(defgeneric (setf get-debug-stream) (stream web-app-server)
  (:documentation "Set the debug stream of this web application server")
  (:method (stream (web-app-server web-app-server))
   (with-slots (debug-stream-appender)
       web-app-server
     (setf (get-stream debug-stream-appender) stream))))

(defgeneric get-logstream (web-app-server)
  (:documentation "Get the log stream of this web application server")
  (:method ((web-app-server web-app-server))
   (with-slots (stream-appender)
       web-app-server
     (get-stream stream-appender))))

(defgeneric (setf get-logstream) (stream web-app-server)
  (:documentation "Set the log stream of this web application server")
  (:method (stream (web-app-server web-app-server))
   (with-slots (stream-appender)
       web-app-server
     (setf (get-stream stream-appender) stream))))

(defgeneric init (web-app-server &optional options)
  (:documentation "Initialize web-app-server using options")
  (:method ((web-app-server web-app-server) &optional options)
   (destructuring-bind (&key (dynamic-prefix "/kpax/dynamic/")
                             (static-prefix "/kpax/static/")
                             (hostname "localhost")
                             (web-apps :all)
                             debug-mode
                             &allow-other-keys)
       options
     (setf (get-dynamic-prefix web-app-server) dynamic-prefix
           (get-static-prefix web-app-server) static-prefix
           (get-hostname web-app-server) hostname
           (get-web-apps web-app-server) web-apps
           (get-debug-mode web-app-server) debug-mode
           (get-state web-app-server) :ready)
     (log-info web-app-server "Server ready")
     web-app-server)))

(defgeneric startup (web-app-server &optional options)
  (:documentation "Start up web-app-server using options")
  (:method ((web-app-server web-app-server) &optional options)
   (destructuring-bind (&key debug-mode &allow-other-keys)
       options
     (setf (get-debug-mode web-app-server) debug-mode)
     (with-slots (state) web-app-server
       (case state
         (:ready (log-info web-app-server "Server running") (setf state :running))
         (t (error "Cannot startup from state ~s" state)))))))

(defgeneric shutdown (web-app-server &optional options)
  (:documentation "Shut down web-app-server using options")
  (:method ((web-app-server web-app-server) &optional options)
   (declare (ignore options))
   (with-slots (state) web-app-server
     (case state
       (:running 
        (log-info web-app-server "Server stopped and ready")
        (setf (get-debug-stream web-app-server) nil)
        (setf (get-logstream web-app-server) nil)
        (setf state :ready))
       (:ready :ready)
       (t (error "Cannot shutdown from state ~s" state))))))

(defgeneric pause (web-app-server &optional options)
  (:documentation "Temporarily and gracefully take KPAX off-line, toggle")
  (:method ((web-app-server web-app-server) &optional options)
   (declare (ignore options))
   (with-slots (state) web-app-server
     (case state
       (:running (log-info web-app-server "Server paused") (setf state :paused))
       (:paused (log-info web-app-server "Server unpaused and running") (setf state :running))
       (t (error "Cannot pause in state ~s" state))))))

(defgeneric start-web-app-internal (web-app-server web-app options)
  (:documentation "Start web-app in web-app-server")
  (:method (web-app-server web-app options)
   (declare (ignore web-app-server web-app options))))

(defgeneric stop-web-app-internal (web-app-server web-app options)
  (:documentation "Stop web-app in web-app-server")
  (:method (web-app-server web-app options)
   (declare (ignore web-app-server web-app options))))

(defmethod print-object ((web-app-server web-app-server) stream)
  (print-unreadable-object (web-app-server stream :type t :identity t)
    (with-slots (name state)
        web-app-server
      (format stream "~s ~s" name state))))

;;;; eof