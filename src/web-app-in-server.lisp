;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: web-app-in-server.lisp,v 1.8 2004/09/09 14:41:15 sven Exp $
;;;;
;;;; KPAX web application implementation (code dependent of the web server instance)
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export 
 '(get-home-url))

;; a webapp definition and instance should be independent of the server instance(s) using it.
;; web-app-in-server groups the part of web-app that is server instance dependent,
;; in preparation of a better separation later.

(defclass web-app-in-server (attributes-mixin)
  ((complete-static-prefix :initform nil)
   (complete-dynamic-prefix :initform nil))
  (:documentation "The representation of a KPAX web application active in a server instance"))

(defgeneric start (web-app server &optional options)
  (:documentation "Start web-app")
  (:method ((web-app web-app-in-server) server &optional options)
   (with-slots (state) web-app
     (case state
       ((:created :stopped)
        (start-web-app-internal server web-app options)
        (log-info server "Started ~s" web-app)
        (log-info server 
                  "~a://~a:~d~a" 
                  (get-scheme server) (get-hostname server) (get-port server) 
                  (get-home-url web-app server))
        (setf state :started))
       (t (error "Cannot start ~s in state ~s" web-app state))))))

(defgeneric stop (web-app server &optional options)
  (:documentation "Stop web-app")
  (:method ((web-app web-app-in-server) server &optional options)
   (destructuring-bind (&key force ignore-errors &allow-other-keys) options
     (with-slots (state) web-app
       (cond ((or (eq state :started) force)
              (stop-web-app-internal server web-app options)
              (log-info server "Stopped ~s" web-app)
              (setf state :stopped))
             (t (unless ignore-errors
                  (error "Cannot stop ~s in state ~s" web-app state))))))))

(defgeneric get-complete-static-prefix (scope web-app web-app-server)
  (:method (scope (web-app web-app-in-server) (web-app-server web-app-server))
   (ecase scope
     (:server (get-static-prefix web-app-server))
     (:webapp (with-slots (complete-static-prefix)
                  web-app
                (or complete-static-prefix
                    (setf complete-static-prefix 
                          (concatenate 'string 
                                       (get-static-prefix web-app-server)
                                       (get-static-prefix web-app) ;; subclass responsability
                                       "/")))))))
  (:method ((scope (eql :server)) (web-app null) (web-app-server web-app-server))
   (get-static-prefix web-app-server)))

(defgeneric get-complete-dynamic-prefix (web-app web-app-server)
  (:method ((web-app web-app-in-server) (web-app-server web-app-server))
   (with-slots (complete-dynamic-prefix)
       web-app
     (or complete-dynamic-prefix
         (setf complete-dynamic-prefix (concatenate 'string 
                                                    (get-dynamic-prefix web-app-server)
                                                    (get-dynamic-prefix web-app) ;; subclass responsability
                                                    "/"))))))

(defgeneric get-home-url (web-app web-app-server)
  (:documentation "Return the home URL for web-app in web-app-server")
  (:method ((web-app web-app-in-server) (web-app-server web-app-server))
   (get-complete-dynamic-prefix web-app web-app-server)))

(defmethod reset-cached-values ((web-app web-app-in-server))
  (with-slots (complete-dynamic-prefix complete-static-prefix)
      web-app
    (setf complete-static-prefix nil
          complete-dynamic-prefix nil)))

;;;; eof
