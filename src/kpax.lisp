;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: kpax.lisp,v 1.13 2004/09/01 08:06:50 sven Exp $
;;;;
;;;; The main KPAX file, managing web-apps and kpax as a whole
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export 
 '(init-kpax 
   destroy-kpax
   run-kpax 
   stop-kpax
   restart-kpax
   pause-kpax
   get-kpax-server
   defwebapp 
   get-web-app
   map-web-apps
   start-web-app 
   stop-web-app 
   restart-web-app))

(defun get-web-app (name)
  "Get the web application known by name (return nil when not found)"
  (gethash name *web-apps*))

(defun map-web-apps (function)
  "Apply function on each defined web application and return the result"
  (loop :for web-app :being :the :hash-values :of *web-apps*
        :collect (funcall function web-app)))

(defun ensure-web-app (name options)
  "Either create a new web-app or use an existing one, resetting its options"
  (let ((web-app (get-web-app name)))
    (unless web-app
      (setf web-app (make-instance 'web-app :name name)
            (gethash name *web-apps*) web-app))
    (setf (get-option-list web-app) options)
    (process-option-list web-app)
    (loop :for server :in *web-app-servers* :do
          (stop-web-app name server :force t)
          (start-web-app name server))
    web-app))

(defmacro defwebapp (name &rest options)
  "Define a web application by name with the options listed"
  `(ensure-web-app ,name 
                   ,(cons 'list (append (list :load-truename (or *load-truename* *compile-file-truename*))
                                        (list :load-package *package*)
                                        (reduce #'append options)))))

(defun start-web-app (name server &rest options)
  "Start the web application known by name, installing and publishing it in the context of server"
  (start (get-web-app name) server options))

(defun stop-web-app (name server &rest options)
  "Stop the web application known by name, making it no longer accessible in the context of server"
  (stop (get-web-app name) server options))

(defun restart-web-app (name server &rest options)
  "Stop and start the web application known by name"
  (apply #'stop-web-app name server options)
  (apply #'start-web-app name server options))

(defun init-kpax (server-class &rest options)
  "Initialize the KPAX web application framework"
  (let* ((name (getf options :name "anonymous"))
         (existing-server (get-kpax-server name)))
    (when existing-server
      (stop-kpax existing-server :force t))
    (let ((server (make-instance server-class :name name))
          (existing-server (get-kpax-server name)))
      (when existing-server
        (destroy-kpax existing-server))
      (pushnew server *web-app-servers*)
      (init server options))))

(defun run-kpax (server &rest options)
  "Run the KPAX web application server"
  (map-web-apps #'(lambda (web-app)
                    (unless (eql (get-state web-app) :stopped)
                      (stop web-app server '(:force t :ignore-errors t)))
                    (start web-app server)))
  (startup server options))

(defun stop-kpax (server &rest options)
  "Stop the KPAX web application server"
  (map-web-apps #'(lambda (web-app) 
                    (stop web-app server '(:force t))))
  (shutdown server options))

(defun restart-kpax (server &rest options)
  "Restart the KPAX web application server"
  (apply #'stop-kpax server options)
  (apply #'run-kpax server options))

(defun destroy-kpax (server &rest options)
  "Stop and remove the the KPAX web application server"
  (shutdown server options)
  (setf *web-app-servers* (remove server *web-app-servers*))
  :destroyed)

(defun pause-kpax (server &rest options)
  "Take all KPAX web applications temporarily off-line, showing a 'mainteance in progress' page"
  (pause server options))

(defun get-kpax-server (name)
  "Return the KPAX web application server instance with name"
  (find name *web-app-servers* :test #'string-equal :key #'get-name))

;;;; eof