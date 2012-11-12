;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: run.lisp,v 1.1 2004/06/15 07:48:38 sven Exp $
;;;;
;;;; Code to run the server in development mode
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export '(start-kpax))

(defun start-kpax (&key (server-class 's-http-server) (port 2001) (logstream *standard-output*) (debug-mode t))
  "Do init-kpax & run-kpax in debug mode with extra logging if needed"
  (let ((server (init-kpax server-class :debug-mode debug-mode)))
    (when debug-mode 
      (setf (get-debug-stream server) logstream)
      (setf (get-cutoff server) +log-level-off+))
    (run-kpax server :debug-mode debug-mode :port port)
    server))

;;; Manipulating a running server...

;; Disable debug mode

#+NIL
(setf (get-debug-mode *web-app-server*) nil
      (get-debug-stream *web-app-server*) nil)

;; Hook onto a logstream

#+NIL
(defvar *logstream* (open "~/logs/kpax.log" 
                          :direction :output  
                          :if-exists :append
                          :if-does-not-exist :create))

;; Ask for logging output to be sent to our logstream

#+NIL
(setf (get-logstream *web-app-server*) *logstream*)

;; Setup S-HTTP-SERVER access logging

#+NIL
(defvar *access-logstream* (open "~/logs/kpax-access.log" 
                                 :direction :output  
                                 :if-exists :append
                                 :if-does-not-exist :create))

;; Ask for access logging output to be sent to our logstream

#+NIL
(setf (s-http-server::get-access-log-stream (get-s-http-server *web-app-server*)) *access-logstream*)



;;;; eof