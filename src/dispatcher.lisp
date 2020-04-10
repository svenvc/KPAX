;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: dispatcher.lisp,v 1.25 2004/11/08 13:51:08 sven Exp $
;;;;
;;;; The dispatcher is the first to handle a KPAX request-response:
;;;; - it finds the web app and delegates handling to it
;;;; - it does per request error handling
;;;; - it implements server pause functionality
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export 
 '(forward-request-response
   forward-to
   redirect-to
   not-yet-implemented
   html-message
   standard-message-hook
   standard-error-hook))

(defclass dispatcher ()
  ()
  (:documentation "I receive request-response objects from the server and dispatch them"))

;; optimize matching later!

(defun path-elements (path-string)
  (s-utils:tokens path-string :separators (list #\/)))

(defun find-web-app-matching (path-elements server)
  ;; either get-web-apps of server is :all in which case any existing web-app is visible
  ;; or else get-web-apps of server is a list of visible web-app names
  (maphash #'(lambda (name web-app)
               (when (or (eql (get-web-apps server) :all)
                         (member name (get-web-apps server)))
                 (let* ((prefix-elements (path-elements (get-complete-dynamic-prefix web-app server)))
                        (matching-position (search prefix-elements path-elements :test #'string=)))
                   (when (and matching-position (zerop matching-position))
                     (return-from find-web-app-matching 
                       (values web-app
                               (subseq path-elements (length prefix-elements))))))))
           *web-apps*)
  nil)

(defun standard-message-hook (request-response title message &rest args)
  (html-page (out request-response) 
    (:html
     (:head 
      (:title (str title))
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body
      (:div :class "NX_panel"
       (:span :class "NX_title" (str title))
       (:div :class "NX_border"
        (:p (apply #'format out (cons message args)))))))))

(defun html-message (request-response title message &rest args)
  "Render a simple page with a message using title and formatting message using args"
  (let ((web-app (get-application request-response)))
    (apply (if web-app (get-message-hook web-app) #'standard-message-hook)
           request-response
           title message args)))

(defun not-yet-implemented (request-response)
  "Web function that outputs a not yet implemented message"
  (let ((message "Not yet implemented"))
    (html-message request-response message message)))

(defun current-backtrace-to-string ()
  "Produce a string representation of the current stack of the current process"
  #+lispworks
  (with-output-to-string (out)
    (mp:map-process-backtrace mp:*current-process* 
                              #'(lambda (x) (format out "~a~%" x))))
  #+sbcl
  (with-output-to-string (out)
    (sb-debug:print-backtrace :stream out))
  #-(or lispworks sbcl)
  "no-backtrace")

(defun standard-error-hook (request-response condition backtrace)
  (html-page (out request-response :pprint-html nil)
    (:html 
     (:head
      (:title "KPAX Server Error")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:div :class "NX_panel"
       (:span :class "NX_title" "KPAX Server Error")
       (:div :class "NX_border"
        (:p "Handling")
        (:pre
         (esc (princ-to-string (get-uri-string request-response))))
        (:p "failed with condition")
        (:pre
         (esc (princ-to-string condition)))
        (:p "and backtrace")
        (:pre
         (esc (or backtrace "no-backtrace")))))))))

(defun server-error-page (request-response condition backtrace)
  "Render an error page reporting on condition"
  (reset-response request-response)
  (setf (get-response-status request-response) :error)
  (let ((web-app (get-application request-response)))
    (if web-app
        (funcall (get-error-hook web-app) request-response condition backtrace)
      (standard-error-hook request-response condition backtrace))))

(defgeneric handle-request-response (dispatcher request-response)
  (:documentation "Handle a request-response")
  (:method ((dispatcher dispatcher) (request-response request-response))
   (when (eq (get-state (get-server request-response)) :paused)
     (html-message request-response "Paused" "The server is currently paused, please come back later")
     (commit request-response)
     (return-from handle-request-response))
   (log-debug request-response "handling ~a" (get-uri-string request-response))
   (let* ((path-string (puri:uri-path (get-uri request-response)))
          (path-elements (path-elements path-string)))
     (multiple-value-bind (web-app sub-path)
         (find-web-app-matching path-elements (get-server request-response))
       (loop :with current-condition :with backtrace :do
             (restart-case 
                 (handler-bind ((condition #'(lambda (condition)
                                               (log-error request-response "error ~a" condition)
                                               (setf current-condition condition
                                                     backtrace (current-backtrace-to-string))
                                               (if (get-debug-mode (get-server request-response))
                                                     (error condition)
                                                 (invoke-restart 'accept condition)))))
                   (if web-app
                       (progn
                         (setup-for-web-app request-response web-app sub-path)
                         (handle-request-response web-app request-response)
                         (return
                          (multiple-value-prog1
                              (commit request-response)
                            (register-request-response-for-debugging request-response))))
                     (error "No web app found for ~s" path-string)))
               (retry ()
                 :report "Retry handling failed request"
                 (reset-response request-response))
               (accept (condition)
                 :interactive (lambda () (list current-condition))
                 :report "Continue by returning an error page"
                 (return
                  (if (get-debug-mode (get-server request-response))
                      (progn
                        (server-error-page request-response condition backtrace)
                        (commit request-response))
                    (ignore-errors
                      (server-error-page request-response condition backtrace)
                      (commit request-response)))))))))))

(defgeneric forward-request-response (dispatcher request-response &key webapp function)
  (:documentation "Forward a request-repsonse to a different webapp and/or function but keeping the session")
  (:method ((dispatcher dispatcher) (request-response request-response) &key webapp function)
   ;; maybe we have to go to handle-request-response - we don't do that for now
   (when (and (null webapp) (null function))
     (error "cannot forward request-response: not enough arguments"))
   (when (and webapp (symbolp webapp)) 
     (setf webapp (get-web-app webapp)))
   (when webapp
     (when (null function) 
       (setf function (get-index webapp)))
     (setf (get-application request-response) webapp)
     (setf (get-attribute (get-session request-response) :web-app) webapp)
     ;; the webapp changed, the session id cookie (per webapp) must follow
     (save-session-id request-response)
     ;; we store an extra session id under the web-server path to fix a safari anomaly
     ;; so both /kpax/dynamic/webapp as well as /kpax/dynamic get bound as path
     ;; this is a potential security problem: any kpax app will get the same session
     ;; even secure web-apps will get it, possibly with a semi valid login/user as well !
     (set-session-id-path request-response 
                          (get-dynamic-prefix (get-server request-response))))
   ;; reset what we can
   (setf (get-request-sub-path request-response) nil)
   ;; uri arguments are wrong now !! - we have no setf's to do this
   (funcall function request-response)))

(defun forward-to (request-response &key webapp function)
  "Convenience wrapper to forward a request-repsonse to a different webapp and/or function"
  (forward-request-response (get-dispatcher (get-server request-response))
                            request-response
                            :webapp webapp
                            :function function))

(defun redirect-to (request-response relative-url &rest args)
  "Redirect to another url, built from relative-url and args"
  (let ((url (apply #'dynamic-unescaped-url request-response relative-url args)))
    (setf (get-response-status request-response) :moved
          (get-response-header-value request-response "Location") url)))

;;;; eof
