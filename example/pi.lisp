;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: pi.lisp,v 1.6 2004/09/13 09:26:28 sven Exp $
;;;;
;;;; PI is a tool to look at some of the KPAX internals
;;;;
;;;; Copyright (C) 2004,2008 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :pi
  (:index 'pi-index)
  (:web-functions '(pi-server-info pi-list-sessions pi-list-web-applications 
                    pi-inspect-session pi-inspect-last-request-response pi-inspect-server pi-inspect-web-app
                    pi-touch-session pi-reap-old-sessions
                    logout))
  (:static-root "static/")
  (:authorizer '(("admin" . "trustno1"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro pi-page ((var request-response title) &body body)
    `(html-page (,var ,request-response) 
       (:html
        (:head 
         (:title ,title)
         (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
        (:body 
         (:h1 ,title)
         ,@body
         (:div :class "NX_button_group" :style "margin-top:10px"
          (:a :class "NX_button" :href (dynamic-url request-response nil) "Home")
          " "
          (:a :class "NX_button" :href (dynamic-url request-response 'logout) "Log out")))))))

(defun pi-index (request-response)
  (pi-page (out request-response "KPAX PI")
    (:div :class "NX_vertical_button_group"
     (loop :for (title function) :in '(("Server Info" pi-server-info)
                                       ("List Sessions" pi-list-sessions)
                                       ("List Web Applications" pi-list-web-applications)
                                       ("Inspect Last Request Response" pi-inspect-last-request-response)
                                       ("Inspect Server" pi-inspect-server))
           :do (htm
                (:a :class "NX_button" :href (dynamic-url request-response function) :style "width:250px"
                 (str title))
                " "))))) 

(defun sys-inspect (object)
  #-lispworks
  (inspect object)
  #+lispworks
  (env:display-inspector :object object))

(defvar *pi-boot-time* (get-universal-time))

(defun execute-to-string (thunk)
  (string-trim (list #\Newline #\Return #\Tab)
               (with-output-to-string (out)
                 (funcall thunk out))))
                     
(defun pi-server-info (request-response)
  (pi-page (out request-response "KPAX PI Server Info")
    (:div :class "NX_panel"
     (:span :class "NX_title" "KPAX Server")
     (:div :class "NX_border"
      (:p (fmt "KPAX Web Application Server running on ~a ~a" 
               (lisp-implementation-type) 
               (lisp-implementation-version)))
      (:p (fmt "KPAX Server instance is ") (esc (princ-to-string (get-server request-response))))
      (:p (fmt "Server Booted ~a " (s-utils:format-universal-time *pi-boot-time*)))
      (:p (fmt "Server Uptime is ~a " (s-utils:format-duration (- (get-universal-time) *pi-boot-time*))))
      (:pre
       (fmt "Output of (room):~%~%") 
       (esc (execute-to-string #'(lambda (out) (let ((*standard-output* out)) (room))))))
      (if (s-sysdeps:multiprocessing-capable-p)
          (htm
           (:pre
            (fmt "All processes:~%~%") 
            (esc (execute-to-string #'(lambda (out) (pprint (s-sysdeps:all-processes) out))))))
        (htm (:pre "Multiprocessing is not available, running a single-threaded server")))))))

(defun pi-inspect (request-response object)
  (pi-page (out request-response (escape-string (format nil "KPAX PI Inspecting: ~s" object)))
    (htm
     (:div :class "NX_panel"
      (:span :class "NX_title" "Inspect")
      (:div :class "NX_border"
       (:pre
        (esc (with-output-to-string (s) (describe object s))))))))
  (when (get-debug-mode (get-server request-response))
    (sys-inspect object)))

(defun pi-inspect-last-request-response (request-response)
  (pi-inspect request-response *last-request-response*))

(defun pi-inspect-server (request-response)
  (pi-inspect request-response (get-server request-response)))

(defun pi-inspect-session (request-response)
  (let* ((server (get-server request-response))
         (session-id (s-utils:parse-integer-safely (get-request-parameter-value request-response "id")))
         (session (find-session server session-id)))
    (pi-inspect request-response session)))

(defun pi-touch-session (request-response)
  (let* ((server (get-server request-response))
         (session-id (s-utils:parse-integer-safely (get-request-parameter-value request-response "id")))
         (session (find-session server session-id)))
    (touch session)
    (pi-list-sessions request-response)))

(defun pi-reap-old-sessions (request-response)
  (reap-old-sessions (get-server request-response))
  (pi-list-sessions request-response))

(defun pi-list-sessions (request-response)
  (pi-page (out request-response "KPAX PI Sessions")
    (:table :class "NX_table" :width "100%"
     (:tr 
      (:th "Session ID") 
      (:th "Created") 
      (:th "Last Modified") 
      (:th "Age")
      (:th "Inactive")
      (:th "Web-App")
      (:th "User")
      (:th "Actions"))
     (let ((all-sessions (sort (get-all-sessions (get-server request-response)) 
                               #'> :key #'get-last-modified-timestamp))
           (now (get-universal-time)))
       (dolist (session all-sessions)
         (htm
          (:tr 
           (:td (fmt "~36R [~:*~d]" (get-session-id session)))
           (:td (s-utils:format-universal-time (get-created-timestamp session) :stream out))
           (:td (s-utils:format-universal-time (get-last-modified-timestamp session) :stream out))
           (:td (s-utils:format-duration (- now (get-created-timestamp session)) :stream out))
           (:td (s-utils:format-duration (- now (get-last-modified-timestamp session)) :stream out))
           (:td (str (let ((web-app (get-attribute session :web-app)))
                       (if web-app (get-name web-app) ""))))
           (:td (esc (let ((user (get-attribute session :user)))
                       (if user (princ-to-string user) ""))))
           (:td 
            (:div :class "NX_button_group"
             (:a :class "NX_button" 
              :href (dynamic-url request-response 'pi-inspect-session :id (get-session-id session)) 
              "Inspect")
             " "
             (:a :class "NX_button" 
              :href (dynamic-url request-response 'pi-touch-session :id (get-session-id session)) 
              "Touch"))))))))
    (:div :class "NX_button_group" :style "margin-top:10px"
     (:a :class "NX_button" 
      :href (dynamic-url request-response 'pi-reap-old-sessions) "Reap Old Sessions"))))

(defun pi-inspect-web-app (request-response)
  (let* ((web-app-name (find-symbol (get-request-parameter-value request-response "id") :keyword))
         (web-app (get-web-app web-app-name)))
    (pi-inspect request-response web-app)))

(defun pi-list-web-applications (request-response)
  (pi-page (out request-response "KPAX PI Web Applications")
    (:table  :class "NX_table"
     (:tr 
      (:th "Name") 
      (:th "Actions"))
     (let ((server (get-server request-response))
           all-web-apps)
       (map-web-apps #'(lambda (web-app) (push web-app all-web-apps)))
       (setf all-web-apps (sort all-web-apps #'string-lessp :key #'get-name))
       (dolist (web-app all-web-apps)
         (htm
          (:tr
           (:td 
            (str (get-name web-app)))
           (:td
            (:div :class "NX_button_group"
             (:a :class "NX_button" :href (get-home-url web-app server) 
              "Open")
             " "
             (:a :class "NX_button" :href (dynamic-url request-response 'pi-inspect-web-app :id (get-name web-app)) 
              "Inspect"))))))))))

;;;; eof
