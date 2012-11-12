;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: secure-login.lisp,v 1.4 2004/10/18 07:39:26 sven Exp $
;;;;
;;;; An example of using a custom login hook and custom authorizer
;;;; to do a zero-knowledge, challenge based secure authentication login protocol.
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defparameter *accounts* '(("admin" . "trustno1") ("guest" . "welcome")))

(defwebapp :secure-login
  (:index 'secure-login-start)
  (:static-root "static/")
  (:web-functions '(secure-login-page-two logout))
  (:session-validation t)
  (:login-hook 'secure-login-hook)
  (:authorizer 'secure-login-authorizer))

(defun secure-login-start (request-response)
  (html-page (out request-response)
    (:html 
     (:head 
      (:title "Secure Login!")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:h1 "Secure Login!")
      (:div :class "NX_panel"
       (:span :class "NX_title" "KPAX")
       (:div :class "NX_border"
        (:p (fmt "Welcome, ~a, to the secure KPAX Common Lisp Web Application Framework!" 
                 (get-attribute (get-session request-response) :user)))
        (:div :class "NX_button_group"
         (:a :class "NX_button" :href (dynamic-url request-response 'secure-login-page-two)
          "Page 2")
         (:a :class "NX_button" :href (dynamic-url request-response 'logout)
          "Logout"))))
      (:h2 "Request Parameters")
      (:table :class "NX_table" :width "100%"
       (:tr (:th :width "20%" "Parameter Name") (:th "Value"))
       (dolist (parameter-name (get-request-parameters request-response))
         (htm
          (:tr 
           (:td (str parameter-name))
           (:td (str (get-request-parameter-value request-response parameter-name)))))))))))

(defun secure-login-page-two (request-response)
  (html-page (out request-response)
    (:html 
     (:head 
      (:title "Secure Login (page 2)!")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:h1 "Secure Login (page 2)!")
      (:div :class "NX_panel"
       (:span :class "NX_title" "KPAX 2")
       (:div :class "NX_border"
        (:p "This is page two of this example.")
        (:div :class "NX_button_group"
         (:a :class "NX_button" :href (dynamic-url request-response nil) "Home"))))))))

(defun secure-login-authorizer (request-response)
  (let* ((session (get-session request-response))
         (username (get-request-parameter-value request-response "username"))
         (username-password (assoc username *accounts* :test #'equal))
         (server-challenge (get-attribute session :challenge))
         (challenge (get-request-parameter-value request-response "challenge"))
         (response (get-request-parameter-value request-response "response")))
    (if (and username-password
             challenge
             server-challenge
             (equal challenge server-challenge)
             response
             (string-equal response 
                           (ironclad:byte-array-to-hex-string (hmac-sha1 challenge (cdr username-password)))))
        (setf (get-attribute session :user) username)
      nil)))
         
(defun secure-login-hook (request-response &optional logout)
  (let ((session (get-session request-response))
        (web-app (get-application request-response))
        (username (get-request-parameter-value request-response "username"))
        (challenge (write-to-string (secure-random (expt 2 128)) :base 36)))
    (setf (get-attribute session :challenge) challenge)
    (when logout (setf (get-attribute session :user) nil))
    (html-page (out request-response)
      (:html 
       (:head
        (:title "Login")
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
       (:body 
        :style "padding:20px" 
        :onload (format nil "document.forms[0].elements[~d].focus();" (if username 1 0))
        (:script :type "text/javascript" :src (static-url request-response :webapp "sha1.js") "")
        (:script :type "text/javascript" :src (static-url request-response :webapp "secure-login.js") "")
        (:div :align "center"
         (:div :class "NX_login_panel"
          (:form 
           :action (dynamic-url request-response (get-index web-app)) 
           :onsubmit "login_form_onsubmit();"
           :method :post
           (:div :class "NX_title" "Secure Login")
           (:div :class "NX_fields"
            (:table
             (:tr 
              (:td (:label "Username:"))
              (:td (:input :type "text" :name "username" :size 24 :value (or username ""))))
             (:tr
              (:td (:label "Password:"))
              (:td (:input :type "password" :name "password" :size 24 :value "")))))
           (:input :type "hidden" :name "challenge" :value challenge)
           (:input :type "hidden" :name "response" :value "")
           (:div :class "NX_buttons"
            (:a 
             :class "NX_button" 
             :href "#" :onclick "login_button_onclick();" :title "Login" 
             "Login"))))))))))

;;;; eof