;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: secure1.lisp,v 1.7 2004/09/16 10:06:06 sven Exp $
;;;;
;;;; The simple, secured web application.
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :secure1
  (:index 'secure1-start)
  (:static-root "static/")
  (:web-functions '(logout))
  (:session-validation t)
  (:authorizer '(("admin" . "trustno1") ("guest" . "welcome"))))

;; by default web apps require a proper login (use (:unsecure t) to change this default)
;; the simplest authorizer is an assoc list of (username . password) strings
;; explicitely specifying :web-functions to be nil limits access to other web functions
;; the default for :web-functions is :all, allowing access to all explicit internal functions
;; in the web-app's package (which is easier during development)

(defun secure1-start (request-response)
  (html-page (out request-response)
    (:html 
     (:head 
      (:title "Secure1!")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:h1 "Secure1!")
      (:div :class "NX_panel"
       (:span :class "NX_title" "KPAX")
       (:div :class "NX_border"
        (:p (fmt "Welcome, ~a, to the secure KPAX Common Lisp Web Application Framework!" 
                 (get-attribute (get-session request-response) :user)))
        (:div :class "NX_button_group"
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

;;;; eof