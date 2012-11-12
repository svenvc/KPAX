;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: list-web-apps.lisp,v 1.8 2004/09/17 08:08:48 sven Exp $
;;;;
;;;; This web app lists all entry points into all installed web apps
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :list-web-apps
  (:index 'list-web-apps-start)
  (:static-root "static/")
  (:unsecure t))

(defun list-web-apps-start (request-response)
  (html-page (out request-response)
    (:html 
     (:head
      (:title "KPAX Web Application Listing")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:h1 "KPAX Web Application Listing")
      (:p "These are the currently installed (loaded) KPAX web applications:")
      (:div :class "NX_vertical_button_group"
       (let ((server (get-server request-response)))
         (map-web-apps #'(lambda (web-app)
                           (htm
                            (:a :class "NX_button" :style "width:150px;" :href (get-home-url web-app server)
                             (str (get-name web-app))))))))))))

;;;; eof