;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: no-session1.lisp,v 1.1 2004/09/15 12:11:21 sven Exp $
;;;;
;;;; A web application can also do *NO* session tracking
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :no-session1
  (:index 'no-session1-start)
  (:session-tracking-style :none)
  (:unsecure t))

(defun no-session1-start (request-response)
  (html-page (out request-response)
    (:html 
     (:head 
      (:title "No Session1!"))
     (:body 
      (:h1 "No Session1!")
      (:p 
       (fmt "Current session is ~s, which is to be expected" (get-session request-response)))
      (:p 
       (:a :href (dynamic-url request-response nil) "Reload"))))))

;;;; eof