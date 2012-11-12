;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: session1.lisp,v 1.11 2004/09/17 08:08:48 sven Exp $
;;;;
;;;; A basic web application that tests session tracking
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :session1
  (:index 'session1-start)
  (:static-root "static/")
  (:unsecure t))

(defun session1-start (request-response)
  (html-page (out request-response)
    (:html 
     (:head 
      (:title "Session1!")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (let ((session (get-session request-response)))
       (htm
        (:body 
         (:div :class "NX_panel"
          (:span :class "NX_title" "Session1!")
          (:div :class "NX_border"
           (:p 
            (fmt "Current session id is ~36R [~:*~d]" (get-session-id session)))
           (:p 
            "Session created " 
            (s-utils:format-universal-time (get-created-timestamp session) :stream out))
           (:p 
            "Session last modified " 
            (s-utils:format-universal-time (get-last-modified-timestamp session) :stream out))
           (let ((age (- (get-universal-time) (get-created-timestamp session))))
             (unless (zerop age)
               (htm
                (:p 
                 "Session has been alive for " 
                 (s-utils:format-duration (- (get-universal-time) (get-created-timestamp session)) :stream out)))))
           (let ((hit-counter-value (or (get-attribute session :counter) 0)))
             (incf hit-counter-value)
             (setf (get-attribute session :counter) hit-counter-value)
             (htm 
              (:p (fmt "This page has been hit ~d time~:p in this session" hit-counter-value))))
           (:div :class "NX_button_group"
            (:a :class "NX_button" :href (dynamic-url request-response nil) "Reload"))))))))))

;;;; eof