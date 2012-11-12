;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: helloworld1.lisp,v 1.11 2004/09/09 11:21:37 sven Exp $
;;;;
;;;; The most basic HelloWorld example: how to define a webapp consisting of one page 
;;;; with some dynamic elements as well as a static reference
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :helloworld1
  (:index 'helloworld1-start)
  (:static-root "static/")
  (:unsecure t))

(defun get-daypart (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour)
      (decode-universal-time time)
    (declare (ignore second minute))
    (cond ((<= 6 hour 12) "morning")
          ((<= 12 hour 18) "afternoon")
          ((<= 18 hour 24) "evening")
          (t "night"))))

(defun helloworld1-start (request-response)
  (html-page (out request-response)
    (:html 
     (:head 
      (:title "HelloWorld1!") 
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:h1 "HelloWorld1!")
      (:div :class "NX_panel"
       (:span :class "NX_title" "KPAX")
       (:div :class "NX_border"
        (:p (fmt "Good ~a!" (get-daypart)))
        (:p (fmt "HelloWorld from KPAX running on ~a ~a" 
                 (lisp-implementation-type)
                 (lisp-implementation-version)))
        (:img 
         :width 359 :height 510 :alt "KPAX Movie Poster" 
         :src (static-url request-response :webapp "kpax-movie-poster.jpg"))))))))

;;;; eof