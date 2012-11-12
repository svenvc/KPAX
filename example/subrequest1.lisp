;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: subrequest1.lisp,v 1.4 2005/04/26 12:40:15 sven Exp $
;;;;
;;;; A demo of doing a subrequest using the XMLHttpRequest JavaScript object.
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :subrequest1
  (:index 'subrequest1-start)
  (:web-functions '(subrequest1-compute))
  (:static-root "static/")
  (:unsecure t))

(defun subrequest1-start (request-response)
  (html-page (out request-response)
    (:html 
     (:head 
      (:title "Subrequest1!") 
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:script :type "text/javascript" :src (static-url request-response :webapp "subrequest1.js") "")
      (:script :type "text/javascript" 
       (fmt "base_url=\"~a\";" (dynamic-url request-response 'subrequest1-compute)))
      (:h1 "Subrequest1!")
      (:div :class "NX_panel"
       (:span :class "NX_title" "Add two numbers")
       (:div :class "NX_border"
        (:form :style "margin-top:10px;"
         (:input :type "text" :name "number1" :value "0") :br
         "+" :br
         (:input :type "text" :name "number2" :value "0") :br
         (:div :class "NX_buttons" :style "margin-top:10px; margin-bottom:10px;"
          (:a 
           :class "NX_button" 
           :onclick "compute_response();" :title "Add" 
           "="))
         (:input :type "text" :name "sum" :value "0"))))))))

(defun subrequest1-compute (request-response)
  (let ((number1 (util:parse-integer-safely (get-request-parameter-value request-response "number1")))
        (number2 (util:parse-integer-safely (get-request-parameter-value request-response "number2")))
        (out (get-content-stream request-response)))
    (setf (get-response-mime-type request-response) "text/plain")
    (if (and number1 number2)
        (let ((sum (+ number1 number2)))
          (setf (get-attribute (get-session request-response) :sum) sum) ;; for testing only ;-)
          (format out "~d" sum))
      (format out "ERROR NaN"))))

;;;; eof
