;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: calculator.lisp,v 1.9 2004/09/09 11:21:37 sven Exp $
;;;;
;;;; This example is a web interface to a classic four function calculator
;;;; where the calculator has a real independent model.
;;;; This example also demonstrates the use of a style sheet.
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

;;; the web app definition

(defwebapp :calculator2
  (:index 'show-calculator2)
  (:session-tracking-style :url)
  (:static-root "static/")
  (:request-tracking t)
  (:unsecure t))

;;; the calculator model

;;; the web app main page

(defparameter *id-and-layout2* 
  '(("numpad" ((#\7 #\8 #\9 #\0) (#\4 #\5 #\6 #\0) (#\1 #\2 #\3 #\0) (#\0 #\. #\= #\0) ("CHS" "EEX" "CLX" "ENT"))) 
    #|("operations" ((#\+) (#\-) (#\*) (#\/) (#\C) (#\X)))|#))

(defun show-calculator2 (request-response)
  (let ((calculator (get-attribute (get-session request-response) :calculator)))
    (when (null calculator)
      (setf calculator (make-instance 'calculator)
            (get-attribute (get-session request-response) :calculator) calculator))
    (html-page (out request-response :pprint-html t)
      (:html 
       (:header 
        (:title "KPAX Calculator")
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :webapp "calculator.css")))
       (:body
        (:div :id "calculator"
         (:h1 (str (display calculator)))
         (loop :for (id layout) :in *id-and-layout2* 
               :do
               (htm
                (:div :id id
                 (dolist (row layout)
                   (htm 
                    (:div :class "row"
                     (dolist (key row)
                       (htm 
                        (:a :href (dynamic-url request-response 'calculator-hit-key :key key)
                         (str key)))))))
                 (:div :class "row"
                  (:a :class "double" :href "#" "ENTER")))))))))))

;;; the function that processes key hits from the web page



;;;; eof
