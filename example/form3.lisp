;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: forms1.lisp,v 1.6 2004/09/10 08:05:29 sven Exp $
;;;;
;;;; Testing form processing, multi language edition
;;;;
;;;; Copyright (C) 2006 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

;;; the web app definition

(defwebapp :forms3
  (:index 'forms3-start)
  (:static-root "static/")
  (:unsecure t))

(defun forms3-start (request-response)
  (html-page (out request-response)
    (:html 
     (:head 
      (:title "Forms!")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:h1 "Forms!")
      (:div :class "NX_form"
       (:form 
        :action (dynamic-url request-response 'process-form3) 
        :method :post
        :accept-charset "utf-8"
        (:input :type "text" :name "field" :value "")
        (:input :type "submit" :value "Submit")))
      (:p 
       "Demo contents: [English] 'Foo bar' "
       "[French] '&#xE9;l&#xE8;ve en Fran&#xE7;ais' "
       "[Czech] 'Pr&#x16F;vodce v &#x10D;e&#x161;tin&#x11B;'")))))

(defun process-form3 (request-response)
  (let* ((foo-parameter-value (get-request-parameter-value request-response "field"))
         (foo-value (gpsbuddy::utf8-encoded-bytes->string (map 'vector #'char-code foo-parameter-value))))
    (html-page (out request-response)
      (:html 
       (:head 
        (:title "Forms Result!")
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
       (:body 
        (:h1 "Forms Results!")
        (:div :class "NX_panel"
         (:span :class "NX_title" "Result")
         (:div :class "NX_border"
          (:p
           (fmt "Method is ~a" (get-request-method request-response)))
          (:p 
           (fmt "Field value is '~a'" (escape-string-all foo-value)))
          (:div :class "NX_button_group"
           (:a :class "NX_button" :href (dynamic-url request-response 'forms3-start) "Back")))))))))

;;;; eof
