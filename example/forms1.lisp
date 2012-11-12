;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: forms1.lisp,v 1.6 2004/09/10 08:05:29 sven Exp $
;;;;
;;;; Testing form processing, error handling
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

;;; the web app definition

(defwebapp :forms1
  (:index 'forms1-start)
  (:static-root "static/")
  (:unsecure t))

(defun forms1-start (request-response)
  (html-page (out request-response)
    (:html 
     (:head 
      (:title "Forms!")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:h1 "Forms!")
      (:div :class "NX_form"
       (:form :action (dynamic-url request-response 'process-form1) :method :post
        (:input :type "text" :name "field" :value "")
        (:input :type "submit" :value "Submit")))
      (:div :class "NX_form"
       (:form :action (dynamic-url request-response 'process-form2) :method :get
        (:input :type "text" :name "error-string" :value "my-error")
        (:input :type "submit" :value "Force Error")))))))

(defun process-form1 (request-response)
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
         (fmt "Field value is '~a'" (get-request-parameter-value request-response "field")))
        (:div :class "NX_button_group"
         (:a :class "NX_button" :href (dynamic-url request-response 'forms1-start) "Back"))))))))

(defun forms1-f1 (x)
  (error x)
  'do-not-optimize)

(defun forms1-f2 (x)
  (forms1-f1 x)
  'do-not-optimize)

(defun forms1-f3 (x)
  (forms1-f2 x)
  'do-not-optimize)

(defun process-form2 (request-response)
  (let ((error-string (or (get-request-parameter-value request-response "error-string")
                          "forced-error")))
    (if (string-equal error-string "ok")
        (html-message request-response "OK" 
                      "We did not force an error, all is OK. Method is ~a"
                      (get-request-method request-response))
      ;; force an error
      (progn
        (forms1-f3 error-string)
        'do-not-optimize))))

;;;; eof
