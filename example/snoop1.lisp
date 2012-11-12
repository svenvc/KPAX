;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: snoop1.lisp,v 1.5 2004/09/16 08:19:33 sven Exp $
;;;;
;;;; A basic web application that snoops headers & parameters from its request-response
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :snoop1
  (:index 'snoop1-start)
  (:static-root "static/")
  (:unsecure t))

(defun snoop1-start (request-response)
  (html-page (out request-response)
    (:html 
     (:head
      (:title "Snoop1!")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:h1 "Snoop1!")
      (:p (fmt "Request Method is ~a" (get-request-method request-response)))
      (:p (fmt "Request URI is ~a" (puri:render-uri (get-uri request-response) nil)))
      (:p (fmt "Request Protocol is ~a" (get-request-protocol request-response)))
      (:h2 "Request Headers")
      (:table :class "NX_table" :width "100%"
       (:tr (:th :width "20%" "Header Name") (:th "Value"))
       (dolist (header-name (get-request-headers request-response))
         (htm
          (:tr 
           (:td (str header-name))
           (:td (str (get-request-header-value request-response header-name)))))))
      (:h2 "Request Parameters")
      (:table :class "NX_table" :width "100%"
       (:tr (:th :width "20%" "Parameter Name") (:th "Value"))
       (dolist (parameter-name (get-request-parameters request-response))
         (htm
          (:tr 
           (:td (str parameter-name))
           (:td (str (get-request-parameter-value request-response parameter-name)))))))))))

;;;; eof