;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: tabs1.lisp,v 1.1 2004/09/21 09:18:56 sven Exp $
;;;;
;;;; Testing a generic tab system
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

;;; the menu1 example web application

(defparameter *tabs1-menu*
  (make-menu '(menu :menubar
                    :items ((menu-item :home :action tabs1-index)
                            (menu-item :red :action tabs1-red)
                            (menu-item :green :action tabs1-green)
                            (menu-item :blue :action tabs1-blue)))))
                            
(defwebapp :tabs1
  (:index 'tabs1-index)
  (:static-root "static/")
  (:unsecure t))

(defparameter *tabs1-menubar-render-options* 
  (make-menubar-render-options '(:always-use-anchors t)))

(defun tabs1-generic-page (request-response id)
  (let* ((menu-item (find-menu-item *tabs1-menu* id))
         (label (get-actual-label menu-item)))
    (html-page (out request-response)
      (:html 
       (:head 
        (:title (fmt "Tabs1 - ~a" label))
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
       (:body
        (:div :class "NX_tabbed_menubar"
         (render-tabs *tabs1-menu* request-response id *tabs1-menubar-render-options*)        
         (:div :class "NX_tab_content"
          (:p (fmt "You are currently on the page called '~a' with id ~s" label id))
          (:p "Actual instance: " (esc (prin1-to-string menu-item)))
          (:p "Please use the above tabs to navigate through the application"))))))))

(defun tabs1-index (request-response)
  (tabs1-generic-page request-response :home))

(defun tabs1-red (request-response)
  (tabs1-generic-page request-response :red))

(defun tabs1-green (request-response)
  (tabs1-generic-page request-response :green))

(defun tabs1-blue (request-response)
  (tabs1-generic-page request-response :blue))

;;;; eof
