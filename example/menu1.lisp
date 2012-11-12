;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: menu1.lisp,v 1.12 2005/06/14 08:53:52 sven Exp $
;;;;
;;;; Testing a generic menu system
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

;;; the menu1 example web application

(defparameter *menu1-menu*
  (make-menu '(menu :menubar
                    :items ((menu-item :home :action menu1-index)
                            (menu-item :red :action menu1-red)
                            (menu-item :green :action menu1-green)
                            (menu-item :blue :action menu1-blue)
                            (menu :more 
                                  :label "Extra" 
                                  :action menu1-more
                                  :items ((menu-item :foo :action menu1-foo)
                                          (menu-item :bar :action menu1-bar)))
                            (menu :help
                                  :items ((menu-item :basic-help :action menu1-basic-help)
                                          (menu-item :about :action menu1-about)))))))
                            
(defwebapp :menu1
  (:index 'menu1-index)
  (:static-root "static/")
  (:unsecure t))

(defparameter *menu1-menubar-render-options* 
  (make-menubar-render-options '(:div-menubar-class "NX_menubar"
                                 :ul-script-hook "nav"
                                 :render-not-selected-menus t
                                 :li-selected-class "selected"
                                 :always-use-anchors t)))

(defun menu1-generic-page (request-response id)
  (let* ((menu-item (find-menu-item *menu1-menu* id))
         (label (get-actual-label menu-item *menu1-menubar-render-options*)))
    (html-page (out request-response)
      (:html 
       (:head 
        (:title (fmt "Menu1 - ~a" label))
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css"))
        (render-menubar-ie-js-code request-response))
       (:body
        (render-menubar *menu1-menu* request-response id *menu1-menubar-render-options*)        
        (:h1 :style "clear: both; margin-top: 50px;"
         (fmt "Menu1 - ~a" label))
        (:div  
         (:div :class "NX_panel"
          (:span :class "NX_title" "Page")
          (:div :class "NX_border"
           (:p (fmt "You are currently on the page called '~a' with id ~s" label id))
           (:p (fmt "Current path to this page is ~s" (get-menu-item-path *menu1-menu* id)))
           (:p "Actual instance: " (esc (prin1-to-string menu-item)))
           (:p "Please use the above menu to navigate through the application")))))))))

(defun menu1-index (request-response)
  (menu1-generic-page request-response :home))

(defun menu1-red (request-response)
  (menu1-generic-page request-response :red))

(defun menu1-green (request-response)
  (menu1-generic-page request-response :green))

(defun menu1-blue (request-response)
  (menu1-generic-page request-response :blue))

(defun menu1-basic-help (request-response)
  (menu1-generic-page request-response :basic-help))

(defun menu1-about (request-response)
  (menu1-generic-page request-response :about))

(defun menu1-more (request-response)
  (menu1-generic-page request-response :more))

(defun menu1-foo (request-response)
  (menu1-generic-page request-response :foo))

(defun menu1-bar (request-response)
  (menu1-generic-page request-response :bar))

;;;; eof
