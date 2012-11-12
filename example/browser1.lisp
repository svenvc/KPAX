;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: browser1.lisp,v 1.4 2004/09/17 08:08:48 sven Exp $
;;;;
;;;; A data browser example (a table with rows/columns, multiple pages, sorting)
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :browser1
  (:index 'browser1-index)
  (:static-root "static/")
  (:unsecure t))

(defun browser1-index (request-response)
  (html-page (out request-response)
    (:html 
     (:head 
      (:title "Data Browser")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:h1 "Data Browser")
      (:div :class "NX_button_group"
       (:a :class "NX_button" :href (dynamic-url request-response 'browser1-packages) "Packages")
       (:a :class "NX_button" :href (dynamic-url request-response 'browser1-numbers) "Numbers")
       (:a :class "NX_button" :href (dynamic-url request-response 'browser1-numbers-simple) "Numbers Simple"))))))

(defun package-nicknames-string (package)
  (format nil "~{~A~^, ~}" (package-nicknames package)))

(defun package-use-list-string (package)
  (format nil "~{~A~^, ~}" (mapcar #'package-name (package-use-list package))))

(defun package-used-by-list-string (package)
  (format nil "~{~A~^, ~}" (mapcar #'package-name (package-used-by-list package))))

(defun inspect-package (request-response)
  (let* ((package-name (get-request-parameter-value request-response "package-name"))
         (package (find-package package-name)))
    (if package
        (html-message request-response "Inspect" "Inspecting package ~a" (escape-string (princ-to-string package)))
      (html-message request-response "Error" "Package '~a' not found" package-name))))
 
(defun edit-package (request-response)
  (let* ((package-name (get-request-parameter-value request-response "package-name"))
         (package (find-package package-name)))
    (if package
        (html-message request-response "Edit" "Editing package ~a" (escape-string (princ-to-string package)))
      (html-message request-response "Error" "Package '~a' not found" package-name))))

(defun fake-delete-package (request-response)
  (let* ((package-name (get-request-parameter-value request-response "package-name"))
         (package (find-package package-name)))
    (if package
        (html-message request-response "Delete" "Deleting package ~a ;-)" (escape-string (princ-to-string package)))
      (html-message request-response "Error" "Package '~a' not found" package-name))))

(defun render-package-actions (package-name request-response)
  (html-part (out request-response)
    (:div :class "NX_vertical_button_group"
     (:a :class "NX_button" 
      :href (dynamic-url request-response 'inspect-package :package-name package-name) "Inspect")
     (:a :class "NX_button" 
      :href (dynamic-url request-response 'edit-package :package-name package-name) "Edit")
     (:a :class "NX_button" 
      :href (dynamic-url request-response 'fake-delete-package :package-name package-name) "Delete"))))

(defun make-packages-data-set-view ()
  (make-data-set-view (list-all-packages)
                      'browser1-packages
                      '((:view-position view-position)
                        (:data-set-position data-set-position)
                        (:name package-name 
                         :label "Name" :sortable t :sort-predicate string-lessp)
                        (:nicknames package-nicknames-string
                         :label "Nicknames")
                        (:use package-use-list-string
                         :label "Use")
                        (:used-by package-used-by-list-string
                         :label "Used-by")
                        (:actions package-name
                         :label "Actions"
                         :renderer render-package-actions))
                      :sortable t
                      :sort-column :name))

(defparameter *browser1-render-options* (make-options '(:table-class "NX_table"
                                                        :naviagtion-class "NX_navigation")
                                                      *default-data-set-render-options*))

(defun browser1-packages (request-response)
  (let ((data-set-view (get-attribute (get-session request-response) :packages-data-set-view)))
    (when (null data-set-view)
      (setf data-set-view (make-packages-data-set-view)
            (get-attribute (get-session request-response) :packages-data-set-view) data-set-view))
    (html-page (out request-response)
      (:html
       (:head 
        (:title "Packages")
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
       (:body (:h1 "Packages")
        (render data-set-view request-response *browser1-render-options*)
        (:div :class "NX_button_group" (:a :class "NX_button" :href (dynamic-url request-response nil) "Home")))))))

(defun number->decimal (n) 
  (write-to-string n :base 10))

(defun number->hexadecimal (n) 
  (write-to-string n :base 16))

(defun number->binary (n) 
  (write-to-string n :base 2))

(defun number->octal (n) 
  (write-to-string n :base 8))

(defun make-numbers-data-set-view ()
  (make-data-set-view  (loop :for i :below 512 :collect i)
                       'browser1-numbers
                       '((:view-position view-position
                          :label "View Order")
                         (:data-set-position data-set-position
                          :label "Data Order")
                         (:decimal number->decimal
                          :label "Decimal" :sortable t :sort-predicate string-lessp)
                         (:binary number->binary
                          :label "Binary" :sortable t :sort-predicate string-lessp)
                         (:decimal number->hexadecimal
                          :label "Hexadecimal" :sortable t :sort-predicate string-lessp)
                         (:octal number->octal
                          :label "Octal" :sortable t :sort-predicate string-lessp))
                       :sortable t
                       :page-size 32
                       :sort-column :decimal))

(defun browser1-numbers (request-response)
  (let ((data-set-view (get-attribute (get-session request-response) :numbers-data-set-view)))
    (when (null data-set-view)
      (setf data-set-view (make-numbers-data-set-view)
            (get-attribute (get-session request-response) :numbers-data-set-view) data-set-view))
    (html-page (out request-response)
      (:html
       (:head 
        (:title "Numbers")
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
       (:body (:h1 "Numbers")
        (render data-set-view request-response *browser1-render-options*)
        (:div :class "NX_button_group" (:a :class "NX_button" :href (dynamic-url request-response nil) "Home")))))))

(defun browser1-numbers-simple (request-response)
  (let ((data-set-view (get-attribute (get-session request-response) :numbers-simple-data-set-view)))
    (when (null data-set-view)
      (setf data-set-view (make-data-set-view (loop :for i :below 128 :collect i)
                                              'browser1-numbers-simple
                                              '((:decimal number->decimal)
                                                (:hexadecimal number->hexadecimal)
                                                (:octal number->octal)
                                                (:binary number->binary))
                                              :page-size 16)
            (get-attribute (get-session request-response) :numbers-simple-data-set-view) data-set-view))
    (html-page (out request-response)
      (:html
       (:head 
        (:title "Numbers Simple")
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
       (:body (:h1 "Numbers Simple")
        (render data-set-view request-response *browser1-render-options*)
        (:div :class "NX_button_group" (:a :class "NX_button" :href (dynamic-url request-response nil) "Home")))))))

;;;; eof
