;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: html.lisp,v 1.14 2004/11/26 13:23:26 sven Exp $
;;;;
;;;; HTML generation support
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(generate-submit
   generate-text-field
   generate-password-field
   generate-hidden-field
   generate-text-area
   generate-checkbox
   generate-radiobutton
   generate-select))

(defun generate-submit (request-response label &optional name)
  "Generate a submit type input tag"
  (html-part (out request-response)
    (:input :type "submit" :value label :name name)))

(defun generate-label (request-response
                       name
                       id
                       value)
  "Generate a label tag"
  (html-part (out request-response)
    (:label
     :id id
     :name name
     :value (escape-string (princ-to-string value)))))

(defun generate-text-field (request-response
                            name
                            value
                            &key
                            size
                            maxlength
                            readonly
                            onblur)
  "Generate a text type form input tag"
  (html-part (out request-response)
    (:input
     :type "text"
     :name name
     :value (escape-string (princ-to-string value))
     :size size
     :maxlength maxlength
     :readonly (when readonly "readonly")
     :onblur onblur)))

(defun generate-password-field (request-response
                                name
                                value
                                &key
                                size
                                maxlength
                                readonly)
  "Generate a password type form input tag"
  (html-part (out request-response)
    (:input
     :type "password"
     :name name
     :value (escape-string (princ-to-string value))
     :size size
     :maxlength maxlength
     :readonly (when readonly "readonly"))))

(defun generate-hidden-field (request-response
                              name
                              value)
  "Generate a hidden type form input tag"
  (html-part (out request-response)
    (:input
     :type "hidden"
     :name name
     :value (escape-string (princ-to-string value)))))

(defun generate-text-area (request-response
                           name
                           value
                           &key
                           cols
                           rows
                           readonly)
  "Generate a textarea form tag"
  (html-part (out request-response :pprint-html nil)
    (:textarea 
     :name name
     :rows rows
     :cols cols
     :readonly (when readonly "readonly")
     (str value))))

(defun generate-checkbox (request-response 
                          name 
                          value 
                          &key 
                          (true-value "T") 
                          (checked-value "checked") 
                          label
                          id
                          onchange)
  "Generate a checkbox type form input tag with optional extra label"
  (html-part (out request-response)
    (:input
     :type "checkbox" 
     :name name 
     :id id
     :value true-value 
     :checked (when value checked-value)
     :onchange onchange)
    (when label (htm (str label)))))

(defun generate-radiobutton (request-response 
                             name 
                             value 
                             &key 
                             (true-value "T") 
                             (checked-value "checked") 
                             label)
  "Generate a radio type form input tag with optional label"
  (html-part (out request-response)
    (:input
     :type "radio" 
     :name name 
     :value true-value 
     :checked (when value checked-value))
    (when label (htm (str label)))))

(defun generate-select (request-response
                        name
                        values-list
                        current-value
                        &key
                        (selected-value "selected")
                        multiple
                        (multiple-value "multiple")
                        size
                        labels
                        onchange
                        (test #'eql))
  "Generate a select tag with nested option tags, indicating the current selection"
  (html-part (out request-response)
    (:select 
     :name name 
     :size size 
     :multiple (when multiple multiple-value)
     :onchange onchange
     (loop :for value :in values-list
           :for label :in (or labels values-list)
           :do
           (htm
            (:option 
             :value value
             :selected (when (if multiple 
                                 (member value current-value)
                               (funcall test value current-value)) 
                         selected-value)
             (when label (htm (str label)))))))))
       
;;;; eof
