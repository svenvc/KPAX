;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: menubar.lisp,v 1.20 2005/06/14 08:53:52 sven Exp $
;;;;
;;;; A simple menubar framework
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export 
 '(make-menu menu menu-item 
   render-menubar make-menubar-render-options
   find-menu-item get-id get-actual-label get-items get-action
   render-tabs get-menu-item-path
   render-menubar-ie-js-code))

;;; a menu and menu-item data model with support functions

(defclass menu-item ()
  ((id :accessor get-id :initarg :id :initform nil)
   (label :accessor get-label :initarg :label :initform nil)
   (parent :accessor get-parent :initarg :parent :initform nil)
   (action :accessor get-action :initarg :action :initform nil)
   (description :accessor get-description :initarg :description :initform nil))
  (:documentation "A representation for a menu item"))

(defgeneric get-id (object)
  (:documentation "Get the id that uniquely identifies object"))

(defgeneric get-action (item)
  (:documentation "Get the action to perform for item"))

(defgeneric get-actual-label (item &optional render-options)
  (:documentation "Get the actual (translated using render-options) label to use for item"))

(defclass menu (menu-item)
  ((items :accessor get-items :initarg :items :initform nil))
  (:documentation "A representation for a menu"))

(defgeneric get-items (object)
  (:documentation "Return the items in object"))

(defmethod print-object ((menu-item menu-item) stream)
  (print-unreadable-object (menu-item stream :type t :identity t)
    (format stream "~A" (get-id menu-item))))

(defun make-menu (spec)
  "Make a new menu based on spec"
  (if (eql (type-of spec) 'menu)
      spec
    (destructuring-bind (kind id &key label items action description)
        spec
      (ecase kind
        (menu (let ((menu (make-instance 'menu 
                                         :id id :label label :action action :description description))
                    (items (mapcar #'make-menu items)))
                (dolist (item items) (setf (get-parent item) menu))
                (setf (get-items menu) items)
                menu))
        (menu-item (make-instance 'menu-item 
                                  :id id :label label :action action :description description))))))

(defun print-menu (menu &optional (level 4))
  (format t "~S ~S" (class-name (class-of menu)) (get-id menu))
  (etypecase menu
    (menu (terpri)
          (dolist (item (get-items menu))
            (dotimes (i level) (write-char #\space))
            (print-menu item (+ level 4))
            (terpri)))
    (menu-item (format t " :action ~A" (get-action menu))))
  (format t ""))

(defun find-menu-item (menu id)
  "Find the menu item with id in menu"
  (if (eql (get-id menu) id)
      menu
    (etypecase menu
      (menu (some #'(lambda (menu-item)
                      (find-menu-item menu-item id))
                  (get-items menu)))
      (menu-item nil))))

(defun find-top-menu (menu-item)
  (let ((parent (get-parent menu-item)))
    (if parent
        (find-top-menu parent)
      menu-item)))

(defun get-menu-item-path (menu id)
  "Return the path list in menu to the menu item with id"
  (if (eql (get-id menu) id)
      (list id)
    (etypecase menu
      (menu (let ((sub-path (some #'(lambda (menu-item)
                                      (get-menu-item-path menu-item id))
                                  (get-items menu))))
              (when sub-path
                (cons (get-id menu) sub-path))))              
      (menu-item nil))))

;;; menubar rendering

(defparameter *default-menubar-render-options* 
  (make-options '(:menu-item-renderer standard-menu-item-renderer 
                  :localizer identity
                  :div-menubar-class "menubar"
                  :li-selected-class "selected"
                  :render-not-selected-menus nil
                  :always-use-anchors nil
                  :render-empty-items nil)))

(defun make-menubar-render-options (key-value-list)
  "Make new menubar render options by combining key-value-list with *default-menubar-render-options*"
  (make-options key-value-list *default-menubar-render-options*))

(defun standard-menu-item-renderer (menu-item)
  (or (get-label menu-item) 
      (string-capitalize (get-id menu-item))))

(defmethod get-actual-label ((menu-item menu-item) &optional (render-options *default-menubar-render-options*))
  (let ((renderer (get-option render-options :menu-item-renderer))
        (localizer (get-option render-options :localizer)))
    (funcall localizer (funcall renderer menu-item))))

(defmethod render-menu ((menu-item menu-item) request-response selected-page-id render-options)
  (let ((label (get-actual-label menu-item render-options)))
    (html-part (out request-response :pprint-html nil)
      (if (eql (get-id menu-item) selected-page-id)
          (htm
           (:li :class (get-option render-options :li-selected-class)
            (if (get-option render-options :always-use-anchors)
                (htm (:a :href (dynamic-url request-response (get-action menu-item)) 
                      (str label)))
              (htm (str label)))))
        (when (or (get-action menu-item) (get-option render-options :render-empty-items))
          (htm
           (:li
            (if (get-action menu-item)
                (htm (:a :href (dynamic-url request-response (get-action menu-item)) (str label)))
              (htm (str label))))))))))

(defmethod render-menu ((menu menu) request-response selected-page-id render-options)
  (let ((label (get-actual-label menu render-options))
        (selected-menu (find-menu-item menu selected-page-id)))
    (html-part (out request-response :pprint-html nil)
      (if selected-menu
          (htm
           (:li :class (get-option render-options :li-selected-class)
            :id (format nil "~a-menu" (string-downcase label))
            (if (or (get-option render-options :always-use-anchors) 
                    (and (not (eq menu selected-menu)) (get-action menu)))
                (htm (:a :href (dynamic-url request-response (get-action menu)) (str label)))
              (htm (str label)))
            (:ul :id (format nil "~a-submenu" (string-downcase label))
             (dolist (menu-item (get-items menu))
               (render-menu menu-item request-response selected-page-id render-options)))))
        (let ((action (or (get-action menu)
                          (when (get-items menu) (get-action (first (get-items menu)))))))
          (htm
           (:li :id (format nil "~a-menu" (string-downcase label))
            (if action 
                (htm (:a :href (dynamic-url request-response action) (str label)))
              (htm (str label)))
            (when (get-option render-options :render-not-selected-menus)
              (htm
               (:ul :id (format nil "~a-submenu" (string-downcase label))
                (dolist (menu-item (get-items menu))
                  (render-menu menu-item request-response selected-page-id render-options))))))))))))

(defun render-menubar (menu request-response selected-page-id
                       &optional
                       (render-options *default-menubar-render-options*))
  "Render menu as menubar with selected-page-id in HTML to request-response using render-options"
  (html-part (out request-response :pprint-html nil)
    (:div :class (get-option render-options :div-menubar-class)
     (:ul :id (get-option render-options :ul-script-hook)            
      (dolist (menu-item (get-items menu))
        (render-menu menu-item request-response selected-page-id render-options))))
    (:div :style "clear:left;" "&nbsp;")))

(defun render-tabs (menu request-response selected-page-id
                    &optional
                    (render-options *default-menubar-render-options*))
  "Render menu as tabs with selected-page-id in HTML to request-response using render-options"
  (html-part (out request-response :pprint-html nil)
    (:ul :id (get-option render-options :ul-script-hook)            
     (dolist (menu-item (get-items menu))
       (render-menu menu-item request-response selected-page-id render-options)))))

;;; some code to make IE do the right thing

(defparameter *menubar-ie-js-code* 
"<!--//--><![CDATA[//><!--
startList = function() {
if (document.all&&document.getElementById) {
navRoot = document.getElementById(\"nav\");
for (i=0; i<navRoot.childNodes.length; i++) {
node = navRoot.childNodes[i];
if (node.nodeName==\"LI\") {
node.onmouseover=function() {
this.className+=\"over\";}
node.onmouseout=function() {
this.className=this.className.replace(\"over\", \"\");}
}}}}
window.onload=startList; //--><!]]>")

(defun render-menubar-ie-js-code (request-response)
  "Render the necessary IE-specific JavaScript code for menubars" 
  (html-part (out request-response)
    (:script :type "text/javascript" (str *menubar-ie-js-code*))))

;;;; eof
