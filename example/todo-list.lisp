;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: todo-list.lisp,v 1.4 2005/04/25 07:40:30 sven Exp $
;;;;
;;;; This web app is a GUI to a Todo List.
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :todo-list
  (:index 'todo-list-index)
  (:static-root "static/")
  (:unsecure t))

(defparameter *todo-categories* '(:home :work))

(defvar *todo-list-items-id-counter* 0)

(defclass todo-list-item ()
  ((id :accessor get-id :initarg :id :initform (incf *todo-list-items-id-counter*))
   (text :accessor get-text :initarg :text :initform nil)
   (category :accessor get-category :initarg :category :initform :work)))

(defmethod print-object ((object todo-list-item) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "#~d '~a' [~a]" (get-id object) (get-text object) (get-category object))))

(defvar *todo-list* '())

(defun todo-list-index (request-response &key edit)
  (html-page (out request-response :pprint-html nil)
    (:html 
     (:head
      (:title "KPAX Todo List")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:h1 "KPAX Todo List")
      (:p "This is your Todo List:")
      (:form :class "NX_form"
       (:caption "Categories Filter:")
       (generate-checkbox request-response :show-home t :label "Home")
       (generate-checkbox request-response :show-work t :label "Work"))
      (if (null *todo-list*)
          (htm
           (:div :class "NX_button_group" 
            (:a :class "NX_button" :href (dynamic-url request-response 'todo-list-new-first-item) "New")))
        (htm 
         (:ol
          (let ((last-index (1- (length *todo-list*)))
                (clipboard (get-attribute (get-session request-response) :clipboard)))
            (loop 
             :for item :in *todo-list*
             :for i :upfrom 0
             :do
             (htm
              (:li 
               (if (eql item edit)
                   (htm
                    (:form :action (dynamic-url request-response 'todo-list-commit-edit) :method :post
                     (generate-hidden-field request-response :id (get-id item))
                     (generate-text-field request-response :text (get-text item) :size 32)
                     (generate-select request-response :category *todo-categories* (get-category item))
                     (:input :type :submit :value "OK")
                     (:input :type :reset :value "RESET")))
                 (htm
                  (fmt "~a [~a]" (get-text item) (get-category item))))
               (:div :class "NX_button_group"
                (if (eql item edit)
                    (htm (:a :class "NX_button" :href (dynamic-url request-response nil) "Cancel"))
                  (htm (:a :class "NX_button" :href (dynamic-url request-response 'todo-list-edit-item :id (get-id item)) "Edit")))
                (:a :class "NX_button" :href (dynamic-url request-response 'todo-list-new-item-after :id (get-id item)) "New")
                (if (zerop i) 
                    (htm "Up")
                  (htm (:a :class "NX_button" :href (dynamic-url request-response 'todo-list-move-item-up :id (get-id item)) "Up")))
                (if (= i last-index)
                    (htm "Down")
                  (htm (:a :class "NX_button" :href (dynamic-url request-response 'todo-list-move-item-down :id (get-id item)) "Down")))
                (:a :class "NX_button" :href (dynamic-url request-response 'todo-list-cut-item :id (get-id item)) "Cut")              
                (:a :class "NX_button" :href (dynamic-url request-response 'todo-list-copy-item :id (get-id item)) "Copy")
                (if clipboard
                    (htm (:a :class "NX_button" :href (dynamic-url request-response 'todo-list-paste-item-after :id (get-id item)) "Paste"))
                  (htm "Paste"))
                (:a :class "NX_button" :href (dynamic-url request-response 'todo-list-delete-item :id (get-id item)) "Delete")
                ))))))))
      (:div :class "NX_button_group"
       (loop :for (title . action) :in '(("Refresh" . nil) 
                                         ("Clear All" . not-yet-implemented) 
                                         ("Undo" . not-yet-implemented)
                                         ("Redo" . not-yet-implemented)
                                         ("Show Clipboard" . not-yet-implemented))
             :do (htm
                  (:a :class "NX_button" :href (dynamic-url request-response action) (str title)))))))))

(defun get-item (request-response)
  (let ((id (s-utils:parse-integer-safely (get-request-parameter-value request-response :id))))
    (find id *todo-list* :key #'get-id)))

(defun clone-item (item)
  (make-instance 'todo-list-item :text (get-text item) :category (get-category item)))

(defun todo-list-edit-item (request-response)
  (let ((item (get-item request-response)))
    (if item
        (todo-list-index request-response :edit item)
      (todo-list-index request-response))))

(defun todo-list-new-first-item (request-response)
  (let ((new-item (make-instance 'todo-list-item)))
    (push new-item *todo-list*)
    (todo-list-index request-response :edit new-item)))

(defun todo-list-new-item-after (request-response)
  (let* ((item (get-item request-response))
         (sublist (member item *todo-list*))
         (new-item (make-instance 'todo-list-item)))
    (if (and item sublist)
        (progn
          (setf (rest sublist) (cons new-item (rest sublist)))
          (todo-list-index request-response :edit new-item))
      (todo-list-index request-response))))

(defun todo-list-delete-item (request-response)
  (let ((item (get-item request-response)))
    (when item (setf *todo-list* (delete item *todo-list*)))
    (todo-list-index request-response)))

(defun todo-list-move-item-up (request-response)
  (let* ((item (get-item request-response))
         (position (position item *todo-list*)))
    (when (and item position (> (length *todo-list*) 1))
      (if (zerop position)
          (let ((tmp (first *todo-list*)))
            (setf (first *todo-list*) (second *todo-list*)
                  (second *todo-list*) tmp))
        (let* ((sublist (nthcdr (1- position) *todo-list*))
               (tmp (first sublist)))
          (setf (first sublist) (second sublist)
                (second sublist) tmp))))
    (todo-list-index request-response)))

(defun todo-list-move-item-down (request-response)
  (let* ((item (get-item request-response))
         (sublist (member item *todo-list*)))
    (when (and item sublist (rest sublist))
      (let ((tmp (first sublist)))
        (setf (first sublist) (second sublist)
              (second sublist) tmp)))
    (todo-list-index request-response)))

(defun todo-list-copy-item (request-response)
  (let ((item (get-item request-response)))
    (when item
      (setf (get-attribute (get-session request-response) :clipboard) item))
    (todo-list-index request-response)))

(defun todo-list-paste-item-after (request-response)
  (let* ((item (get-item request-response))
         (sublist (member item *todo-list*))
         (clipboard (get-attribute (get-session request-response) :clipboard)))
    (when (and item clipboard sublist)
      (setf (rest sublist) (cons (clone-item clipboard) (rest sublist))))
    (todo-list-index request-response)))

(defun todo-list-cut-item (request-response)
  (let ((item (get-item request-response)))
    (when item
      (setf *todo-list* (delete item *todo-list*))
      (setf (get-attribute (get-session request-response) :clipboard) item))
    (todo-list-index request-response)))

(defun todo-list-commit-edit (request-response)
  (let ((item (get-item request-response))
        (text (get-request-parameter-value request-response :text))
        (category (find-symbol (get-request-parameter-value request-response :category) :keyword)))
    (when (and item text category)
      (setf (get-text item) text
            (get-category item) category))
    (todo-list-index request-response)))

;;;; eof
