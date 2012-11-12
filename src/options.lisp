;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: options.lisp,v 1.2 2004/10/05 13:56:23 sven Exp $
;;;;
;;;; A generic hierarchical option mechanism
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(make-options))

(defclass options ()
  ((key-value-list :initarg :key-value-list :initform nil)
   (parent :initarg :parent :initform nil))
  (:documentation "A generic hierarchical option mechanism"))

(defmethod get-option ((options options) key)
  (with-slots (key-value-list parent) options
    (let ((match (member key key-value-list)))
      (if match
          (second match)
        (when parent
          (get-option parent key))))))

(defun make-options (key-value-list &optional parent)
  "Combine the options in key-value-list with those in parent"
  (make-instance 'options :key-value-list key-value-list :parent parent))

;;;; eof