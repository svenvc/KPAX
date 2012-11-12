;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: attributes-mixin.lisp,v 1.4 2004/06/16 14:46:16 sven Exp $
;;;;
;;;; A mixin that adds named attributes.
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(get-attribute
   get-attributes
   clear-attributes))

(defclass attributes-mixin ()
  ((attributes :initform nil))
  (:documentation "This mixin adds named attributes"))

(defgeneric get-attribute (object key)
  (:documentation "Get the attribute value for name in object")
  (:method ((object attributes-mixin) key)
   (with-slots (attributes) 
       object
     (cdr (assoc key attributes :test #'string-equal)))))

(defgeneric (setf get-attribute) (value attributes-mixin key)
  (:documentation "Set the value of the attribute for key in object")
  (:method (value (object attributes-mixin) key)
   (with-slots (attributes) 
       object
     (let ((pair (assoc key attributes :test #'string-equal)))
       (if pair
           (setf (cdr pair) value)
         (push (cons key value) attributes))
       value))))

(defgeneric get-attributes (object)
  (:documentation "Return the list of all attribute keys known by object")
  (:method ((object attributes-mixin))
   (with-slots (attributes)
       object
     (mapcar #'car attributes))))

(defgeneric clear-attributes (object)
  (:documentation "Clear all named attributes in object")
  (:method ((object attributes-mixin))
   (with-slots (attributes)
       object
       (setf attributes nil))))

;;;; eof