;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: translator.lisp,v 1.3 2004/05/26 15:27:56 sven Exp $
;;;;
;;;; Translators help to deal with request-response parameter parsing, converting and error handling
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(make-translator
   translate
   translate-integer
   translate-keyword))

(defclass translator ()
  ((map :accessor get-map :initform (make-hash-table :test #'equal)))
  (:documentation "A translator helps to parse, convert and error handle request-response parameters"))

(defmethod load-translations ((translator translator) list)
  (when list
    (let ((parameter (car list))
          (translation (cadr list)))
      (setf (gethash parameter (get-map translator)) translation)
      (load-translations translator (cddr list)))))

(defun make-translator (&rest args)
  "Make a new translator"
  (let ((translator (make-instance 'translator)))
    (load-translations translator args)
    translator))

(defun translate-integer (value)
  "Translate value to an integer"
  (when value
    (parse-integer value :junk-allowed t)))

(defun translate-keyword (value)
  "Translate value to a keyword"
  (when value
    (find-symbol (string-upcase value) :keyword)))

(defgeneric translate (translator request-response parameter)
  (:documentation "Translate parameter from request-response using translator")
  (:method ((translator translator) (request-response request-response) parameter)
   (let ((value (get-request-parameter-value request-response parameter))
         (translation (gethash parameter (get-map translator))))
     (if translation
         (funcall translation value)
       value))))

;;;; eof