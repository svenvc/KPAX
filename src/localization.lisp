;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: localization.lisp,v 1.4 2004/10/05 13:56:23 sven Exp $
;;;;
;;;; Localization support
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(*locale*
   *localization-source*
   localized-string
   localized-format))

;;; the public interface

(defvar *locale* nil 
  "Special variable to be bound to a locale specification object")

(defvar *localization-source* nil
  "Special variable to be bound to an object to be used as localization source")

(defun localized-string (key &rest args)
  "Translate key to a string in the context of *locale* and *localization-source*, using args to format"
  (apply #'localized-format nil key args))

(defun localized-format (stream key &rest args)
  "Translate key to stream in the context of *locale* and *localization-source*, using args to format"
  (declare (special *locale*) (special *localization-source*))
  (let ((localized-string (get-localized-string *localization-source* *locale* key)))
    (if localized-string
        (apply #'format stream localized-string args)
      (princ-to-string key))))

;;; possible extensions

;; localized-format-universal-time
;; localized-format-duration
;; localized-format-number
;; localized-format-money

;;; interface between the public interface and the implementation

(defgeneric get-localized-string (localization-source locale key)
  (:documentation "Translate key in the context of locale and localization-source")
  (:method (localization-source locale key)
   (declare (ignore localization-source locale key))
   nil))

;;; a simple implementation

(export
 '(init-localized-strings
   *simple-localization-source*))

(defclass simple-localization-source ()
  ((map :initform (make-hash-table)))
  (:documentation "Holds a mapping of locale keys to a mapping if keys to strings"))

(defmethod get-localized-string ((localization-source simple-localization-source) locale key)
  (with-slots (map)
      localization-source
    (let ((string-map (gethash locale map)))
      (when string-map
        (gethash key string-map)))))

(defmethod (setf get-localized-string) (string (localization-source simple-localization-source) locale key)
  (with-slots (map)
      localization-source
    (let ((string-map (gethash locale map)))
      (when (not string-map)
        (setf string-map (setf (gethash locale map) (make-hash-table))))
      (setf (gethash key string-map) string))))

(defmethod reset ((localization-source simple-localization-source))
  (with-slots (map)
      localization-source
    (clrhash map)))

(defmethod load-localized-strings ((localization-source simple-localization-source) locale alist)
  (loop :for (key value) :in alist
        :do (setf (get-localized-string localization-source locale key) value)))

(defparameter *default-strings-location* *load-truename*)

(defun load-localized-strings-file (filename &optional (location *default-strings-location*))
  (let ((path (merge-pathnames filename location)))
    (when (probe-file path)
      (with-open-file (in path)
        (let ((alist (read in)))
          alist)))))

(defvar *simple-localization-source* (make-instance 'simple-localization-source)
  "A builtin, simple localization source")

(defun init-localized-strings (&optional (location *default-strings-location*) (locales '(:nl :fr :en)))
  "Initialize the builtin, simple localization source using location and locales"
  (reset *simple-localization-source*)
  (dolist (locale locales)
    (load-localized-strings *simple-localization-source* 
                            locale 
                            (load-localized-strings-file (format nil "strings-~a.lisp" (string-downcase locale))
                                                         location)))
  *simple-localization-source*)

;;; translation maintenance

(defmethod dump-alist ((localization-source simple-localization-source) locale)
  (with-slots (map) 
      localization-source
    (let ((string-map (gethash locale map)))
      (when string-map
        (let ((keys (sort (loop :for k :being :the :hash-keys :in string-map :collect k) #'string-lessp)))
          (loop :for k :in keys :collect (list k (or (gethash k string-map) ""))))))))

(defmethod dump-multi-alist ((localization-source simple-localization-source) locales)
  (with-slots (map) 
      localization-source
    (let ((keys (sort (reduce #'union 
                              (mapcar #'(lambda (locale) 
                                          (let ((string-map (gethash locale map)))
                                            (when string-map
                                              (loop :for k :being :the :hash-keys :in string-map :collect k))))
                                      locales))
                      #'string-lessp)))
      (loop :for k :in keys :collect (cons k (mapcar #'(lambda (locale)
                                                         (or (let ((string-map (gethash locale map)))
                                                               (when string-map
                                                                 (gethash k string-map)))
                                                             ""))
                                                     locales))))))

(defmethod load-multi-localized-strings ((localization-source simple-localization-source) locale alist index)
  (loop :for (key . rest) :in alist
        :do (setf (get-localized-string localization-source locale key) (elt rest index))))

;;;; eof