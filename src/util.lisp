;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: util.lisp,v 1.15 2005/06/17 15:02:43 sven Exp $
;;;;
;;;; General utilities
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(uri-encode-for-query
   uri-decode-for-query
   maptree))

(defun maptree (function tree)
  "Apply function to all elements in tree (a cons), returning a new isomorphic tree with the results"
  (cond ((null tree) nil)
        ((atom tree) (funcall function tree))
        (t (cons (maptree function (car tree))
                 (maptree function (cdr tree))))))

;; our own uri encoding implementation (ascii only, not that efficient, eager to be safe)

(defparameter +uri-encode-char-map+
  (let ((map (make-array '(127) :element-type 'boolean)))
    (flet ((allow-range (from to)
             (loop :for i :upfrom (char-code from) :upto (char-code to) 
                   :do (setf (aref map i) t))))
      (allow-range #\0 #\9)
      (allow-range #\A #\Z)
      (allow-range #\a #\z)
      (loop :for char :across "-_.!~*'()" ;; these are the 'mark' characters from the 'unreserved' set
            :do (setf (aref map (char-code char)) t))
      map)))

(defun uri-encode-for-query (string &key (signal-errors t))
  "URI encode string for use as a query parameter value"
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop
       :for char = (read-char in nil nil)
       :until (null char)
       :do
       (let ((code (char-code char)))
         (cond ((>= code 256) (if signal-errors 
                                  (error "non-ascii char") 
                                (format out "%3F")))
               ((or (>= code 127)
                    (not (aref +uri-encode-char-map+ code)))
                (format out "%~2,'0x" code))
               (t (write-char char out))))))))

(defun uri-decode-for-query (string)
  "URI decode string from a query parameter value"
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop 
       :for char = (read-char in nil nil)
       :until (null char)
       :do
       (if (char= char #\%)
           (let ((char1 (read-char in nil nil))
                 (char2 (read-char in nil nil)))
             (if (and char1 char2)
                 (write-char (code-char (parse-integer (map 'string #'identity (list char1 char2)) 
                                                       :radix 16)) 
                             out)
               (error "incomplete escape sequence")))
         (write-char char out))))))

;;;; eof