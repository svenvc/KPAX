;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: html-macro.lisp,v 1.6 2004/11/30 15:00:37 nicky Exp $
;;;;
;;;; HTML generation support (macro's)
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(html-part
   html-page
   *pprint-html*
   *html-prologue*))

(defvar *pprint-html* nil
  "Controls pretty printing of generated HTML (used at macro-expansion/compile time)")

(defvar *html-prologue* 
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
  "The contents of this string is inserted before any HTML page (used at macro-expansion/compile time)")

(defmacro html-page ((var request-response &key (pprint-html *pprint-html*) (html-prologue *html-prologue*)) &body body)
  "Generate contents for a whole page"
  `(with-html-output (,var 
                      (get-content-stream ,request-response) 
                      :prologue ,html-prologue 
                      :indent ,pprint-html)
     ,@body))

(defmacro html-part ((var request-response &key (pprint-html *pprint-html*)) &body body)
  "Generate contents for a part"
  `(with-html-output (,var
                      (get-content-stream ,request-response)
                      :indent ,pprint-html)
     ,@body))

;;;; eof
