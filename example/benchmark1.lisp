;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: benchmark1.lisp,v 1.4 2004/07/01 13:52:24 sven Exp $
;;;;
;;;; Implementation of the DW-Bench dynamic web application server benchmark
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :benchmark1
  (:index 'benchmark1)
  (:unsecure t))

(defconstant +benchmark1-title+ "DW-Bench Dynamic")

(defun benchmark1-timestamp (out)
  (s-utils:format-universal-time (get-universal-time) :stream out))

(defun benchmark1 (request-response)
  (html-page (out request-response :pprint-html nil)
    (:html 
     (:header (:title (str +benchmark1-title+)))
     (:body 
      (:h1 (str +benchmark1-title+))
      (:table :border 1
       (dotimes (i 25)
         (htm
          (:tr
           (dotimes (j 25)
             (htm
              (:td (str (* (1+ i) (1+ j))))))))))
      (:p (benchmark1-timestamp out))))))

;;;; eof