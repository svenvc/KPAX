;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: globals.lisp,v 1.1 2004/01/29 10:27:58 sven Exp $
;;;;
;;;; Definition of globals used by KPAX
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(*web-app-server*))

(defvar *web-apps* (make-hash-table)
  "A map from symbol to web application object")

(defvar *web-app-servers* ()
  "A list of known web application server object")

(defvar *web-app-server* nil
  "A handle to the current default web application server object")

;;;; eof