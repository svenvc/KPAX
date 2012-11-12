;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: package-kpax.lisp,v 1.2 2004/01/27 16:28:46 sven Exp $
;;;;
;;;; Definition of the KPAX package
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :cl-user)

(defpackage :kpax
  (:use #:common-lisp #:cl-who)
  (:documentation "The KPAX Web Application Framework"))

;;;; eof
