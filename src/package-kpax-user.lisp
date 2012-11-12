;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: package-kpax-user.lisp,v 1.3 2004/01/27 16:28:46 sven Exp $
;;;;
;;;; Definition of the KPAX-USER package
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :cl-user)

(defpackage :kpax-user
  (:use #:common-lisp
	#:kpax
        #:cl-who)
  (:documentation "The KPAX-USER package"))

;;;; eof
