;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: nkpax-paserve.asd,v 1.1 2004/11/10 15:20:55 sven Exp $
;;;;
;;;; The KPAX-PASERVE ASDF system definition
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

;; ASDF system definition

(defsystem :kpax-paserve
  :name "kpax-paserve"
  :author "Sven Van Caekenberghe, Beta Nine BVBA"
  :version "$Id: nkpax-paserve.asd,v 1.1 2004/11/10 15:20:55 sven Exp $"
  :licence "Copyright (C) 2004,2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved."
  :description "KPAX Web Application Framework for Portable Allegro Serve"
  :components
  ((:module 
    :src
    :components
    ((:file "paserve"))))
  :depends-on (:aserve :kpax-core))

;;;; eof
