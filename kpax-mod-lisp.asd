;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: nkpax-mod-lisp.asd,v 1.1 2004/11/10 15:20:55 sven Exp $
;;;;
;;;; The KPAX-MOD-LISP ASDF system definition
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

;; ASDF system definition

(defsystem :kpax-mod-lisp
  :name "kpax-mod-lisp"
  :author "Sven Van Caekenberghe, Beta Nine BVBA"
  :version "$Id: nkpax-mod-lisp.asd,v 1.1 2004/11/10 15:20:55 sven Exp $"
  :licence "Copyright (C) 2004,2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved."
  :description "KPAX Web Application Framework for Mod-Lisp"
  :components
  ((:module 
    :src
    :components
    ((:file "mod-lisp"))))
  :depends-on (:kpax-core))

;;;; eof
