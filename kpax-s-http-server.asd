;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: nkpax-mod-lisp.asd,v 1.1 2004/11/10 15:20:55 sven Exp $
;;;;
;;;; The KPAX-S-HTTP-SERVER ASDF system definition
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

;; ASDF system definition

(defsystem :kpax-s-http-server
  :name "kpax-s-http-server"
  :author "Sven Van Caekenberghe, Beta Nine BVBA"
  :version "$Id: nkpax-mod-lisp.asd,v 1.1 2004/11/10 15:20:55 sven Exp $"
  :licence "Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved."
  :description "KPAX Web Application Framework for S-HTTP-SERVER"
  :components
  ((:module 
    :src
    :components
    ((:file "s-http-server")
     #+lispworks (:file "s-https-server" :depends-on ("s-http-server")))))
  :depends-on (:kpax-core :s-http-server))

;;;; eof
