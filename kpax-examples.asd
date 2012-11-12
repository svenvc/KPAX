;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: nkpax-examples.asd,v 1.15 2004/12/16 10:36:52 sven Exp $
;;;;
;;;; The KPAX examples ASDF system definition
;;;;
;;;; Copyright (C) 2004,2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

;; ASDF system definition

(defsystem :kpax-examples
  :name "kpax-examples"
  :author "Sven Van Caekenberghe, Beta Nine BVBA"
  :version "$Id: nkpax-examples.asd,v 1.15 2004/12/16 10:36:52 sven Exp $"
  :licence "Copyright (C) 2004,2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved."
  :description "KPAX Web Application Framework Examples"
  :components
  ((:module 
    :example
    :components
    ((:file "helloworld1")
     (:file "factorial1")
     (:file "session1")
     (:file "no-session1")
     (:file "snoop1")
     (:file "forms1")
     (:file "forms2")
     (:file "benchmark1")
     (:file "todo-list")
     (:file "secure1")
     (:file "upload1")
     (:file "upload2")
     (:file "upload3")
     (:file "calculator")
     (:file "list-web-apps")
     (:file "menu1")
     (:file "tabs1")
     (:file "browser1")
     (:file "secure-login")
     (:file "pi")
     (:file "lsp")
     (:file "wiki")
     (:file "reddit")
     (:file "welcome"))))
  :depends-on (:kpax :s-http-client))

;;;; eof
