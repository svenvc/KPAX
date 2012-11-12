;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: nkpax-core.asd,v 1.3 2005/06/14 11:37:50 sven Exp $
;;;;
;;;; The KPAX-CORE ASDF system definition
;;;;
;;;; Copyright (C) 2004-2007 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

;; we specify some of our pathnames relative to the logical host "home"
;; openmcl already has this host configured, add it for others when it's absent

#-cmu
(unless (ignore-errors (logical-pathname-translations "home"))
  (setf (logical-pathname-translations "home")
        `(("**;*.*.*" ,(concatenate 'string (namestring (user-homedir-pathname)) "**/*.*")))))

;; ASDF system definition

(defsystem :kpax-core
  :name "kpax-core"
  :author "Sven Van Caekenberghe, Beta Nine BVBA"
  :version "$Id: nkpax-core.asd,v 1.3 2005/06/14 11:37:50 sven Exp $"
  :licence "Copyright (C) 2004-2007 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved."
  :description "KPAX Web Application Framework Core"
  :components
  ((:module 
    :src
    :components
    (;; level 0 
     (:file "package-kpax")
     (:file "log" :depends-on ("package-kpax"))
     (:file "util" :depends-on ("package-kpax"))
     (:file "localization" :depends-on ("package-kpax")) ;; strings-en.lisp, string-fr.lisp & strings-nl.lisp
     (:file "mersenne-twister-random")
     (:file "crypto" :depends-on ("package-kpax" "mersenne-twister-random"))
     ;; level 1
     (:file "html-macro" :depends-on ("package-kpax"))
     (:file "html" :depends-on ("html-macro"))
     (:file "globals" :depends-on ("package-kpax"))
     (:file "constants" :depends-on ("package-kpax"))
     (:file "attributes-mixin" :depends-on ("package-kpax"))
     (:file "session" :depends-on ("attributes-mixin"))
     (:file "server" :depends-on ("session" "log"))
     (:file "request-response" :depends-on ("attributes-mixin" "log" "util"))
     (:file "translator" :depends-on ("request-response"))
     (:file "web-app-in-server" :depends-on ("attributes-mixin" "server" "globals"))
     (:file "session-tracking" :depends-on ("session" "request-response"))
     (:file "web-app" :depends-on ("server" "request-response" "util" "html" "web-app-in-server" "session-tracking"))
     (:file "dispatcher" :depends-on ("request-response" "globals" "html"))
     (:file "debug" :depends-on ("server" "web-app" "dispatcher" "request-response" "constants"))
     (:file "kpax" :depends-on ("server" "request-response" "html" "globals"))
     ;; level 2
     (:file "run" :depends-on ("kpax"))
     (:file "multipart-form-encoding" :depends-on ("kpax"))
     (:file "extensions" :depends-on ("kpax"))
     (:file "options" :depends-on ("kpax"))
     (:file "constraints" :depends-on ("package-kpax"))
     (:file "menubar" :depends-on ("kpax" "options"))
     (:file "data-set-view" :depends-on ("kpax" "options"))
     (:file "web-form" :depends-on ("constraints" "kpax" "options"))
     (:file "package-kpax-user" :depends-on ("kpax")))))
  :depends-on (:cl-who :puri :ironclad :s-utils :s-sysdeps :s-base64))

;;;; eof
