;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: nkpax.asd,v 1.26 2004/11/10 15:20:55 sven Exp $
;;;;
;;;; The KPAX ASDF system definition
;;;;
;;;; Copyright (C) 2004,2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
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

(defsystem :kpax
  :name "kpax"
  :author "Sven Van Caekenberghe, Beta Nine BVBA"
  :version "$Id: nkpax.asd,v 1.26 2004/11/10 15:20:55 sven Exp $"
  :licence "Copyright (C) 2004,2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved."
  :description "KPAX Web Application Framework"
  :depends-on (:kpax-mod-lisp :kpax-s-http-server))

;;;; eof
