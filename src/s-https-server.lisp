;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: mod-lisp.lisp,v 1.30 2005/10/11 14:29:22 sven Exp $
;;;;
;;;; Implementation of web server and request-response interface using the S-HTTPS-SERVER
;;;;
;;;; Copyright (C) 2006 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(s-https-server))

;; Objects representing the bridge between KPAX and S-HTTPS-SERVER

(defclass s-https-server (s-http-server)
  ((s-http-server :reader get-s-http-server :initarg :s-http-server :initform (s-http-server:make-s-https-server)))
  (:documentation "S-HTTPS-SERVER implementation of the KPAX web application server protocol"))

(defmethod get-scheme ((web-app-server s-https-server))
  "https")

;; Example SSL context:

#+nil
(s-http-server:make-ssl-context :certificate "/Users/sven/darcs/s-http-server/rsrc/test-server.crt"
                                :private-key "/Users/sven/darcs/s-http-server/rsrc/test-server.key"
                                :dhparam "/Users/sven/darcs/s-http-server/rsrc/dhparam.pem"
                                :password "123456")

;;;; eof
