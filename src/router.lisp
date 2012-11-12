;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: dispatcher.lisp,v 1.25 2004/11/08 13:51:08 sven Exp $
;;;;
;;;; The router (if any) helps the dispatcher in deciding which combination
;;;; of web-app and web-function gets to handle a request-response by looking
;;;; at the request's URI pattern and matching it from a preconfigured map.
;;;; Some URI parts could be transformed to request parameters.
;;;; The router handles an ordered collection of routes.
;;;; Handling by the router and routes is bi-directional: 
;;;; - from request URI to web-app, web-function and/or arguments
;;;; - from web-app, web-function and/or arguments to URI
;;;;
;;;; Copyright (C) 2007 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export 
 '())

(defclass router ()
  (routes)
  (:documentation "I map URI request patterns to web-apps, web-functions and/or arguments using routes"))

(defclass route ()
  ()
  (:documentation "I map a single URI request pattern to a web-app, web-function and/or arguments"))
 
;;;; eof
