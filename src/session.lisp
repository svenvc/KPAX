;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: session.lisp,v 1.12 2004/10/05 13:56:23 sven Exp $
;;;;
;;;; Web application session management 
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(get-session-id
   get-created-timestamp
   get-last-modified-timestamp
   get-number-of-requests
   get-session-manager
   get-all-sessions
   get-session-count
   get-session-lifetime
   find-session
   touch
   reap-old-sessions
   invalidate-all-sessions))

(defclass session (attributes-mixin)
 ((id :reader get-session-id :initarg :id)
  (created-timestamp :reader get-created-timestamp :initform (get-universal-time))
  (last-modified-timestamp :accessor get-last-modified-timestamp :initform (get-universal-time))
  (number-of-requests :accessor get-number-of-requests :initform 0)
  (session-manager :accessor get-session-manager :initarg :session-manager))
 (:documentation "A web application session maintained over different response-request cycles"))

(defgeneric get-session-id (session)
  (:documentation "Return the id that uniquely identifies this session"))

(defgeneric get-created-timestamp (session)
  (:documentation "Return the universal time when this session was created"))

(defgeneric get-last-modified-timestamp (session)
  (:documentation "Return the universal time when this session was last modified ('touched')"))

(defgeneric get-number-of-requests (session)
  (:documentation "Return the number of request this session handled in its lifetime"))

(defgeneric touch (object)
  (:documentation "Update the last modified timestamp & number of requests counter the object")
  (:method ((session session))
   (setf (get-last-modified-timestamp session) (get-universal-time))
   (incf (get-number-of-requests session))))

(defmethod print-object ((session session) stream)
  (print-unreadable-object (session stream :type t :identity t)
    (format stream "~s" (get-session-id session))))

(defconstant +default-session-lifetime+ (* 60 60))

(defclass session-manager ()
  ((sessions :reader get-sessions :initform (make-hash-table))
   (session-lifetime :accessor get-session-lifetime :initform +default-session-lifetime+)
   (sessions-last-reaped :initform (get-universal-time))
   (session-id-seed :accessor get-session-id-seed :initform (secure-random (get-universal-time)))
   (session-lock :accessor get-session-lock :initform (s-sysdeps:make-process-lock "session-manager")))
  (:documentation "Manage a collection of web application sessions"))

(defgeneric get-session-lifetime (session-manager)
  (:documentation "Return after how many seconds of idle time a session is destroyed"))

(defgeneric get-all-sessions (session-manager)
  (:documentation "Return a list of all known sessions")
  (:method ((session-manager session-manager))
   (let (all-sessions)
     (s-sysdeps:with-process-lock 
         ((get-session-lock session-manager))
       (maphash #'(lambda (key value)
                    (declare (ignore key))
                    (push value all-sessions))
                (get-sessions session-manager)))
     all-sessions)))

(defgeneric get-session-count (session-manager)
  (:documentation "Return the number of known sessions")
  (:method ((session-manager session-manager))
   (s-sysdeps:with-process-lock 
       ((get-session-lock session-manager))
     (hash-table-count (get-sessions session-manager)))))

(defgeneric find-session (session-manager id)
  (:documentation "Find the session with id in session-manager")
  (:method ((session-manager session-manager) id)
   (when id
     (s-sysdeps:with-process-lock 
         ((get-session-lock session-manager))
       (gethash id (get-sessions session-manager))))))

(defmethod get-new-session-id ((session-manager session-manager))
  (incf (get-session-id-seed session-manager))
  (+ (ash (get-session-id-seed session-manager) 32) (secure-random (expt 2 32))))

(defmethod create-new-session ((session-manager session-manager))
  (let* ((session-id (get-new-session-id session-manager))
         (session (make-instance 'session :id session-id :session-manager session-manager)))
    (s-sysdeps:with-process-lock 
        ((get-session-lock session-manager))
      (reap-old-sessions session-manager)
      (setf (gethash session-id (get-sessions session-manager)) session))
    session))

(defconstant +session-reaping-interval+ 60)

(defgeneric reap-old-sessions (session-manager)
  (:documentation "Remove all sessions from session-manager that are expired")
  (:method ((session-manager session-manager))
   (with-slots (session-lifetime sessions sessions-last-reaped)
       session-manager
     (let ((now (get-universal-time))
           sessions-to-reap)
       (unless (< now (+ sessions-last-reaped +session-reaping-interval+))
         (maphash #'(lambda (id session)
                      (when (> (- now (get-last-modified-timestamp session)) 
                               session-lifetime)
                        (push (cons id session) sessions-to-reap)))
                  sessions)
         (loop :for (id . session) :in sessions-to-reap :do
               (remhash id sessions)
               (let ((session-hook (get-attribute session :session-hook)))
                 (when session-hook
                   (funcall session-hook session :destroy))))
         (setf sessions-last-reaped now))))))

(defgeneric invalidate-all-sessions (session-manager)
  (:documentation "Remove all sessions from session-manager")
  (:method ((session-manager session-manager))
   (s-sysdeps:with-process-lock 
       ((get-session-lock session-manager))
     (clrhash (get-sessions session-manager)))))

;;;; eof