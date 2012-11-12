;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: session-tracking.lisp,v 1.11 2004/12/20 14:48:49 sven Exp $
;;;;
;;;; Session tracking connects request-responses to sessions
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(defun parse-session-id (string)
  "A full session-id has 3 parts, as in server-id.session-id.request-id"
  (let* ((tokens (s-utils:tokens string :separators '(#\.)))
         (server-id-token (first tokens))
         (session-id-token (second tokens))
         (request-id-token (third tokens)))
    (values server-id-token 
            (s-utils:parse-integer-safely session-id-token :radix 36) 
            (s-utils:parse-integer-safely request-id-token))))

(defmethod print-session-id ((request-response request-response) stream)
  "Print a full session-id to stream, as in server-id.session-id.request-id"
  (let* ((server-id (get-server-id (get-server request-response)))
         (session (get-session request-response))
         (session-id (get-session-id session))
         (request-id (get-attribute session :request-id)))
    (format stream "~a.~36r~@[.~d~]" server-id session-id request-id)))

(defmethod append-session-id ((request-response request-response) stream)
  (ecase (get-session-tracking-style (get-application request-response))
    ((:cookie :none))
    (:url (print-session-id request-response stream) (write-char #\/ stream))))

(defmethod get-session-id ((request-response request-response))
  "Return the session-id and request-id from request-response, while checking the server-id"
  (let* ((session-tracking-style (get-session-tracking-style (get-application request-response)))
         (full-session-id-string (ecase session-tracking-style
                                   (:none "")
                                   (:cookie (get-cookie request-response "session-id"))
                                   (:url (first (get-request-sub-path request-response))))))
    (multiple-value-bind (server-id session-id request-id)
        (parse-session-id full-session-id-string)
      (when (string-equal server-id (get-server-id (get-server request-response)))
        (when (and session-id (eq session-tracking-style :url))
          ;; we're pretty sure we parsed a valid session-id from the sub-path, pop it
          (pop (get-request-sub-path request-response)))
        (values session-id request-id)))))

(defmethod set-session-id-path ((request-response request-response) path)
  (ecase (get-session-tracking-style (get-application request-response))
    (:cookie (set-cookie request-response 
                         "session-id" 
                         (print-session-id request-response nil)
                         path))
    ((:url :none))))

(defmethod save-session-id ((request-response request-response))
  (ecase (get-session-tracking-style (get-application request-response))
    (:cookie (let ((web-app-path (dynamic-url request-response nil)))
               (setf web-app-path (subseq web-app-path 0 (1- (length web-app-path))))
               (set-session-id-path request-response web-app-path)))
    ((:url :none))))

(defmethod check-and-update-request-id ((request-response request-response) request-id)
  (let* ((session (get-session request-response))
         (saved-request-id (get-attribute session :request-id)))
    (if (null saved-request-id)
        (setf (get-attribute session :request-id) 0)
      (if (eql saved-request-id request-id)
          (incf (get-attribute session :request-id))
        nil))))

(defparameter +request-signature-headers+ 
  '(:user-agent #+NIL :accept :accept-encoding :accept-language :accept-charset))

(defmethod store-request-signature ((request-response request-response))
  "Store a request signature into the session for future validation"
  (let ((web-app (get-application request-response))
        (session (get-session request-response)))
    (when (get-session-validation web-app)
      (setf (get-attribute session :client-ip-address) (get-request-ip-address request-response))
      (dolist (header +request-signature-headers+)
        (setf (get-attribute session header) (get-request-header-value request-response header))))))

(defmethod validate-session-signature ((request-response request-response) session)
  "Check if the request signature stored in the session matches that of the new request"
  (let ((web-app (get-application request-response)))
    (if (get-session-validation web-app)
        (let ((stored-ip-address (get-attribute session :client-ip-address))
              (request-ip-address (get-request-ip-address request-response)))
          (and (if (equal stored-ip-address request-ip-address)
                   t
                 (progn 
                   (log-warn request-response "Session validation failed on ip-address")
                   nil))
               (every #'(lambda (header)
                          (if (equal (get-attribute session header)
                                     (get-request-header-value request-response header))
                              t
                            (progn
                              (log-warn request-response "Session validation failed on header ~a" header)
                              nil)))
                      +request-signature-headers+)))
      t)))

(defmethod bind-session ((request-response request-response))
  "Binds a session to request-response using the session tracking style, return status"
  (if (eq (get-session-tracking-style (get-application request-response)) :none)
      (values nil t)
    (multiple-value-bind (session-id request-id)
        (get-session-id request-response)
      (let* ((session-manager (get-server request-response))
             (session (find-session session-manager session-id))
             (web-app (get-application request-response))
             (request-tracking (get-request-tracking (get-application request-response))))
        (if (and session 
                 (eql (get-attribute session :web-app) web-app))
            (if (validate-session-signature request-response session)
                (progn
                  (touch session)
                  (setf (get-session request-response) session))
              (setf session nil))
          (let ((new-session (create-new-session session-manager)))
            (setf (get-session request-response) new-session)
            (log-debug request-response "created new session ~36R [~:*~d]" (get-session-id new-session))
            (let ((session-hook (get-session-hook web-app)))
              (setf (get-attribute new-session :web-app) web-app)
              (when session-hook 
                (setf (get-attribute new-session :session-hook) session-hook))
              (store-request-signature request-response))))
        (if request-tracking
            (let ((request-consistent (check-and-update-request-id request-response request-id)))
              ;; since we're doing request tracking we always have to save the session id in the cookie
              (save-session-id request-response)
              (values (null session) request-consistent))
          (progn
            ;; only save the session id in case of new sessions
            (unless session (save-session-id request-response))
            (values (null session) t)))))))

;;;; eof