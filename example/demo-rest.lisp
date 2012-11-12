;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: demo-rest.lisp,v 1.1 2004/06/21 13:35:52 sven Exp $
;;;;
;;;; An extension to forms2.lisp, a REST (web) service interface, both client and server
;;;; Requires s-xml to be loaded
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defun demo-user-to-xml (demo-user)
  (with-slots (fullname username password age)
      demo-user
    (with-html-output-to-string (out)
      (:demo-user
       (when (slot-boundp demo-user 'id) 
         (htm (:id (str (slot-value demo-user 'id)))))
       (:fullname (esc fullname))
       (:username (esc username))
       (:password (esc password))
       (:age (str age))))))

(defun lxml-to-demo-user (lxml)
  (when (eq (first lxml) :|demo-user|)
    (let ((slots (rest lxml))
          (demo-user (make-instance 'demo-user)))
      (setf (slot-value demo-user 'id) (util:parse-integer-safely (second (assoc :|id| slots)))
            (slot-value demo-user 'fullname) (second (assoc :|fullname| slots))
            (slot-value demo-user 'username) (second (assoc :|username| slots))
            (slot-value demo-user 'password) (second (assoc :|password| slots))
            (slot-value demo-user 'age) (util:parse-integer-safely (second (assoc :|age| slots))))
      demo-user)))

(defun demo-rest (request-response)
  (let ((method (get-request-method request-response))
        (id (util:parse-integer-safely (get-request-parameter-value request-response "id"))))
    (case method
      (:get (if id
                (html-part (out request-response)
                  (str (demo-user-to-xml (demo-user-with-id id))))
              (html-part (out request-response)
                (:demo-users
                 (dolist (demo-user (list-demo-users))
                   (htm
                    (str (demo-user-to-xml demo-user))))))))
      (:post (let ((demo-user-data (lxml-to-demo-user (s-xml:parse-xml-string (get-request-body request-response))))
                   (demo-user (create-new-demo-user)))
               (dolist (slot '(fullname username password age))
                 (setf (slot-value demo-user slot) (slot-value demo-user-data slot)))
               (html-part (out request-response)
                 (str (demo-user-to-xml demo-user)))))
      (:put (let* ((demo-user-data (lxml-to-demo-user (s-xml:parse-xml-string (get-request-body request-response))))
                   (demo-user (demo-user-with-id (slot-value demo-user-data 'id))))
              (when demo-user
                (dolist (slot '(fullname username password age))
                  (setf (slot-value demo-user slot) (slot-value demo-user-data slot)))
                (html-part (out request-response)
                  (str (demo-user-to-xml demo-user))))))
      (:delete (let ((demo-user (demo-user-with-id id)))
                 (when demo-user
                   (remove-demo-user demo-user))))
      (t nil))))

;; client interface

(defparameter *demo-rest-url* "http://localhost/kpax/dynamic/forms2/demo-rest")

(defun rest-list-all-demo-users ()
  (let* ((result (net.aserve.client:do-http-request *demo-rest-url*
                                                    :method :get))
         (lxml (s-xml:parse-xml-string result)))
    (when (eq (first lxml) :|demo-users|)
      (mapcar #'lxml-to-demo-user (rest lxml)))))

(defun rest-get-demo-user (id)
  (let ((result (net.aserve.client:do-http-request *demo-rest-url* 
                                                   :method :get
                                                   :query `((id . ,id)))))
    (lxml-to-demo-user (s-xml:parse-xml-string result))))

(defun rest-new-demo-user (fullname username password age)
  (let ((demo-user (make-instance 'demo-user)))
    (setf (slot-value demo-user 'fullname) fullname
          (slot-value demo-user 'username) username
          (slot-value demo-user 'password) password
          (slot-value demo-user 'age) age)
    (let ((result (net.aserve.client:do-http-request *demo-rest-url* 
                                                     :method :post
                                                     :content (princ-to-string (demo-user-to-xml demo-user)))))
      (lxml-to-demo-user (s-xml:parse-xml-string result)))))

(defun rest-change-demo-user (demo-user)
  (let ((result (net.aserve.client:do-http-request *demo-rest-url* 
                                                   :method :put 
                                                   :content (princ-to-string (demo-user-to-xml demo-user)))))
    (lxml-to-demo-user (s-xml:parse-xml-string result))))

(defun rest-delete-demo-user (demo-user)
  (net.aserve.client:do-http-request *demo-rest-url* 
                                     :method :delete 
                                     :query `((id . ,(slot-value demo-user 'id))))
  t)

;;;; eof
