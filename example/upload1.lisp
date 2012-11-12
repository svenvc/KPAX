;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: upload1.lisp,v 1.6 2004/12/16 15:08:36 sven Exp $
;;;;
;;;; Example showing how to do a JPEG file upload
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :upload1
  (:index 'upload1-index)
  (:static-root "static/")
  (:unsecure t))

(defun upload1-index (request-response)
  (html-page (out request-response)
    (:html
     (:head 
      (:title "KPAX JPEG Upload")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body
      (:div :class "NX_panel"
       (:span :class "NX_title" "KPAX Upload")
       (:div :class "NX_border"
        (:p "Please select a JPEG file to upload:")
        (:form
         :class "NX_form"
         :method "post" 
         :action (dynamic-url request-response 'upload1-receive) 
         :enctype "multipart/form-data"
         (:table :border 0
          (:tr (:td "File:" (:input :type "file" :name "file")))
          (:tr (:td "Info:" (:input :type "text" :name "info")))
          (:tr (:td "Extra:" (:input :type "text" :name "extra")))
          (:tr (:td (:input :type "reset" :name "Reset") (:input :type "submit" :name "Upload")))))))))))

(defun extract-uploaded-file (body-string)
  "Extract the uploaded file from body-string, save it and return the pathname to it"
  (let* ((parts (extract-multipart-parts body-string))
         (part (find-multipart-named "file" parts)))
    (when part
      (destructuring-bind (headers data) part
        (let ((content-type (second (find-multipart-header-named "Content-Type" headers))))
          (when (string-equal "image/jpeg" content-type)
            (let ((pathname (merge-pathnames (prin1-to-string (get-universal-time)) #p"/tmp/foo.jpg")))
              (with-open-file (out pathname 
                                   :direction :output
                                   :element-type '(unsigned-byte 8)
                                   :if-does-not-exist :create)
                (with-input-from-string (in data)
                  (s-utils:copy-stream in out 
                                       (make-string 4096)
                                       #+clisp (make-array 4096 :element-type '(unsigned-byte 8))
                                       #+clisp #'char-code)))
              (values pathname
                      (second (find-multipart-named "info" parts))
                      (second (find-multipart-named "extra" parts))))))))))

(defun upload1-receive (request-response)
  (multiple-value-bind (pathname info extra)
      (extract-uploaded-file (get-request-body request-response))
    (if pathname
        (progn
          (log-debug request-response "accepted uploaded file: ~a" pathname)
          (upload1-show request-response (pathname-name pathname) info extra))
      (html-message request-response "Error" "Upload failed!"))))

(defun upload1-serve-file (request-response)
  (let ((pathname (merge-pathnames (get-request-parameter-value request-response "file") #p"/tmp/foo.jpg"))
        (out (get-content-stream request-response)))
    (when (and pathname (probe-file pathname))
      (setf (get-response-mime-type request-response) "image/jpeg")
      ;; content-length will be set automatically by KPAX at commit time
      (with-open-file (in pathname :element-type '(unsigned-byte 8))
        (s-utils:copy-stream in out 
                             (make-array 4096 :element-type '(unsigned-byte 8))
                             #+clisp (make-string 4096)
                             #+clisp #'code-char)))))

(defun upload1-show (request-response pathname info extra)
  (html-page (out request-response)
    (:html
     (:head 
      (:title "KPAX Uploaded JPEG")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body
      (:div :class "NX_panel"
       (:span :class "NX_title" "KPAX Uploaded JPEG")
       (:div :class "NX_border"
        (:p "This is the JPEG file you uploaded:")
        (:div :style "width:100%;overflow:auto;"
         (:img 
          :src (dynamic-url request-response 'upload1-serve-file :file pathname) 
          :alt pathname))
        (:p (fmt "Pathname: ~s; Info: ~s; Extra: ~s" pathname info extra))
        (:div :class "NX_button_group" :style "margin-top:20px;"
         (:a :class "NX_button" :href (dynamic-url request-response nil) "Back"))))))))

;;;; eof
