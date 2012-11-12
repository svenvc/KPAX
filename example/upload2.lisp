;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: upload1.lisp,v 1.6 2004/12/16 15:08:36 sven Exp $
;;;;
;;;; Example showing how to do an abitrary large file upload
;;;;
;;;; Copyright (C) 2004, 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :upload2
  (:index 'upload2-index)
  (:static-root "static/")
  (:unsecure t))

(defun upload2-index (request-response)
  (html-page (out request-response)
    (:html
     (:head 
      (:title "KPAX File Upload")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body
      (:div :class "NX_panel"
       (:span :class "NX_title" "KPAX Upload")
       (:div :class "NX_border"
        (:p "Please select a file to upload:")
        (:form
         :class "NX_form"
         :method "post" 
         :action (dynamic-url request-response 'upload2-receive) 
         :enctype "multipart/form-data"
         (:table :border 0
          (:tr (:td "File:" (:input :type "file" :name "file")))
          (:tr (:td "Info:" (:input :type "text" :name "info")))
          (:tr (:td "Extra:" (:input :type "text" :name "extra")))
          (:tr (:td (:input :type "reset" :name "Reset") (:input :type "submit" :name "Upload")))))))))))

(defun extract-uploaded-file-2 (body-string)
  "Extract the uploaded file from body-string, save it and return the pathname to it"
  (let* ((parts (extract-multipart-parts body-string))
         (part (find-multipart-named "file" parts)))
    (when part
      (destructuring-bind (headers data) part
        (let* ((content-type (second (find-multipart-header-named "Content-Type" headers)))
               (content-disposition (find-multipart-header-named "Content-Disposition" headers))
               (filename (find-multipart-header-attribute "filename" content-disposition)))
          (let ((pathname (merge-pathnames (prin1-to-string (get-universal-time)) #p"/tmp/foo.bin")))
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
                    (second (find-multipart-named "extra" parts))
                    content-type
                    filename)))))))

(defun upload2-receive (request-response)
  (multiple-value-bind (pathname info extra content-type filename)
      (extract-uploaded-file-2 (get-request-body request-response))
    (if pathname
        (progn
          (log-debug request-response "accepted uploaded file: ~a" pathname)
          (upload2-show request-response (pathname-name pathname) info extra content-type filename))
      (html-message request-response "Error" "Upload failed!"))))

(defun upload2-show (request-response pathname info extra content-type filename)
  (html-page (out request-response)
    (:html
     (:head 
      (:title "KPAX Uploaded JPEG")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body
      (:div :class "NX_panel"
       (:span :class "NX_title" "KPAX Uploaded File")
       (:div :class "NX_border"
        (let ((size (with-open-file (in (merge-pathnames pathname #p"/tmp/foo.bin")) (file-length in))))
          (htm (:p 
                (fmt "The file you uploaded is ~:d bytes long" size)
                "; "
                (:a :href (dynamic-url request-response 'upload2-serve :file pathname :content-type content-type)
                 "Download"))))
        (:p (fmt "Filename: ~s; Content-Type: ~s" filename content-type)) 
        (:p (fmt "Pathname: ~s; Info: ~s; Extra: ~s" pathname info extra))
        (:div :class "NX_button_group" :style "margin-top:20px;"
         (:a :class "NX_button" :href (dynamic-url request-response nil) "Back"))))))))

(defun upload2-serve (request-response)
  (let ((pathname (merge-pathnames (get-request-parameter-value request-response "file") #p"/tmp/foo.bin"))
        (content-type (get-request-parameter-value request-response "content-type")))
    (if (and pathname (probe-file pathname) content-type)
        (let ((size (with-open-file (in pathname) (file-length in))))
          (setf (get-response-mime-type request-response) content-type)
          (commit-headers request-response size)
          (let ((out (get-response-stream request-response)))
            (with-open-file (in pathname :element-type '(unsigned-byte 8))
              (s-utils:copy-stream in out (make-array 4096 :element-type '(unsigned-byte 8))))))
      (setf (get-response-state request-response) :not-found))))

;;;; eof
