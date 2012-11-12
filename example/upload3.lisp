;;;; -*- mode: lisp -*-
;;;;
;;;; Example showing how to do a JPEG file upload using the newer, more efficient stream techniques
;;;;
;;;; Copyright (C) 2006 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :upload3
  (:index 'upload3-index)
  (:static-root "static/")
  (:unsecure t))

(defun upload3-index (request-response)
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
         :action (dynamic-url request-response 'upload3-receive) 
         :enctype "multipart/form-data"
         (:table :border 0
          (:tr (:td "File:" (:input :type "file" :name "file")))
          (:tr (:td "Info:" (:input :type "text" :name "info")))
          (:tr (:td "Extra:" (:input :type "text" :name "extra")))
          (:tr (:td (:input :type "reset" :name "Reset") (:input :type "submit" :name "Upload")))))))))))

(defun extract-uploaded-file-3 (stream)
  "Extract the uploaded file from body-string, save it and return the pathname to it"
  (let* ((parts (extract-multipart-parts stream :use-tmp-files-for-data t))
         (part (find-multipart-named "file" parts)))
    (when part
      (destructuring-bind (headers data) part
        (let ((content-type (second (find-multipart-header-named "Content-Type" headers))))
          (when (string-equal "image/jpeg" content-type)
            ;; data is the pathname of a system generated tmp file with the actual data
            (let ((new-pathname (merge-pathnames (make-pathname :type "jpg") data)))
              (rename-file data new-pathname)
              (values new-pathname
                      (second (find-multipart-named "info" parts))
                      (second (find-multipart-named "extra" parts))))))))))

(defun upload3-receive (request-response)
  (let ((mime "multipart/form-data")
        (content-type (get-request-header-value request-response "Content-Type")))
    (if (and (eql (get-request-method request-response) :post)
             (string-equal content-type  mime :end1 (min (length mime) (length content-type))))
        (multiple-value-bind (pathname info extra)
            (extract-uploaded-file-3 (get-request-stream request-response))
          (if pathname
              (progn
                (log-debug request-response "accepted uploaded file: ~a" pathname)
                (upload3-show request-response (pathname-name pathname) info extra))
            (html-message request-response "Error" "Upload failed!")))
      (html-message request-response "Error" "Only POST of ~a allowed!" mime))))

(defun upload3-serve-file (request-response)
  (let ((pathname (merge-pathnames (get-request-parameter-value request-response "file") #p"/tmp/foo.jpg")))
    (if (and pathname (probe-file pathname))
        (let ((size (with-open-file (in (merge-pathnames pathname)) (file-length in))))
          (setf (get-response-mime-type request-response) "image/jpeg")
          (commit-headers request-response size)
          (let ((out (get-response-stream request-response)))
            (with-open-file (in pathname :element-type '(unsigned-byte 8))
              (s-utils:copy-stream in out (make-array 4096 :element-type '(unsigned-byte 8))))))
      (setf (get-response-state request-response) :not-found))))

(defun upload3-show (request-response pathname info extra)
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
          :src (dynamic-url request-response 'upload3-serve-file :file pathname) 
          :alt pathname))
        (let ((size (with-open-file (in (merge-pathnames pathname #p"/tmp/foo.jpg")) (file-length in))))
          (htm (:p (fmt "The file you uploaded is ~:d bytes long" size))))
        (:p (fmt "Pathname: ~s; Info: ~s; Extra: ~s" pathname info extra))
        (:div :class "NX_button_group" :style "margin-top:20px;"
         (:a :class "NX_button" :href (dynamic-url request-response nil) "Back"))))))))

;;;; eof
