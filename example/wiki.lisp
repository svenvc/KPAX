;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: wiki.lisp,v 1.13 2005/01/21 15:39:17 sven Exp $
;;;;
;;;; A WikiWiki implementation as a KPAX Web Application
;;;;
;;;; Copyright (C) 2004, 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

;; the public web app showing readonly wiki pages

(defwebapp :wiki
  (:index 'show)
  (:web-functions nil)
  (:unsecure t))

;; the private web app showing wiki pages and allowing editing

(defwebapp :kiwi
  (:index 'show)
  (:static-root "static/")
  (:web-functions '(edit update logout))
  (:session-validation nil)
  (:authorizer '(("admin" . "trustno1"))))

;; constants

(defconstant +default-wiki-dir-name+ "wiki")

(defconstant +contents-filename+ "contents")

(defconstant +meta-filename+ "meta")

(defconstant +data-extension+ "dat")

(defconstant +default-wiki-resource-name+ "index")

(defconstant +default-mime-type+ "text/html")

;; configuration support

(defun get-wiki-dir (web-app)
  "Get the wiki-dir to use from the web-app (either specified or defaulted)"
  (let* ((web-app-parent (s-utils:pathname-parent (get-option web-app :load-truename)))
         (wiki-dir-web-app (s-utils:make-subdirectory web-app-parent +default-wiki-dir-name+))
         (wiki-dir-option (get-option web-app :wiki-dir)))
    (if (and wiki-dir-option (probe-file wiki-dir-option))
        (truename wiki-dir-option)
      wiki-dir-web-app)))

;; the wiki-resource model with api

(defclass wiki-resource ()
  ((name :accessor get-name :initarg :name)
   (pathname :accessor get-pathname :initarg :pathname)
   (meta)
   (data))
  (:documentation "Object representing a resource managed by the wiki"))

(defmethod print-object ((wiki-resource wiki-resource) stream)
  (print-unreadable-object (wiki-resource stream :type t :identity t)
    (format stream "'~a'" (get-name wiki-resource))))

(defmethod get-title ((wiki-resource wiki-resource))
  (get-meta-info wiki-resource :title (get-name wiki-resource)))

(defmethod get-contents-pathname ((wiki-resource wiki-resource))
  (merge-pathnames (make-pathname :name +contents-filename+ :type +data-extension+) 
                   (get-pathname wiki-resource)))

(defmethod get-meta-pathname ((wiki-resource wiki-resource))
  (merge-pathnames (make-pathname :name +meta-filename+ :type +data-extension+) 
                   (get-pathname wiki-resource)))

(defmethod exists-p ((wiki-resource wiki-resource))
  "Test whether the files of wiki-resource actually exist"
  (and (probe-file (get-pathname wiki-resource))
       (probe-file (get-contents-pathname wiki-resource))))

(defmethod get-content-length ((wiki-resource wiki-resource))
  (if (exists-p wiki-resource)
      (with-open-file (in (get-contents-pathname wiki-resource))
        (file-length in))
    0))

(defmethod get-last-modification-date ((wiki-resource wiki-resource))
  (if (exists-p wiki-resource)
      (file-write-date (get-pathname wiki-resource))
    -1))

(defmethod get-contents ((wiki-resource wiki-resource))
  "Get the (ascii) contents of wiki-resource"
  (let ((pathname (get-contents-pathname wiki-resource)))
    (when (probe-file pathname)
      (with-output-to-string (out)
        (with-open-file (in pathname :direction :input)
          (s-utils:copy-stream in out))))))

(defmethod (setf get-contents) (value (wiki-resource wiki-resource))
  "Set the (ascii) contents of wiki-resource, creating files if necessary"
  (when value
    (let ((pathname (get-contents-pathname wiki-resource)))
      (ensure-directories-exist pathname)
      (with-input-from-string (in value)
        (with-open-file (out pathname :direction :output :if-does-not-exist :create :if-exists :supersede)
          (s-utils:copy-stream in out))))))

(defmethod get-meta ((wiki-resource wiki-resource))
  "Get the meta plist for wiki-resource, loading and caching it if necessary"
  ;; session lock should be used!?
  (if (slot-boundp wiki-resource 'meta)
      (slot-value wiki-resource 'meta)
    (let ((pathname (get-meta-pathname wiki-resource)))
      (setf (slot-value wiki-resource 'meta)
            (when (probe-file pathname)
              (with-open-file (in pathname :direction :input)
                (read in nil nil nil)))))))

(defmethod (setf get-meta) (plist (wiki-resource wiki-resource))
  "Set the meta plist of wiki-resource, both caching and saving it"
  ;; session lock should be used!?
  (setf (slot-value wiki-resource 'meta) plist)
  (let ((pathname (get-meta-pathname wiki-resource)))
    (ensure-directories-exist pathname)
    (with-open-file (out pathname :direction :output :if-does-not-exist :create :if-exists :supersede)
      (print plist out))))

(defmethod get-meta-info ((wiki-resource wiki-resource) indicator &optional default)
  (getf (get-meta wiki-resource) indicator default))

(defmethod (setf get-meta-info) (value (wiki-resource wiki-resource) indicator)
  (setf (getf (get-meta wiki-resource) indicator) value))

(defmethod get-data ((wiki-resource wiki-resource))
  ;; session lock should be used!?
  (if (slot-boundp wiki-resource 'data)
      (slot-value wiki-resource 'data)
    (unless (get-binaryp (get-type wiki-resource))
      (let ((pathname (get-contents-pathname wiki-resource)))
        (when (probe-file pathname)
          (with-open-file (in pathname :direction :input)
            (setf (slot-value wiki-resource 'data)
                  (funcall (get-parser (get-type wiki-resource)) in))))))))

;; managing wiki-resource objects
;; resource names are simple strings, or paths like images/small/logo

(defun make-wiki-resource (resource-name dir)
  "Make a wiki-resource object by resource name in dir, not actually creating any files"
  (unless (stringp resource-name) 
    (setf resource-name (princ-to-string resource-name)))
  (let ((pathname (s-utils:make-subdirectory dir 
                                          (s-utils:tokens resource-name :separators (list #\/)))))
    (make-instance 'wiki-resource 
                   :name resource-name 
                   :pathname pathname)))

(defun get-wiki-resource (request-response resource-name)
  ;; add (webapp scope) caching here later on (include session-lock)
  (make-wiki-resource resource-name (get-wiki-dir (get-application request-response))))

(defun get-wiki-resource-from-request (request-response)
  "Retrieve a wiki-resource object from the request, either as sub-path, parameter or defaulted"
  (let ((resource-name (or (get-request-parameter-value request-response :wiki-resource)
                           (let ((sub-path (rest (get-request-sub-path request-response))))
                             (if sub-path 
                                 (format nil "~{~A~^/~}" sub-path)
                               +default-wiki-resource-name+)))))
    (get-wiki-resource request-response resource-name)))

;; resource handling by type

;; the mime-type entry in the meta-info is important for most general types

;; the RAW type is for binary content which is left untouched by the wiki

(defun render-raw (request-response wiki-resource)
  ;; raw content is read from disk and passed unchanged to the client
  (let ((out (get-content-stream request-response))
        (mime-type (get-meta-info wiki-resource :mime-type +default-mime-type+))
        (pathname (get-contents-pathname wiki-resource)))
    (if (probe-file pathname)
        (progn
          (setf (get-response-mime-type request-response) mime-type)
          (with-open-file (in pathname :direction :input)
            (s-utils:copy-stream in out)))
      (html-message request-response "Sorry" 
                    "Sorry, the wiki resource named '~a' does not exist" 
                    (get-name wiki-resource)))))

(defun parse-raw (in)
  ;; raw content is accepted as is (this function is not used, raw contents in not cached for now)
  (with-output-to-string (out)
    (s-utils:copy-stream in out)))

;; the TEXT type is for general ascii content which is processed for plugins by the wiki

(defun render-text (request-response wiki-resource)
  ;; text content is rendered from a collection of text and plugin elements
  (if (exists-p wiki-resource)
      (render-elements request-response (get-data wiki-resource))
    (format (get-content-stream request-response) "ResourceDoesNotExist(~a)" (get-name wiki-resource))))

(defun parse-text (in)
  ;; text content is parsed into a collection of text and plugin calls
  (parse-elements in))

;; the WIKI type is a simplified text type with additional syntax

(defun render-wiki (request-response wiki-resource)
  (render-text request-response wiki-resource))

(defun parse-wiki (in)
  (parse-text in))

;; the :wiki-resource-stack tracing mechanism

(defun get-current-wiki-resource (request-response)
  (first (get-attribute request-response :wiki-resource-stack)))

(defun get-parent-wiki-resource (request-response)
  (second (get-attribute request-response :wiki-resource-stack)))

(defun get-root-wiki-resource (request-response)
  (first (last (get-attribute request-response :wiki-resource-stack))))

;; the substitution mechanism

(defun parse-elements (in &optional (marker #\=))
  "Parse the in stream and read all embedded marker() s-expressions, return as a list of strings and sexps"
  (let ((buffer (make-string-output-stream))
        (*package* (find-package :kpax-user))
        elements)
    (loop
     (let ((ch (read-char in nil nil)))
       (cond ((null ch) (push (get-output-stream-string buffer) elements) (return))
             ((char= ch marker) (if (equal #\( (peek-char nil in nil nil))
                                    (multiple-value-bind (sexp error-condition) 
                                        (ignore-errors (read-preserving-whitespace in nil nil))
                                      (cond (sexp (push (get-output-stream-string buffer) elements)
                                                  (push sexp elements))
                                            (error-condition (format buffer 
                                                                     "(PluginParseError '~a')" 
                                                                     error-condition))
                                            (t (write-char marker buffer) 
                                               (write-char #\( buffer))))
                                  (write-char ch buffer)))
             (t (write-char ch buffer)))))
    (nreverse (delete-if #'(lambda (x) (and (stringp x) (zerop (length x)))) 
                         elements))))

(defun render-elements (request-response elements)
  "Render the list of elements, writing string to the content stream and executing plugins recursively"
  (loop :for element :in elements
        :do (if (stringp element)
                (write-string element (get-content-stream request-response))
              (let ((plugin-name (first element)))
                (if (fboundp plugin-name)
                    (funcall plugin-name (rest element) request-response)
                  (format (get-content-stream request-response) "(PluginDoesNotExistError '~a')" plugin-name))))))

;; internal web request handling functions

(defun show-nested-wiki-resource (request-response wiki-resource)
  (push wiki-resource (get-attribute request-response :wiki-resource-stack))
  (funcall (get-renderer (get-type wiki-resource)) request-response wiki-resource)
  (pop (get-attribute request-response :wiki-resource-stack)))

(defun show-toplevel-wiki-resource (request-response wiki-resource)
  (let* ((container-resource-name (get-container (get-type wiki-resource)))
         (container-wiki-resource (when container-resource-name 
                                    (get-wiki-resource request-response container-resource-name))))
    (if (and container-wiki-resource
             (exists-p container-wiki-resource))
        (progn
          (push wiki-resource (get-attribute request-response :wiki-resource-stack))
          (show-nested-wiki-resource request-response container-wiki-resource))
      (show-nested-wiki-resource request-response wiki-resource))))

;; plugin support functions

(defun resolve-wiki-resource-reference (request-response reference)
  (if reference
      (case reference
        (current (get-current-wiki-resource request-response))
        (parent (get-parent-wiki-resource request-response))
        (root (get-root-wiki-resource request-response))
        (t (get-wiki-resource request-response reference)))
    (get-current-wiki-resource request-response)))

(defun wiki-resource-url (request-response wiki-resource &optional (operation "show"))
  (dynamic-url request-response "~a/~a" operation (get-name wiki-resource)))
  
;; the plugins

(defun date-and-time-now (args request-response)
  (declare (ignore args))
  (s-utils:format-universal-time (get-universal-time) :stream (get-content-stream request-response)))

(defvar *wiki-boot-time* (get-universal-time))

(defun server-uptime (args request-response)
  (declare (ignore args))
  (s-utils:format-duration (- (get-universal-time) *wiki-boot-time*)
                           :stream (get-content-stream request-response)))

(defun snoop-request (args request-response)
  (destructuring-bind (&key (uri t) (headers t) (parameters t)) args
    (html-part (out request-response)
      (when uri
        (htm
         (:p (fmt "Request URI is ~a" (puri:render-uri (get-uri request-response) nil)))))
      (when headers
        (htm
         (:table :width "100%"
          (:tr (:th :width "20%" "Header Name") (:th "Value"))
          (dolist (header-name (get-request-headers request-response))
            (htm
             (:tr 
              (:td (str header-name))
              (:td (str (get-request-header-value request-response header-name)))))))))
      (when parameters
        (htm
         (:table :width "100%"
          (:tr (:th :width "20%" "Parameter Name") (:th "Value"))
          (dolist (parameter-name (get-request-parameters request-response))
            (htm
             (:tr 
              (:td (str parameter-name))
              (:td (str (get-request-parameter-value request-response parameter-name))))))))))))

(defun br (args request-response)
  (destructuring-bind (&optional (count 1)) args
    (html-part (out request-response)
      (loop :repeat count :do (htm :br)))))

(defun img (args request-response)
  (let ((wiki-resource (get-wiki-resource request-response (first args))))
    (html-part (out request-response :pprint-html nil)
      (:img 
       :src (wiki-resource-url request-response wiki-resource)
       :width (get-meta-info wiki-resource :width)
       :height (get-meta-info wiki-resource :height)))))

(defun wiki-edit-link (args request-response)
  (declare (ignore args))
  (when (eq (get-name (get-application request-response)) :kiwi)
    (html-part (out request-response)
      (:a :href (wiki-resource-url request-response (get-root-wiki-resource request-response) "edit") 
       "Edit"))))

(defun wiki-refresh-link (args request-response)
  (declare (ignore args))
  (when (eq (get-name (get-application request-response)) :kiwi)
    (html-part (out request-response)
      (:a :href (wiki-resource-url request-response (get-root-wiki-resource request-response))
       "Refresh"))))

(defun wiki-logout-link (args request-response)
  (declare (ignore args))
  (when (eq (get-name (get-application request-response)) :kiwi)
    (html-part (out request-response)
      (:a :href (dynamic-url request-response 'logout) 
       "Logout"))))

(defun wiki-home-link (args request-response)
  (html-part (out request-response)
    (:a :href (dynamic-url request-response nil) 
     (str (or (first args) "Home")))))

(defun wiki-url (args request-response)
  (let ((wiki-resource (resolve-wiki-resource-reference request-response (first args))))
    (write-string (wiki-resource-url request-response wiki-resource)
                  (get-content-stream request-response))))

(defun wiki-resource-name (args request-response)
  (let ((wiki-resource (resolve-wiki-resource-reference request-response (first args))))
    (write-string (get-name wiki-resource) (get-content-stream request-response))))

(defun wiki-resource-title (args request-response)
  (let ((wiki-resource (resolve-wiki-resource-reference request-response (first args))))
    (write-string (get-title wiki-resource) (get-content-stream request-response))))

(defun wiki-link (args request-response)
  (let ((wiki-resource (resolve-wiki-resource-reference request-response (first args))))
    (html-part (out request-response :pprint-html nil)
      (:a 
       :href (wiki-resource-url request-response wiki-resource)
       (str (or (second args) (get-title wiki-resource)))))))

(defun include-wiki-resource (args request-response)
  (let ((wiki-resource (resolve-wiki-resource-reference request-response (first args)))
        (out (get-content-stream request-response)))
    (cond ((get-binaryp (get-type wiki-resource))
           (format out "CannotIncludeBinaryResource(~a)" (get-name wiki-resource)))
          ((eq (get-current-wiki-resource request-response) wiki-resource)
           (format out "RecursiveIncludePrevented(~a)" (get-name wiki-resource)))
          (t
           (show-nested-wiki-resource request-response wiki-resource)))))

;; wiki-resource types

(defclass wiki-resource-type ()
  ((name :accessor get-name :initarg :name)
   (binaryp :accessor get-binaryp :initarg :binaryp :initform t)
   (renderer :accessor get-renderer :initarg :renderer :initform 'render-raw)
   (container :accessor get-container :initarg :container :initform nil)
   (parser :accessor get-parser :initarg :parser :initform 'parse-raw)))

(defparameter *wiki-resource-types*
  (let* ((specs '((:raw)
                  (:text :renderer render-text :parser parse-text :binaryp nil)
                  (:wiki :renderer render-wiki :parser parse-wiki :binaryp nil :container wiki-container)))
         (types (mapcar #'(lambda (spec)
                            (destructuring-bind (name &key renderer parser (binaryp t) container) spec
                              (let ((type (make-instance 'wiki-resource-type :name name :binaryp binaryp)))
                                (when renderer (setf (get-renderer type) renderer))
                                (when parser (setf (get-parser type) parser))
                                (when container (setf (get-container type) container))
                                type)))
                        specs))
         (hashtable (make-hash-table)))
    (dolist (type types) 
      (setf (gethash (get-name type) hashtable) type))
    hashtable))

(defmethod get-type ((wiki-resource wiki-resource))
  (gethash (get-meta-info wiki-resource :type :raw) *wiki-resource-types*))

;; web-actions

(defun update (request-response)
  "Action to process the updating (or creation) of a resource"
  (let* ((content-type (get-request-header-value request-response "Content-Type"))
         (multipartp (and content-type (eql 0 (search "multipart/form-data" content-type))))
         (parts (when multipartp (extract-multipart-parts (get-request-body request-response))))
         (wiki-resource (if multipartp 
                            (let ((resource-name (second (find-multipart-named :wiki-resource parts))))
                              (get-wiki-resource request-response resource-name))
                          (get-wiki-resource-from-request request-response)))
         (contents (if multipartp
                       (second (find-multipart-named :file parts))
                     (get-request-parameter-value request-response :contents)))
         (meta (if multipartp
                   (second (find-multipart-named :meta parts))
                 (get-request-parameter-value request-response :meta))))
    (if multipartp
        (when (and contents (plusp (length contents)))
          (setf (get-contents wiki-resource) contents))
      (setf (get-contents wiki-resource) contents))
    (when meta 
      (let ((meta-plist (read-from-string meta nil nil)))
        (when meta-plist
          (setf (get-meta wiki-resource) meta-plist)))) 
    (show-toplevel-wiki-resource request-response wiki-resource)))

(defun edit (request-response)
  "Action to edit a resource"
  (let* ((wiki-resource (get-wiki-resource-from-request request-response))
         (contents (if (exists-p wiki-resource) 
                       (get-contents wiki-resource) 
                     (format nil "Please edit contents of ~a" (get-name wiki-resource))))
         (meta (prin1-to-string (or (get-meta wiki-resource) '(:type :raw))))
         (binaryp (get-binaryp (get-type wiki-resource))))
    (html-page (out request-response :pprint-html nil)
      (:html
       (:head (:title (fmt "Editing: '~a'" (get-title wiki-resource))))
       (:body
        (:p (fmt "Editing ~a" (escape-string (prin1-to-string wiki-resource))))
        (:form 
         :method "post" :action (dynamic-url request-response 'update) 
         :enctype (when binaryp "multipart/form-data")
         (:input :type "hidden" :name :wiki-resource :value (get-name wiki-resource))
         (:p "Meta")
         (:textarea 
          :name :meta :cols "80" :rows "5"
          :style "width:80%;background-color:lightgray;border:1px solid black;"
          (esc meta))
         (:p (fmt "Contents [length: ~:d bytes; last-modified: ~a]" 
                  (get-content-length wiki-resource)
                  (s-utils:format-universal-time (get-last-modification-date wiki-resource))))
         (if binaryp
             (htm
              (:p "This is a binary resource that you can modify by uploading a file containing new data")
              (:p "File: " (:input :type "file" :name "file"))
              (:p 
               (:a :href (wiki-resource-url request-response wiki-resource)
                "View the current contents")
               " of this binary resource"))
           (htm
            (:textarea 
             :name :contents :cols "80" :rows "25"
             :style "width:80%;height:60%;background-color:lightgray;border:1px solid black;"
             (esc contents))))
         :br
         "To make your changes permanent press "
         (:input :type "submit" :name :save :value "Save")
         "- to cancel, go backwards using your browser or just "
         (:a :href (wiki-resource-url request-response wiki-resource)
          "view the current resource")))))))
    
(defun show (request-response)
  "Action to show a resource, in both web-apps"
  (let ((wiki-resource (get-wiki-resource-from-request request-response)))
    (show-toplevel-wiki-resource request-response wiki-resource)))

;;;; eof
