;;;; -*- mode: lisp -*-
;;;;
;;;; Testing UTF-8 encoding/decoding and form processing
;;;;
;;;; Copyright (C) 2009 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

;;; the web app definition

(defwebapp :utf8
  (:index 'utf8-start)
  (:static-root "static/")
  (:request-setup-hook 'utf8-request-setup-hook)
  (:unsecure t))

(defun utf8-request-setup-hook (req/resp)
  (setf (get-response-mime-type req/resp) "text/html; charset=utf-8"
        (get-content-stream req/resp) (make-string-output-stream :element-type 'lw:simple-char)
        (get-attribute req/resp :commit-contents-handler) 'utf8-commit-contents-handler
        (get-attribute req/resp :commit-length-handler) 'utf8-commit-length-handler
        (get-attribute req/resp :decode-parameter-handler) 'utf8-decode-parameter-handler))

(defun utf8-commit-contents-handler (contents-string socket-stream req/resp)
  (if (search "utf-8" (get-response-mime-type req/resp) :test 'char-equal)
      (with-input-from-string (in contents-string)
        (loop :for char = (read-char in nil :eof)
              :until (eql char :eof)
              :do (cl9:write-utf8-char char socket-stream)))
    (write-string contents-string socket-stream)))

(defun utf8-commit-length-handler (contents-string req/resp)
  (if (search "utf-8" (get-response-mime-type req/resp) :test 'char-equal)
      (cl9:utf8-encoded-length contents-string)
    (length contents-string)))

(defun utf8-decode-parameter-handler (parameter-string request-response)
  (declare (ignore request-response))
  (cl9:utf8-encoded-bytes->string (map 'vector 'char-code (uri-decode-for-query parameter-string))))

(defparameter +french+ (map 'string 'code-char '(233 108 232 118 101 32 101 110 32 70 114 97 110 231 97 105 115)))

(defparameter +czech+ (with-output-to-string (out nil :element-type 'lw:simple-char)
                        (loop :for c :in '(77 367 106 32 250 269 101 116)
                              :do (write-char (code-char c) out))))

(defun utf8-start (req/resp)
  (html-page (out req/resp)
    (:html 
     (:head 
      (:title "UTF-8")
      (:meta :http-equiv "Content-Type" :content "text/html; charset=utf-8")
      (:link :rel "stylesheet" :type "text/css" :href (static-url req/resp :server "nx.css")))
     (:body 
      (:h1 "UTF-8")
      (:p "Thess are some tests:"
       (:ul
        (:li "French: " (str +french+))
        (:li "Czech: " (str +czech+))))
      (:div :class "NX_form"
       (:form :action (dynamic-url req/resp 'process-utf8-form) :method :post
        (:input :type "text" :name "field" :value "")
        (:input :type "submit" :value "Submit")))))))

(defun process-utf8-form (req/resp)
  (html-page (out req/resp)
    (:html 
     (:head 
      (:title "UTF-8 Form Result")
      (:meta :http-equiv "Content-Type" :content "text/html; charset=utf-8")
      (:link :rel "stylesheet" :type "text/css" :href (static-url req/resp :server "nx.css")))
     (:body 
      (:h1 "UTF-8 Form Result")
      (:div :class "NX_panel"
       (:span :class "NX_title" "Result")
       (:div :class "NX_border"
        (:p
         (fmt "Method is ~a" (get-request-method req/resp)))
        (:p 
         (fmt "Field value is '~a'" (get-request-parameter-value req/resp "field")))
        (:div :class "NX_button_group"
         (:a :class "NX_button" :href (dynamic-url req/resp 'utf8-start) "Back"))))))))

;;;; eof
