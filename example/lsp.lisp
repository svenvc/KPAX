;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: lsp.lisp,v 1.4 2004/10/05 13:56:23 sven Exp $
;;;;
;;;; An implementation of Lisp Server Pages inside a KPAX Web Application
;;;;
;;;; Some functionality is originally from a Lisp Server Pages implementation by John Wiseman,
;;;; this code tagged by JW is copyrighted:
;;;;
;;;; Copyright 2001, 2002 I/NET Inc. (http://www.inetmi.com/)
;;;; John Wiseman (jjwiseman@yahoo.com)
;;;; 2002-06-10
;;;; Licensed under the MIT license
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :lsp
  (:index 'show-lsp)
  (:unsecure t))

;;; Start code from JW's LSPs (modified/simplified)

(defun construct-lsp-function (file)
  "Builds and compiles the request-handling LSP function for the page whose contents are in FILE (JW)"
  (let ((form (process-lsp-file file)))
    (compile nil form)))

(defun process-lsp-file (file)
  "Given an .lsp file, builds and returns the corresponding lisp function form (JW)"
  `(lambda (request-response)
     ;; We punt hard on the issue of package. (everything in :kpax-user)
     ,(let ((*package* (find-package :kpax-user)))
        (read-from-string
         (format nil "(progn ~A)" (construct-lsp-body-string (contents-of-file file)))))))

(defun contents-of-file (pathname)
  "Returns a string with the entire contents of the specified file (JW)"
  (with-output-to-string (contents)
    (with-open-file (in pathname :direction :input)
      (s-utils:copy-stream in contents))))

;; (i) Converts text outside <% ... %> tags (straight HTML) into calls
;; to net.html.generator.html, (ii) Text inside <% ... %>
;; ("scriptlets") is straight lisp code, (iii) Text inside <%= ... %>
;; ("expressions") becomes the body of the net.html.generator:html
;; macro.

(defun construct-lsp-body-string (lsp-string &optional (start 0))
  "Takes a string containing an LSP page and returns a string containing the lisp code that implements that page (JW)"
  (multiple-value-bind (start-tag start-code tag-type)
      (next-code lsp-string start)
    (if (not start-tag)
      (format nil "(html-part (out request-response) ~S)" (subseq lsp-string start))
      (let ((end-code (search "%>" lsp-string :start2 start-code)))
	(if (not end-code)
	  (error "EOF reached in LSP inside open '<%' tag.")
	  (format nil "(html-part (out request-response) ~S) ~A ~A"
		  (subseq lsp-string start start-tag)
		  (format nil (tag-template tag-type)
			  (subseq lsp-string start-code end-code))
		  (construct-lsp-body-string lsp-string (+ end-code 2))))))))

;; Finds the next scriptlet or expression tag in LSP source.  Returns
;; nil if none are found, otherwise returns 3 values:
;;  1. The position of the opening bracket (<) of the tag.
;;  2. The position of the contents of the tag.
;;  3. The type of tag (:scriptlet or :expression).

(defun next-code (string start)
  "(JW)"
  (let ((start-tag (search "<%" string :start2 start)))
    (if (not start-tag)
      nil
      (if (and (> (length string) (+ start-tag 2))
	       (eql (char string (+ start-tag 2)) #\=))
	(values start-tag (+ start-tag 3) :expression)
	(values start-tag (+ start-tag 2) :scriptlet)))))

;; Given a tag type (:scriptlet or :expression), returns a format
;; string to be used to generate source code from the contents of the
;; tag.

(defun tag-template (tag-type)
  "(JW)"
  (ecase tag-type
    ((:scriptlet) "~A")
    ((:expression) "(html-part (out request-response) ~A)")))

;;; End code from JW's LSPs

;;; KPAX specific part

(defun get-lsp-dir (lsp-web-app)
  (let ((parent (s-utils:pathname-parent (get-option lsp-web-app :load-truename))))
    (s-utils:make-subdirectory parent "lsp")))

(defun get-lsp-function (lsp-web-app lsp-name)
  (let* ((lsp-cache (or (get-attribute lsp-web-app :lsp-cache)
                        (setf (get-attribute lsp-web-app :lsp-cache) (make-hash-table))))
         (lsp-defaults (or (get-attribute lsp-web-app :lsp-defaults)
                           (setf (get-attribute lsp-web-app :lsp-defaults) 
                                 (merge-pathnames "index.lsp" (get-lsp-dir lsp-web-app)))))
         (lsp-lock (or (get-attribute lsp-web-app :lsp-lock)
                       (setf (get-attribute lsp-web-app :lsp-lock)
                             (s-sysdeps:make-process-lock "LSP Lock"))))
         (lsp-file (merge-pathnames lsp-name lsp-defaults))
         (cached-lsp-function.timestamp (s-sysdeps:with-process-lock (lsp-lock) 
                                          (gethash lsp-name lsp-cache))))
    (when (probe-file lsp-file)
      (if (and cached-lsp-function.timestamp
               (< (file-write-date lsp-file) (cdr cached-lsp-function.timestamp)))
          (car cached-lsp-function.timestamp)
        (let ((now (get-universal-time))
              (lsp-function (construct-lsp-function lsp-file)))
          (when lsp-function
            (s-sysdeps:with-process-lock (lsp-lock) 
              (setf (gethash lsp-name lsp-cache) (cons lsp-function now)))
            lsp-function))))))

(defun show-lsp (request-response)
  (let* ((lsp-name (or (second (get-request-sub-path request-response)) "index"))
         (lsp-function (get-lsp-function (get-application request-response) lsp-name)))
    (if lsp-function
        (funcall lsp-function request-response)
      (error "Cannot find LSP named '~a'" lsp-name))))

;;;; eof