;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: multipart-form-encoding.lisp,v 1.2 2004/12/16 15:08:36 sven Exp $
;;;;
;;;; Some utilities to deal with multipart form encoded data (RFC 2388)
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export '(parse-multipart-header 
          extract-multipart-parts
          find-multipart-header-attribute
          find-multipart-header-named
          find-multipart-named))

(defun parse-multipart-header (string)
  "Parse a line of the form header-name: header-value; att1-name='att1-value'; att2-name='att2-value' into (header-name header-value ((att1-name . att1-value) (att2-name . att2-value)))"
  (let ((colon-position (position #\: string))
        header-name header-value attributes)
    (setf header-name (string-trim " " (subseq string 0 colon-position)))
    (let ((tokens (s-utils:tokens string :start (1+ colon-position) :separators ";")))
      (setf header-value (string-trim " " (pop tokens)))
      (dolist (token tokens)
        (let ((subtokens (s-utils:tokens token :separators "=")))
          (push (cons (string-trim " " (first subtokens))
                      (string-trim "\"'" (second subtokens)))
                attributes)))
      (append (list header-name header-value) attributes))))

(defun read-until-boundary (in boundary out)
  "Read lines from stream in until boundary or eof is read, writing lines to stream out, returns t for end-boundary"
  (let ((boundary-length (length boundary))
        line line-length subsequent-lines last-char)
    (loop
     ;; read line by line 
     ;; XXX: lines might be unreasonably large if there are no linebreaks !
     ;; XXX: let's hope no char based conversions are done here !
     (setf line (read-line in nil nil)
           line-length (length line))
     (cond ((or (null line) 
                (string= line boundary :end1 (min line-length boundary-length)))
            ;; stop when either the boundary if read or eof is encountered
            (return (string= line "--" :start1 (- line-length 3) :end1 (1- line-length))))
           (t 
            ;; if this is not the first line
            (when subsequent-lines
              (if last-char (write-char last-char out))
              (terpri out))
            (setf subsequent-lines t)
            (setf last-char (when (plusp (length line)) (char line (1- (length line)))))
            ;; if the last char of the line is Return, postpone writing it until we know for sure
            ;; that it is not the last line (in which case the last Return+Linefeed are skipped)
            (if (and last-char (char= last-char #\Return))
                (write-string line out :end (1- (length line)))
              (progn
                (setf last-char nil)
                (write-string line out))))))))

(defvar *tmp-files-location* #p"/tmp/")
(defvar *tmp-files-counter* 0)
(defvar *tmp-file-lock* (s-sysdeps:make-process-lock "tmp-file-lock"))

(defun make-tmp-file ()
  "Create a new uniquely named tmp file and return its pathname"
  (s-sysdeps:with-process-lock (*tmp-file-lock*)
    (loop
     (let ((pathname (merge-pathnames (make-pathname :name (format nil "tmp-file-~d" (incf *tmp-files-counter*))
                                                     :type "bin")
                                      *tmp-files-location*)))
       (unless (probe-file pathname)
         (with-open-file (out pathname 
                              :direction :output :if-does-not-exist :create :if-exists :error)
           (declare (ignore out)))
         (return pathname))))))

(defun extract-multipart-parts (stream-or-body-string &key use-tmp-files-for-data)
  "Extract all parts of a multipart/form-data encoded stream-or-body-string into ((part1-headers part1-data) ..)"
  (let* ((in (if (stringp stream-or-body-string) 
                 (make-string-input-stream stream-or-body-string) 
               stream-or-body-string))
         (boundary (string-right-trim '(#\Return #\Newline) (read-line in nil nil)))
         parts headers data line end-p)
    (loop
     ;; parse and collect all header lines
     (setf headers nil)
     (loop
      (when end-p (return-from extract-multipart-parts parts))
      (setf line (read-line in nil nil))
      (when line (setf line (string-right-trim '(#\Return #\Newline) line)))
      (cond ((null line) (return-from extract-multipart-parts parts))
            ((equal line "") (return))
            (t (push (parse-multipart-header line) headers))))
     ;; either read the data as a string, or if the data is for a file,
     ;; return a pathname to a tmp file with the data in it
     (setf data (if (and use-tmp-files-for-data
                         (find-multipart-header-attribute "filename" 
                                                          (find-multipart-header-named "Content-Disposition" headers)))
                    (let ((tmp-file (make-tmp-file)))
                      (with-open-file (out tmp-file 
                                           :direction :output :if-does-not-exist :error :if-exists :supersede)
                        (setf end-p (read-until-boundary in boundary out))
                        tmp-file))
                  (with-output-to-string (out)
                    (setf end-p (read-until-boundary in boundary out)))))
     ;; finish the part
     (push (list headers data) parts))))

(defun find-multipart-header-attribute (attribute-name multipart-header)
  "Find the value of a named attribute in a multipart/form-data decoded header"
  (let ((attributes (rest (rest multipart-header))))
    (cdr (find attribute-name attributes :test #'string-equal :key #'first))))

(defun find-multipart-header-named (header-name multipart-headers)
  "Find a named header in a list of multipart/form-data decoded headers"
  (find header-name multipart-headers :test #'string-equal :key #'first))

(defun find-multipart-named (part-name parts)
  "Find a named part in a list of multipart/form-data decoded parts"
  (loop 
   :for (header data) :in parts 
   :do (let ((content-disposition-header (find-multipart-header-named "Content-Disposition" header)))
         (when content-disposition-header
           (let ((name (find-multipart-header-attribute "name" content-disposition-header)))
             (when (string-equal name part-name)
               (return-from find-multipart-named (list header data))))))))

;;;; eof