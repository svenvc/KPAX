;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: helloworld1.lisp,v 1.11 2004/09/09 11:21:37 sven Exp $
;;;;
;;;; This welcome web application is a public entry point into the KPAX examples.
;;;;
;;;; Copyright (C) 2006 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :welcome
  (:index 'welcome-start)
  (:static-root "static/")
  (:unsecure t))

(defparameter *demo-web-apps*
  '((:helloworld1 
     "A simple, mostly static web application"
     "Learn about the structure of web apps.")
    (:factorial1 
     "A simple web app that computes arbitrary factorials."
     "Learn about basic form processing.")
    (:snoop1 
     "A web app that prints out all request headers and request parameters."
     "Learn about getting data from the request-response object.")
    (:session1
     "A web app that prints out session related info and tracks invocations"
     "Learn about basic session management.")
    (:benchmark1
     "A simple, dynamically computed page conforming to the DW-Bench spec."
     "Compare KPAX performance to your favorite alternative.")
    (:menu1
     "A test application for the menu(bar) system on top of KPAX."
     "Learn how to easily define and work with a dynamic menu.")
    (:tabs1
     "A test application for the tabs system on top of KPAX."
     "Learn how to easily define and work with tabs.")
    (:forms2
     "A test application for the sophisticated form processing system on top of KPAX"
     "Learn how to define and work with powerful data driven forms.")
    (:calculator
     "A complete, basic calculator web app."
     "Learn about request tracking and model-view-controller.")
    (:reddit
     "The famous Reddit clone written in less than 100 lines of elegant code."
     "Learn about the power and elegance of KPAX web app development.")))

(defun welcome-start (request-response)
  (html-page (out request-response)
    (:html 
     (:head 
      (:title "Welcome to KPAX") 
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:h1 "Welcome to KPAX")
      (:div :class "NX_panel" :style "width:90%;"
       (:span :class "NX_title" "Overiew")
       (:div :class "NX_border"
        (:p :style "width:98%;"
         "KPAX is a Common Lisp Web Application Framework."
         :br
         "You can learn more about KPAX from its website: "
         (:a :href "http://homepage.mac.com/svc/kpax/" "http://homepage.mac.com/svc/kpax/")
         ".")
        (:p :style "width:98%;"
         "Please explore some of the following public demo applications:")
        (:table :class "NX_table" :width "100%"
         (:tr (:th :width "20%" "Web App Name") (:th "Description") (:th "Source Code"))
         (let ((server (get-server request-response)))
           (loop :for (web-app-name . description) :in *demo-web-apps* :for i :upfrom 1 :do
                 (let ((web-app (get-web-app web-app-name)))
                   (htm 
                    (:tr :class (if (evenp i) "NX_even_row" "NX_odd_row")
                     (:td (:a :href (get-home-url web-app server) (str web-app-name)))
                     (:td (dolist (line description)
                            (htm (str line) :br)))
                     (:td 
                      (:a :href (dynamic-url request-response 'serve-html-source-file :index (1- i)) 
                       "View")
                      " "
                      (:a :href (dynamic-url request-response 'serve-raw-source-file :index (1- i)) 
                       "Download"))))))))
        (:p :style "width:98%;"
         "There are many more interesting examples in the KPAX source distribution.")))))))

(defvar *source-directory* (s-utils:pathname-parent (or *load-truename* *compile-file-truename*)))

(defun serve-raw-source-file (request-response)
  (let* ((index (s-utils:parse-integer-safely (get-request-parameter-value request-response :index)))
         (demo (ignore-errors (elt *demo-web-apps* index))))
    (if demo
        (let* ((filename (make-pathname :name (string-downcase (first demo)) :type "lisp"))
               (pathname (merge-pathnames filename *source-directory*)))
          (setf (get-response-mime-type request-response) 
                "text/common-lisp"
                (get-response-header-value request-response "Content-Disposition")
                (format nil "attachment; filename=~a" filename))
          (with-open-file (in pathname)
            (s-utils:copy-stream in (get-content-stream request-response))))
      (html-message request-response "Error" "Illegal source file specification"))))

;; simple common lisp to colorized html conversion:
;; based on Marc Battyani's pprint.lisp - a very easy and elegant hack
;; does not work (but also doesn't fail) with multiline strings - svc

(defun split-comment (line)
  (let ((comment-position (position #\; line)))
    (if comment-position
        (values (subseq line 0 comment-position)
                (subseq line comment-position))
      line)))

(defun clean-line (line)
  (map 'string 
       #'(lambda (char)
           (if (find char "()'`#	") #\space char))
       line))

(defparameter *definitions* 
  '(defun defvar defparameter defconstant defclass defmethod defgeneric defcondition defstruct))

(defparameter *forms* 
  '(if cond case ecase typecase let let* letrec labels flet
    multiple-value-bind destructuring-bind
    values loop when unless return return-from
    do do* dolist dotimes declare declaim
    with-slots with-open-file with-open-stream 
    with-input-from-string with-output-to-string
    progn prog1 prog2 
    ignore-errors handler-bind handler-case unwind-protect))

(defparameter *special*
  '(&rest &optional &key &allow-other-keys &aux))

(defun process-lisp-line (line stream)
  (multiple-value-bind (code comment)
      (split-comment line)
    (let* ((cleaned-line (clean-line code))
           (start 0)
           (trimmed 0)
           (length (length cleaned-line))
           decorations)
      (loop
       (setf trimmed (position #\space cleaned-line :start start :test #'char/=))
       (unless (and trimmed (< trimmed length))
         (return))
       (multiple-value-bind (obj end)
           (ignore-errors
             (read-from-string cleaned-line nil nil :start trimmed :preserve-whitespace t))
         (unless (numberp end)
           (setf end (position #\space cleaned-line :start trimmed :test #'char=)))
         (unless (and (numberp end) (<= end length))
           (return))
         (cond ((keywordp obj) (push (list trimmed end #\p) decorations))
               ((stringp obj) (push (list trimmed end #\g) decorations))
               ((member obj *definitions*) (push (list trimmed end #\b) decorations))
               ((member obj *forms*) (push (list trimmed end #\b) decorations))
               ((member obj *special*) (push (list trimmed end #\g) decorations)))
         (setf start end)))
      (setf start 0)
      (loop :for (start-token end-token category) :in (nreverse decorations) :do
            (when (/= start start-token)
              (write-string (escape-string-minimal (subseq line start start-token)) stream))
            (format stream "<span class='~a'>" category)
            (write-string (escape-string-minimal (subseq line start-token end-token)) stream)
            (format stream "</span>" category)
            (setf start end-token))
      (when (< start length)
        (write-string (escape-string-minimal (subseq line start length)) stream))
      (when comment
        (format stream "<span class='r'>~a</span>" (escape-string-minimal comment))))
    (terpri stream)))

(defun process-lisp-stream (in-stream out-stream title)
  (with-html-output (out out-stream :prologue t)
    (terpri out)
    (:html
     (:head 
      (:title (str title))
      (:style :type "text/css"
       ".p { color: purple; } .g { color:green; } .b { color:blue; } .r { color:darkred; }"))
     (:body
      (:pre
       (loop :for line = (read-line in-stream nil)
             :while line
             :do (process-lisp-line line out)))))))

(defun serve-html-source-file (request-response)
  (let* ((index (s-utils:parse-integer-safely (get-request-parameter-value request-response :index)))
         (demo (ignore-errors (elt *demo-web-apps* index))))
    (if demo
        (let* ((filename (make-pathname :name (string-downcase (first demo)) :type "lisp"))
               (pathname (merge-pathnames filename *source-directory*)))
          (with-open-file (in pathname)
            (process-lisp-stream in 
                                 (get-content-stream request-response)
                                 filename)))
      (html-message request-response "Error" "Illegal source file specification"))))

#|
(s-http-server:register-context-handler (get-s-http-server *web-app-server*)
                                        "/" 
                                        's-http-server:redirect-handler
                                        :arguments (list "/kpax/dynamic/welcome")
                                        :at-end-p t)

(setf *all-links* (copy-list *demo-all-links*)
      *id-counter* *demo-id-counter*)

(let ((user (create-new-demo-user))
      (data '((ID 1)
              (FULLNAME "Sven Van Caekenberghe")
              (USERNAME "sven")
              (PASSWORD "foobar")
              (NOTES "This is a note!")
              (AGE 39)
              (GENDER :MALE)
              (LANGUAGE DUTCH)
              (INTERESTS (SEX ROCK-AND-ROLL HACKING))
              (ENABLED-P T)
              (MUSIC-TASTES (ROCK ALTERNATIVE NEW-WAVE)))))
  (loop for (slot value) in data do (setf (slot-value user slot) value)))
|#

;;;; eof