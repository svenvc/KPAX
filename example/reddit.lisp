;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: helloworld1.lisp,v 1.11 2004/09/09 11:21:37 sven Exp $
;;;;
;;;; The 'Reddit.lisp' example featured in the Lisp Movie:
;;;; Episode 2: (Re)writing Reddit in Lisp is 20 minutes and 100 lines
;;;; See: http://homepage.mac.com/svc/LispMovies/index.html
;;;;
;;;; This example is *not* loaded automatically by the ASDF :kpax-examples
;;;; since it needs S-HTTP-CLIENT (load this dependency first)
;;;;
;;;; Copyright (C) 2005,2007 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :reddit
  (:index 'reddit-home)
  (:static-root "static/")
  (:unsecure t))

(defvar *id-counter* 0)

(defclass reddit-link ()
  ((url :reader get-url :initarg :url :initform nil)
   (title :reader get-title :initarg :title :initform "")
   (id :reader get-id :initform (incf *id-counter*))
   (timestamp :reader get-timestamp :initform (get-universal-time))
   (points :accessor get-points :initform 0)))

(defvar *all-links* '())

(defun add-new-link (url title)
  (push (make-instance 'reddit-link :url url :title title) *all-links*))

(defun get-sorted-links (sort-by)
  (let ((links (sort (copy-list *all-links*) #'> :key sort-by)))
    (subseq links 0 (min (length links) 25))))

(defun get-link-with-id (id)
  (find id *all-links* :key #'get-id))

(defun render-link (request-response link)
  (with-slots (url title timestamp points id)
      link
    (html-part (out request-response)
      (:li 
       (:a :href url :title url (str title))
       (fmt "Posted ~a ago. ~d point~:p. " (s-utils:format-duration (max 1 (- (get-universal-time) timestamp))) points)
       (:a :href (dynamic-url request-response 'reddit-up :id id) :title "Vote this link up" "Up")
       (:a :href (dynamic-url request-response 'reddit-down :id id) :title "Vote this link down" "Down")))))

(defun reddit-home (request-response)
  (html-page (out request-response)
    (:html
     (:head 
      (:title "Reddit.lisp") 
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :webapp "reddit.css")))
     (:body 
      (:h1 "Reddit.lisp") (:h3 "In less than 100 lines of elegant code")
      (:p 
       (:a :href (dynamic-url request-response nil) :title "Reload the Reddit.lisp Home page" "Refresh")
       (:a :href (dynamic-url request-response 'reddit-new-link) :title "Submit a new link" "New link"))
      (:h2 "Highest Ranking Links")
      (:ol
       (loop :for link :in (get-sorted-links #'get-points) :do
             (render-link request-response link)))
      (:h2 "Latest Links")
      (:ol
       (loop :for link :in (get-sorted-links #'get-timestamp) :do
             (render-link request-response link)))))))

(defun reddit-new-link (request-response &optional message url title)
  (let* ((x (random 10))
         (y (random 10)))
    (setf (get-attribute (get-session request-response) :sum) (+ x y))
    (html-page (out request-response)
      (:html
       (:head 
        (:title "Reddit.lisp - Submit a new link") 
        (:link :rel "stylesheet"  :type "text/css" :href (static-url request-response :webapp "reddit.css")))
       (:body 
        (:h1 "Reddit.lisp") (:h3 "Submit a new link")
        (when message (htm (:p (str message))))
        (:form :action (dynamic-url request-response 'reddit-submit-new-link) :method "post"
         (:input :type "text" :name "url" :value (or url "http://") :size 48 :title "The URL of the new link")
         (:input :type "text" :name "title" :value (or title "Title") :size 48 :title "The title of the new link")
         (:p (fmt "How much is ~r plus ~r ?" x y))
         (:input :type "text" :name "sum" :value "0")
         (:input :type "submit" :value "I Read It !"))
        (:p (:a :href (dynamic-url request-response nil) :title "Back to the Reddit.lisp Home page" "Home")))))))

(defun is-valid-url (url)
  (ignore-errors
    (multiple-value-bind (contents code)
        (s-http-client:do-http-request url)
      (and (stringp contents) (not (zerop (length contents))) (= 200 code)))))

(defun reddit-submit-new-link (request-response)
  (let ((url (get-request-parameter-value request-response "url"))
        (title (get-request-parameter-value request-response "title"))
        (sum (s-utils:parse-integer-safely (get-request-parameter-value request-response "sum")
                                           :default 0)))
    (cond ((not (eql sum (get-attribute (get-session request-response) :sum)))
           (reddit-new-link request-response "Sorry, you got the sum wrong" url title))
          ((or (null url) (equal url "") (equal url "http://")) 
           (reddit-new-link request-response "Sorry, URL missing" url title))
          ((or (null title) (equal title "") (equal title "Title")) 
           (reddit-new-link request-response "Sorry, title missing" url title))
          ((is-valid-url url) 
           (setf (get-attribute (get-session request-response) :sum) nil)
           (add-new-link url title)
           (redirect-to request-response 'reddit-home))
          (t (reddit-new-link request-response "Sorry, URL is not valid" url title)))))

(defun reddit-up (request-response &optional (delta +1))
  (let* ((id (s-utils:parse-integer-safely (get-request-parameter-value request-response "id")))
         (link (find id *all-links* :key #'get-id)))
    (when link (incf (get-points link) delta))
    (redirect-to request-response 'reddit-home)))

(defun reddit-down (request-response)
  (reddit-up request-response -1))

;;; deployed application maintenance

(defvar *server-secret* (random 1234567890))

(defun reddit-kill (request-response)
  (let ((id (s-utils:parse-integer-safely (get-request-parameter-value request-response "id")))
        (key (s-utils:parse-integer-safely (get-request-parameter-value request-response "key"))))
    (when (equal key *server-secret*)
      (setf *all-links* (remove id *all-links* :key #'get-id)))
    (redirect-to request-response 'reddit-home)))

(defun cleanup-links (&key title url)
  (when title
    (setf *all-links* (remove-if #'(lambda (link)
                                     (search title (get-title link) :test #'char-equal))
                                 *all-links*)))
  (when url
    (setf *all-links* (remove-if #'(lambda (link)
                                     (search url (get-url link) :test #'char-equal))
                                 *all-links*)))
  (length *all-links*))

(defun reddit-link->s-expr (reddit-link)
  (mapcar #'(lambda (slot) (list slot (slot-value reddit-link slot)))
          '(url title timestamp points)))

(defmethod s-expr->reddit-link (s-expr)
  (let ((reddit-link (make-instance 'reddit-link)))
    (loop for (slot value) in s-expr do (setf (slot-value reddit-link slot) value))
    reddit-link))

(defun save-links (&optional (file "/tmp/reddit-links.lisp"))
  (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (print (mapcar #'reddit-link->s-expr *all-links*) out)))

(defun load-links (&optional (file "/tmp/reddit-links.lisp"))
  (setf *id-counter* 0)
  (with-open-file (in file)
    (setf *all-links* (mapcar #'s-expr->reddit-link (read in)))))

;;;; eof
