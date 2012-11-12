;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: forms2.lisp,v 1.19 2004/09/10 13:28:17 sven Exp $
;;;;
;;;; Testing form processing, error handling with web form abstraction
;;;; The example is the management of a list of user records
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

;;; the demo model

(defparameter *interests* '(sex drugs rock-and-roll hacking))

(defparameter *languages* '(english french dutch german))

(defparameter *music-tastes* '(rock pop punk alternative jazz new-wave))

(defparameter *genders* '(:male :female))
 
(defclass demo-user ()
  (id fullname username password notes age gender language interests enabled-p music-tastes))

(defvar *demo-users* (make-hash-table))

(defun list-demo-users ()
  (loop for demo-user being the hash-values in *demo-users* 
        collect demo-user))
 
(defun demo-user-with-id (id)
  (gethash id *demo-users*))

(defun create-new-demo-user ()
  (let ((new-id (1+ (hash-table-count *demo-users*))) ;; this is wrong !! (cfr. nicky)
        (demo-user (make-instance 'demo-user)))
    (with-slots (id fullname username password notes age gender language interests enabled-p music-tastes) 
        demo-user
      (setf id new-id fullname nil username nil password nil notes nil age 0 
            gender nil language 'english interests nil enabled-p t music-tastes nil))
    (setf (gethash new-id *demo-users*) demo-user)
    demo-user))

(defun remove-demo-user (demo-user)
  (with-slots (id) demo-user
    (remhash id *demo-users*)))

;;; the demo web form

(defun parse-symbol-kpax-user (string)
  (parse-symbol string :kpax-user))

(defun contains-no-spaces (value)
  (if (stringp value)
      (if (find #\space value) 
          (values nil :contains-spaces) 
        t) 
    t))

(defwebform demo-web-form
  ((:group personal-info 
    :label "Personal Info"
    :members ((fullname :text :label "Fullname")
              (username :text :label "Username" 
                        :options (:size 10)
                        :validator (all required (limited-string 8 4) contains-no-spaces))
              (password :password :label "Password"
                        :options (:size 10)
                        :validator (or optional (limited-string 32)))
              (password2 :password :label "Password"
                         :options (:size 10) :comment "[Confirmation]"
                         :validator (or optional (limited-string 32)))
              (gender :choice
                      :options (:values *genders* :style :buttons)
                      :parser parse-keyword :formatter string-capitalize
                      :validator (or optional (list-element *genders*)))
              (age :text :label "Age" 
                   :options (:size 4)
                   :parser s-utils:parse-integer-safely 
                   :validator (integer-range 0 150))))
   (:group admin-info
    :label "Admin Info"
    :members ((notes :text-area :label "Notes"
                     :options (:cols 40 :rows 10)
                     :validator (or optional (limited-string 256)))
              (enabled-p :choice :label "Enabled"
                         :options (:values :boolean)
                         :parser parse-boolean
                         :validator boolean)))
   (:group preferences
    :label "Preferences"
    :members ((interests :choice
                         :options (:values *interests* :selection :multiple :style :buttons)
                         :parser parse-symbol-kpax-user :formatter string-capitalize
                         :validator (or optional (list-elements *interests*)))
              (language :choice
                        :options (:values *languages* :style :list)
                        :parser parse-symbol-kpax-user :formatter string-capitalize
                        :validator (or optional (list-element *languages*)))
              (music-tastes :choice
                            :options (:values *music-tastes* :style :list :selection :multiple)
                            :parser parse-symbol-kpax-user :formatter string-capitalize
                            :validator (or optional (list-elements *music-tastes*)))))
   (id :hidden))
  (:title "Demo User")
  (:validator validate-demo-web-form)
  (:submit process-demo-web-form))

(defun validate-demo-web-form (form)
  (let ((password-1 (field-value form 'password))
        (password-2 (field-value form 'password2)))
    (if (or (equal password-1 password-2)
            (and (null password-1) (null password-2)))
        t
      (values nil "You did not confirm your password properly: fill it in twice"))))

(defun populate-demo-web-form (demo-user)
  (let ((web-form (instanciate-web-form 'demo-web-form)))
    (copy-slots-object->form demo-user web-form 
                             '(id username fullname age notes enabled-p interests gender language music-tastes))
    (setf (get-submit-text web-form) "Update")
    web-form))

(defun new-demo-web-form ()
  (let ((web-form (instanciate-web-form 'demo-web-form)))
    (setf (get-submit-text web-form) "Create")
    (setf (field-value web-form 'age) 0)
    web-form))

(defun commit-demo-web-form (web-form)
  (let* ((id (field-value web-form 'id))
         (demo-user (demo-user-with-id (s-utils:parse-integer-safely id))))
    (when (and (null demo-user) (null id))
      (setf demo-user (create-new-demo-user)))
    (when demo-user
      ;; don't copy password when it is nil (users can't clear passwords, only change them)
      (let ((slots-to-copy '(username fullname age notes enabled-p interests gender language music-tastes)))
        (when (field-value web-form 'password) 
            (push 'password slots-to-copy))
        (copy-slots-form->object web-form demo-user slots-to-copy)))))

;;; the web app definition

(defwebapp :forms2
  (:index 'forms2-index)
  (:unsecure t))

(defun forms2-index (request-response)
  (html-page (out request-response)
    (:html 
     (:head 
      (:title "Demo Users Management")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:h1 "Demo Users Management")
      (:table :class "NX_table" :width "100%"
       (:tr (:th "ID") (:th "Username") (:th "Fullname") (:th "Actions"))
       (dolist (demo-user (list-demo-users))
         (with-slots (id username fullname) demo-user
           (htm
            (:tr 
             (:td (str id)) (:td (str username)) (:td (str fullname))
             (:td
              (:a :href (dynamic-url request-response 'edit-demo-user :id id) "Edit")
              " | "
              (:a :href (dynamic-url request-response 'delete-demo-user :id id) "Delete")))))))
      (:div :class "NX_button_group" :style "margin-top:20px"
       (:a :class "NX_button" :href (dynamic-url request-response 'new-demo-user) "New User")
       (:a :class "NX_button" :href (dynamic-url request-response nil) "Refresh"))))))

(defun new-demo-user (request-response)
  (present-demo-web-form request-response (new-demo-web-form)))

(defun edit-demo-user (request-response)
  (let* ((id (s-utils:parse-integer-safely (get-request-parameter-value request-response "id")))
         (demo-user (demo-user-with-id id)))
    (if demo-user
        (present-demo-web-form request-response (populate-demo-web-form demo-user))
      (demo-error request-response "Unknown demo user id"))))

(defun present-demo-web-form (request-response demo-web-form)
  (let ((*locale* :en)
        (*localization-source* (or *simple-localization-source* (init-localized-strings))))
    (html-page (out request-response)
      (:html 
       (:head
        (:title (str (get-title demo-web-form)))
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
       (:body 
        (render demo-web-form request-response (make-options '(:div-form "NX_form")
                                                             *default-web-form-render-options*))
        (:div :class "NX_button_group" :style ""
         (:a :class "NX_button" :href (dynamic-url request-response nil) "List All")))))))

(defun process-demo-web-form (request-response)
  (let ((demo-web-form (reconstruct-web-form request-response 'demo-web-form)))
    (setf (get-attribute request-response :web-form) demo-web-form) ;; for debugging
    (if (validate demo-web-form)
        (progn
          (commit-demo-web-form demo-web-form)
          (forms2-index request-response))
      (present-demo-web-form request-response demo-web-form))))

(defun delete-demo-user (request-response)
  (let* ((id (s-utils:parse-integer-safely (get-request-parameter-value request-response "id")))
         (demo-user (demo-user-with-id id)))
    (if demo-user
        (progn
          (remove-demo-user demo-user)
          (forms2-index request-response))
      (demo-error request-response "Unknown demo user id"))))

(defun demo-error (request-response message)
  (html-message request-response "Error" "~a ~a" 
                message 
                (with-html-output-to-string (out) 
                  (:a :href (dynamic-url request-response nil) "OK"))))

;;;; eof
