;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: web-form.lisp,v 1.23 2004/11/26 13:23:26 sven Exp $
;;;;
;;;; A simple web form framework
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(defwebform ensure-web-form-definition get-web-form-definition 
   instanciate-web-form validate reconstruct-web-form
   render make-web-form-render-options
   field-value get-submit-text get-title get-value-field
   copy-slots-form->object copy-slots-object->form
   parse-boolean parse-symbol parse-keyword
   standard-web-form-renderer standard-web-form-element-renderer
   *default-web-form-render-options*))

;;; web form and field definition model

(defclass web-form-definition ()
  ((name :accessor get-name :initarg :name)
   (title :accessor get-title :initform nil) 
   (fields :accessor get-fields :initform nil) 
   (intro :accessor get-intro :initform nil)
   (comments :accessor get-comments :initform nil)
   (form-remark :accessor get-form-remark :initform nil)
   (validator :accessor get-validator :initform (constantly t)) 
   (submit-function :accessor get-submit-function)
   (submit-text :accessor get-submit-text :initform "Submit")))

(defclass web-form-field-definition ()
  ((parent :accessor get-parent :initarg :parent) 
   (name :accessor get-name :initarg :name)
   (label :accessor get-label :initform nil) 
   (type :accessor get-type :initarg :type)
   (formatter :accessor get-formatter :initform #'princ-to-string)
   (parser :accessor get-parser :initform #'identity)
   (validator :accessor get-validator :initform (constantly t)) 
   (options :accessor get-options :initform nil)
   (comments :accessor get-comments :initform nil)))

(defclass web-form-group-definition ()
  ((parent :accessor get-parent :initarg :parent)
   (name :accessor get-name :initarg :name)
   (label :accessor get-label :initform nil)
   (options :accessor get-options :initform nil)
   (fields :accessor get-fields :initform nil)))

;;; web form and field instance model

(defclass web-form ()
  ((definition :accessor get-definition :initarg :definition) 
   (id :accessor get-id :initarg :id)
   (fields :accessor get-fields :initform nil)
   (submit-text :initform nil)
   (error-messages :accessor get-error-messages :initform nil)))

(defclass web-form-field ()
  ((options :accessor get-options :initform nil)
   (definition :accessor get-definition :initarg :definition) 
   (value :accessor get-value :initform nil) 
   (error-messages :accessor get-error-messages :initform nil)))

(defclass web-form-group ()
  ((definition :accessor get-definition :initarg :definition)
   (fields :accessor get-fields :initform nil)))

;;; model support

(defmethod print-object ((object web-form-definition) stream)
  (with-slots (name) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~a" name))))

(defmethod print-object ((object web-form-field-definition) stream)
  (with-slots (name parent) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~a ~a" name (get-name parent)))))

(defmethod print-object ((object web-form-group-definition) stream)
  (with-slots (name parent) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~a ~a" name (get-name parent)))))

(defmethod print-object ((object web-form) stream)
  (with-slots (id definition) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "#~d ~a" id (get-name definition)))))

(defmethod print-object ((object web-form-field) stream)
  (with-slots (definition) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~a" (get-name definition)))))

(defmethod print-object ((object web-form-group) stream)
  (with-slots (definition) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~a" (get-name definition)))))

;;; defining web forms

(defun process-field-specs (parent field-specs)
  (with-slots (fields) parent
    (dolist (field-spec field-specs)
      (if (eq (first field-spec) :group)
          (destructuring-bind (name &key label members)
              (rest field-spec)
            (let ((web-form-group-definition (make-instance 'web-form-group-definition
                                                            :parent parent
                                                            :name name)))
              (when label (setf (get-label web-form-group-definition) label))
              (setf (get-fields web-form-group-definition)
                    (process-field-specs web-form-group-definition members))
              (push web-form-group-definition fields)))
        (destructuring-bind (name type &key label validator options comments formatter parser &allow-other-keys) 
            field-spec
          (let ((web-form-field-definition (make-instance 'web-form-field-definition
                                                          :parent parent
                                                          :name name
                                                          :type type)))
            (when label (setf (get-label web-form-field-definition) label))
            (when validator (setf (get-validator web-form-field-definition) 
                                  (compile-validator-expression validator)))
            (when formatter (setf (get-formatter web-form-field-definition) formatter))
            (when parser (setf (get-parser web-form-field-definition) parser))
            (when options (setf (get-options web-form-field-definition) options))
            (when comments (setf (get-comments web-form-field-definition) comments))
            (push web-form-field-definition fields)))))
    (setf fields (nreverse fields))))

(defmethod process-options ((web-form-definition web-form-definition) options)
  (destructuring-bind (&key title validator intro comments submit form-remark &allow-other-keys) 
      (reduce #'append options)
    (when title (setf (get-title web-form-definition) title))
    (when submit (setf (get-submit-function web-form-definition) submit))
    (when intro (setf (get-intro web-form-definition) intro))
    (when comments (setf (get-comments web-form-definition) comments))
    (when form-remark (setf (get-form-remark web-form-definition) form-remark))
    (when validator (setf (get-validator web-form-definition) validator))))

(defvar *web-form-definitions* (make-hash-table))

(defun get-web-form-definition (name)
  "Get the web form definition with name"
  (gethash name *web-form-definitions*))

(defun ensure-web-form-definition (name field-specs options)
  "Ensure (create or update) a web form with name, field-specs and options"
  (let ((web-form-definition (gethash name *web-form-definitions*)))
    (unless web-form-definition
      (setf web-form-definition (make-instance 'web-form-definition :name name))
      (setf (gethash name *web-form-definitions*) web-form-definition))
    (setf (get-fields web-form-definition) nil) 
    ;; maybe clear field def slots to invalidate existing field instances' refs ?
    (process-field-specs web-form-definition field-specs)
    (process-options web-form-definition options)
    web-form-definition))

(defmacro defwebform (name field-specs &rest options)
  "Define a web form with name, field-specs and options"
  `(ensure-web-form-definition ',name ',field-specs ',options))

;;; instanciating web forms

(defparameter *web-form-id* 0)

(defmethod instanciate-web-form-internal ((web-form-field-definition web-form-field-definition))
  (make-instance 'web-form-field 
                 :definition web-form-field-definition))

(defmethod instanciate-web-form-internal ((web-form-group-definition web-form-group-definition))
  (let ((web-form-group (make-instance 'web-form-group
                                       :definition web-form-group-definition)))
    (setf (get-fields web-form-group) 
          (mapcar #'instanciate-web-form-internal (get-fields web-form-group-definition))) 
    web-form-group))

(defmethod instanciate-web-form-internal ((web-form-definition web-form-definition))
  (let ((web-form (make-instance 'web-form 
                                 :id (incf *web-form-id*)
                                 :definition web-form-definition)))
    (setf (get-fields web-form) 
          (mapcar #'instanciate-web-form-internal (get-fields web-form-definition))) 
    web-form))

(defun instanciate-web-form (name)
  "Create a new web form instance from the web form definition named by name"
  (let ((web-form-definition (get-web-form-definition name)))
    (when web-form-definition
      (instanciate-web-form-internal web-form-definition))))

;;; accessors

(defmethod get-value-fields ((web-form-group web-form-group))
  (apply #'append (mapcar #'get-value-fields (get-fields web-form-group))))

(defmethod get-value-fields ((web-form-field web-form-field))
  (list web-form-field))

(defmethod get-value-fields ((web-form web-form))
  (apply #'append (mapcar #'get-value-fields (get-fields web-form))))

(defmethod get-field ((web-form-definition web-form-definition) name)
  (find name (get-fields web-form-definition) :key #'get-name))

(defmethod get-value-field ((web-form web-form) name)
  (find name (get-value-fields web-form) :key #'(lambda (field) (get-name (get-definition field)))))

(defgeneric get-value-field (web-form name)
  (:documentation "Return the value field with name in web-form"))

(defgeneric field-value (web-form name)
  (:documentation "Return the value of a field with name in web-form"))

(defmethod field-value ((web-form web-form) name)
  (get-value (get-value-field web-form name)))

(defmethod (setf field-value) (value (web-form web-form) name)
  (setf (get-value (get-value-field web-form name)) value))

(defgeneric get-title (object)
  (:documentation "Get the title of object"))

(defmethod get-title ((web-form web-form))
  (let ((definition (get-definition web-form)))
    (get-title definition)))

(defmethod (setf get-title) (value (web-form web-form))
  (let ((definition (get-definition web-form)))
    (setf (get-title definition) value)))

(defmethod get-submit-function ((web-form web-form))
  (get-submit-function (get-definition web-form)))

(defgeneric get-submit-text (form)
  (:documentation "Access the submit button text of form"))

(defmethod get-submit-text ((web-form web-form))
  (with-slots (submit-text) web-form
    (or submit-text 
        (get-submit-text (get-definition web-form)))))

(defmethod (setf get-submit-text) (value (web-form web-form))
  (with-slots (submit-text) web-form
    (setf submit-text value)))

(defmethod get-label ((web-form-field web-form-field))
  (let ((definition (get-definition web-form-field)))
    (or (get-label definition)
        (string-capitalize (get-name definition)))))

(defmethod get-type ((web-form-field web-form-field))
  (get-type (get-definition web-form-field)))

(defmethod get-name ((web-form-field web-form-field))
  (get-name (get-definition web-form-field)))

(defmethod get-comments ((web-form-field web-form-field))
  (let ((definition (get-definition web-form-field)))
    (get-comments definition)))

(defmethod get-validator ((web-form-field web-form-field))
  (get-validator (get-definition web-form-field)))

(defmethod get-formatter ((web-form-field web-form-field))
  (get-formatter (get-definition web-form-field)))

(defmethod get-parser ((web-form-field web-form-field))
  (get-parser (get-definition web-form-field)))

(defmethod get-validator ((web-form web-form))
  (get-validator (get-definition web-form)))

(defmethod get-formatted-value ((web-form-field web-form-field))
  (let ((value (get-value web-form-field)))
    (if value
        (funcall (get-formatter web-form-field) value)
      "")))

(defmethod get-option ((web-form-field web-form-field) key)
  (let ((field-match (member key (get-options web-form-field))))
    (if field-match
        (second field-match)
      (let* ((definition (get-definition web-form-field))
             (definition-match (member key (get-options definition))))
        (second definition-match)))))

(defmethod (setf get-option) (value (web-form-field web-form-field) key)
  (let ((match (member key (get-options web-form-field))))
    (if match
        (setf (second match) value)
      (progn 
        (push value (get-options web-form-field))
        (push key (get-options web-form-field))))))

(defmethod get-option ((web-form-group web-form-group) key)
  (declare (ignore key))
  ;; no group options for now
  nil)

;;; the web form rendering framework

(defparameter *default-web-form-render-options* 
  (make-options '(:form-renderer standard-web-form-renderer
                  :element-renderer standard-web-form-element-renderer
                  :localizer identity
                  :render-arrows t
                  :span-arrow-class "arrow"
                  :form-class "form"
                  :div-choice-class "cr"
                  :div-intro-comments-class "intro"
                  :div-form-comments-class "comments"
                  :div-field-comment-class "field-comment"
                  :div-field-error-class "validation-error"
                  :span-field-error-class "validation-error"
                  :div-form-error-class "validation-errors"))
  "The default options for rendering a web form")

(defun make-web-form-render-options (key-value-list)
  "Make new web form render options by combining key-value-list with *default-web-form-render-options*"
  (make-options key-value-list *default-web-form-render-options*))

(defmethod get-actual-label ((web-form-field web-form-field) 
                             &optional (render-options *default-web-form-render-options*))
  (let ((localizer (get-option render-options :localizer)))
    (funcall localizer (get-label web-form-field))))

(defun render-form-intro (request-response intro render-options)
  (when intro
    (html-part (out request-response)
      (:div :class (get-option render-options :div-intro-comments-class)
       (str (funcall (get-option render-options :localizer) intro))))))

(defun render-form-comments (request-response comments form-remark render-options)
  (when comments
    (html-part (out request-response)
      (:div :class (get-option render-options :div-form-comments-class)
       (when form-remark
         (htm (str (funcall (get-option render-options :localizer) form-remark))))
       (:ul
        (dolist (comment comments)
          (htm (:li (str (funcall (get-option render-options :localizer) comment))))))))))
         
(defun render-form-error-messages (request-response error-messages render-options)
  (when error-messages
    (html-part (out request-response)
      (:div :class (get-option render-options :div-form-error-class)
       (when (get-option render-options :warning-img)
         (htm (:span :class (get-option render-options :div-warning-img-class)
               (:img :src (get-option render-options :warning-img)))))
       (:ul
        (dolist (error-message error-messages)
          (htm (:li (str (funcall (get-option render-options :localizer) error-message))))))))))

(defun render-error-messages (request-response error-messages render-options)
  (when error-messages
    (html-part (out request-response)
      (htm (:div :class (get-option render-options :div-field-error-class)
            (when (get-option render-options :render-arrows)
              (htm (:span :class (get-option render-options :span-arrow-class) (str "&uarr;"))))
            (dolist (error-message error-messages)
              (htm (:span :class (get-option render-options :span-field-error-class)
                    (str (funcall (get-option render-options :localizer) error-message))))))))))

(defun render-comments (request-response comments render-options)
  (html-part (out request-response)
    (cond ((and comments (consp comments))
           (dolist (comment comments)
             (htm (:span :class (get-option render-options :div-field-comment-class) 
                   (str (funcall (get-option render-options :localizer) comment))))))
          (comments (htm (str (funcall (get-option render-options :localizer) comments)))))))

(defun standard-web-form-renderer (request-response web-form render-options)
  "Standard, builtin renderer for form using options"
  (html-part (out request-response)
    (:form :action (dynamic-url request-response (get-submit-function web-form)) :method "post"
     :id (get-option render-options :form-id) :class (get-option render-options :form-class)
     (let ((title (get-title web-form)))
       (when title
         (htm (:div :class "title" (str (funcall (get-option render-options :localizer) title))))))
     (render-form-intro request-response (get-intro (get-definition web-form)) render-options)
     (render-form-error-messages request-response (get-error-messages web-form) render-options)
     (dolist (field (get-fields web-form))
       (funcall (or (get-option field :renderer)
                    (get-option render-options :element-renderer))
                request-response field render-options))
     (generate-hidden-field request-response "form-id" (get-id web-form))
     (render-form-comments request-response 
                           (get-comments (get-definition web-form)) 
                           (get-form-remark (get-definition web-form)) 
                           render-options)
     (:div :class "buttons"
      (:input :type "submit" :value (get-submit-text web-form))))))

(defgeneric standard-web-form-element-renderer (request-response componentweb-form-field render-options)
  (:documentation "Standard, builtin renderer for a component in a form using options"))

(defmethod standard-web-form-element-renderer (request-response (web-form-group web-form-group) render-options)
  (let ((label (get-label (get-definition web-form-group))))
    (html-part (out request-response)
      (:fieldset
       (when label (htm (:legend (str (funcall (get-option render-options :localizer) label)))))
       (dolist (field (get-fields web-form-group))
         (funcall (or (get-option field :renderer)
                      (get-option render-options :element-renderer))
                  request-response field render-options))))))

(defmethod standard-web-form-element-renderer (request-response (web-form-field web-form-field) render-options)
  (let ((name (string-downcase (get-name web-form-field)))
        (field-type (get-type web-form-field)))
    (cond ((eql :hidden field-type)
           (generate-hidden-field request-response name (get-formatted-value web-form-field)))
          ((member field-type (list :static-text :text :password :text-area))
           (render-text-element request-response web-form-field render-options field-type))
          ((eql :choice field-type)
           (render-choice-element request-response web-form-field render-options))
          (t (if (fboundp field-type)
                 (funcall (symbol-function field-type) request-response web-form-field)
               (error "Unknown web form field type ~a" field-type))))
    (render-error-messages request-response (get-error-messages web-form-field) render-options)))

(defun render-required-indication (request-response web-form-field)
  (multiple-value-bind (value error)
      (funcall (get-validator (get-definition web-form-field)) nil)
    (declare (ignore value))
    (when error (html-part (out request-response)
                  (htm (:span :class "required" "*"))))))

(defun render-text-element (request-response web-form-field render-options field-type)
  (let ((name (string-downcase (get-name web-form-field))))
    (html-part (out request-response)
      (htm (:div            
            (case field-type
              (:static-text
               (htm (:label :for name (str (get-actual-label web-form-field render-options)) ":"))
               (generate-text-field request-response name (get-formatted-value web-form-field) 
                                    :size (or (get-option web-form-field :size) 32) :readonly t)
               (render-comments request-response (get-comments web-form-field) render-options))
              (:text
               (htm (:label :for name 
                     (render-required-indication request-response web-form-field)
                     (str (get-actual-label web-form-field render-options)) ":"))
               (generate-text-field request-response name (get-formatted-value web-form-field)
                                    :size (or (get-option web-form-field :size) 32)
                                    :maxlength (get-option web-form-field :maxlength)
                                    :readonly (get-option web-form-field :readonly))
               (render-comments request-response (get-comments web-form-field) render-options))
              (:password
               (htm (:label :for name 
                     (render-required-indication request-response web-form-field)
                     (str (get-actual-label web-form-field render-options)) ":"))
               (generate-password-field request-response name (get-formatted-value web-form-field)
                                        :size (or (get-option web-form-field :size) 32)
                                        :maxlength (get-option web-form-field :maxlength)
                                        :readonly (get-option web-form-field :readonly))
               (render-comments request-response (get-comments web-form-field) render-options))
              (:text-area
               (htm (:label :for name 
                     (render-required-indication request-response web-form-field)
                     (str (get-actual-label web-form-field render-options)) ":"))
               (generate-text-area request-response name (get-formatted-value web-form-field)
                                   :rows (get-option web-form-field :rows)
                                   :cols (get-option web-form-field :cols)
                                   :readonly (get-option web-form-field :readonly))
               (render-comments request-response (get-comments web-form-field) render-options))))))))

(defun render-choice-element (request-response web-form-field render-options)
  (let* ((name (string-downcase (get-name web-form-field)))
         (raw-value (get-value web-form-field))
         (values-option (get-option web-form-field :values))
         (values-list (resolve-static-value values-option))
         (selection-option (or (get-option web-form-field :selection) :single))
         (style-option (get-option web-form-field :style))
         (formatter (get-formatter web-form-field)))
    (html-part (out request-response)
      (cond ((eq values-option :boolean)
             (htm (:div :class "chk"
                   (:label :for name  
                    (generate-checkbox request-response name raw-value)
                    (str (get-actual-label web-form-field render-options)))
                   (render-required-indication request-response web-form-field)
                   (render-comments request-response (get-comments web-form-field) render-options))))
            ((and (eq selection-option :multiple) (eq style-option :buttons))
             (htm (:div :class (get-option render-options :div-choice-class)
                   (:span :class "cr" (str (get-actual-label web-form-field render-options)) ":")
                   (dolist (value values-list)
                     (htm (:label :for name 
                           (render-required-indication request-response web-form-field)
                           (generate-checkbox request-response name (member value raw-value :test #'equal) 
                                              :true-value value)
                           (str (funcall formatter value)))))
                   (render-comments request-response 
                                    (get-comments web-form-field) render-options))))
            ((and (eq selection-option :single) (eq style-option :buttons))
             (htm 
              (:div :class (get-option render-options :div-choice-class)
               (:span :class "cr" 
                (render-required-indication request-response web-form-field)
                (str (get-actual-label web-form-field render-options)) ":")
               (dolist (value values-list)
                 (htm (:label :for name 
                       (generate-radiobutton request-response name (equal value raw-value) 
                                             :true-value value)
                       (str (funcall formatter value)))))
               (render-comments request-response (get-comments web-form-field) render-options))))
            ((eq style-option :list)
             (htm (:div
                   (:label :for name 
                    (render-required-indication request-response web-form-field)
                    (str (get-actual-label web-form-field render-options)) ":")
                   (generate-select request-response name values-list raw-value
                                    :multiple (eq selection-option :multiple)
                                    :size (get-option web-form-field :size)
                                    :labels (mapcar formatter values-list)
                                    :test #'equal)
                   (render-comments request-response 
                                    (get-comments web-form-field) render-options))))))))

(defgeneric render (object request-response &optional render-options)
  (:documentation "Render object as HTML on request-response using render-options"))

(defmethod render ((web-form web-form) request-response &optional (render-options *default-web-form-render-options*))
  (let ((renderer (get-option render-options :form-renderer)))
    (funcall renderer request-response web-form render-options)))

;;; web form validation framework (based on constraints)

(defgeneric validate (component)
  (:documentation "Validate component by running all validators"))

(defmethod validate ((web-form web-form))
  (let* ((field-results (mapcar #'validate (get-value-fields web-form)))
         (fields-invalid-p (some #'null field-results))
         (validator (get-validator web-form))
         errors)
    (multiple-value-call #'(lambda (ok &optional error-messages)
                             (unless ok
                               (setf errors (if (consp error-messages)
                                                error-messages
                                              (list error-messages)))))
      (funcall validator web-form))
    (when fields-invalid-p
      (push :some-fields-invalid errors))
    (setf (get-error-messages web-form) errors)
    (null errors)))

(defmethod validate ((web-form-field web-form-field))
  (let ((validator (get-validator web-form-field)))
    (multiple-value-bind (ok error-messages)
        (call-validator validator (get-value web-form-field))
      (unless ok
        (setf (get-error-messages web-form-field) error-messages))
      ok)))

;;; web form life cycle management

(defmethod reset-values ((web-form web-form))
  (setf (get-error-messages web-form) nil)
  (dolist (field (get-value-fields web-form))
    (setf (get-value field) nil
          (get-error-messages field) nil)))

(defun reconstruct-web-form (request-response web-form-name)
  "Reconstruct a web form named web-form-name from request-response"
  ;; always a new form for now, later cache and reuse from session with form-id check
  (let ((web-form (instanciate-web-form web-form-name)))
    (dolist (field (get-value-fields web-form))
      (let ((parameter-values (get-request-parameter-values request-response (get-name field)))
            (parser (get-parser (get-definition field)))
            (is-boolean (eql (get-option field :values) :boolean))
            (multi-valued-p (eq :multiple (get-option field :selection)))
            parsed-value)
        (cond ((and (null parameter-values) (not is-boolean)) 
               (setf parsed-value nil)) ;; don't bother parsing nil
              ((and (not multi-valued-p)
                    (= 1 (length parameter-values))) 
               (setf parsed-value (funcall parser (first parameter-values))))
              (multi-valued-p 
               (setf parsed-value (mapcar parser parameter-values)))
              (is-boolean
               (setf parsed-value (funcall parser nil))) ;; value is absent here
              (t
               (error "Did not expect multiple values")))
        ;; what to do if parse failed (nil for now)
        (setf (get-value field) parsed-value)))
    web-form))

;;; support/util

(defun copy-slots-form->object (form object slots)
  "Copy the named slots from a form to an object"
  (dolist (slot slots)
    (cond ((symbolp slot)
           (setf (slot-value object slot) (field-value form slot)))
          ((consp slot)
           (setf (slot-value object (cdr slot)) (field-value form (car slot))))
          (t 
           (error "unknown slot spec ~s" slot))))) 

(defun copy-slots-object->form (object form slots)
  "Copy the named slots from an object to a form"
  (dolist (slot slots)
    (cond ((symbolp slot)
           (setf (field-value form slot) (slot-value object slot)))
          ((consp slot)
           (setf (field-value form (cdr slot)) (slot-value object (car slot))))
          (t
           (error "unknown slot spec ~s" slot)))))

(defun parse-boolean (string)
  "Parse string as a Common Lisp boolean"
  (when string
    (string-equal string "t")))

(defun parse-symbol (string &optional (package *package*))
  "Parse string as a Common Lisp symbol in package"
  (when string
    (find-symbol (string-upcase string) package)))

(defun parse-keyword (string)
  "Parse string as a Common Lisp keyword in package"
  (when string
    (find-symbol (string-upcase string) :keyword)))

;;;; eof