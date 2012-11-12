;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: data-set-view.lisp,v 1.11 2004/11/30 16:26:26 nicky Exp $
;;;;
;;;; A simple framework to view (and browse) a list of records as a table
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export '(data-set-length data-set-elt make-data-set-view render view-position data-set-position get-data-set
          *default-data-set-render-options*))

(defgeneric data-set-length (data-set)
  (:documentation "Return the number of elements in the data-set")
  (:method ((data-set sequence))
   (length data-set)))

(defgeneric data-set-elt (data-set position)
  (:documentation "Return the element at position in data-set (zero-based offset)")
  (:method ((data-set sequence) position)
   (elt data-set position)))

(defclass data-set-column ()
  ((id :accessor get-id :initarg :id)
   (data-set-view :accessor get-data-set-view :initarg :data-set-view :initform nil)
   (label :accessor get-label :initarg :label :initform nil)
   (key :accessor get-key :initarg :key :initform #'identity)
   (formatter :accessor get-formatter :initarg :formatter :initform #'princ-to-string)
   (sortable :accessor get-sortable :initarg :sortable :initform nil)
   (sort-predicate :accessor get-sort-predicate :initarg :sort-predicate :initform #'<)
   (renderer :accessor get-renderer :initarg :renderer :initform 'standard-data-renderer)))

(defclass data-set-view ()
  ((data-set :accessor get-data-set :initarg :data-set :initform nil)
   (columns :accessor get-columns :initarg :columns :initform nil)
   (page-size :accessor get-page-size :initarg :page-size :initform 20)
   (page :accessor get-page :initarg :page :initform 0)
   (sortable :accessor get-sortable :initarg :sortable :initform nil)
   (action :accessor get-action :initarg :action :initform nil)
   (permutations :accessor get-permutations :initform nil)
   (sort-order :accessor get-sort-order :initarg :sort-order :initform t)
   (sort-column :accessor get-sort-column :initarg :sort-column :initform nil)
   (renderer :accessor get-renderer :initarg :renderer :initform 'standard-data-set-view-renderer)
   (localizer :accessor get-localizer :initarg :localizer :initform #'identity)))

(defgeneric get-data-set (data-set-view)
  (:documentation "Access the underlying data-set in data-set-view"))

(defun standard-data-renderer (string request-response)
  (html-part (out request-response)
    (esc string)))

(defun ensure-permutations (data-set-view)
  (with-slots (permutations data-set)
      data-set-view
    (when (null permutations)
      (let* ((length (data-set-length data-set))
             (vector (make-array length)))
        (loop :for i :below length :do (setf (svref vector i) i))
        (setf permutations vector)))))

(defmethod (setf get-data-set) :after (value (data-set-view data-set-view))
  (setf (get-permutations data-set-view) nil))

(defun sort-data-set-view (data-set-view sort-request)
  (when sort-request
    (setf sort-request (intern sort-request :keyword))
    (when (member sort-request (remove-if-not #'get-sortable (get-columns data-set-view)) :key #'get-id)
      (if (eq sort-request (get-sort-column data-set-view))
          (setf (get-sort-order data-set-view) (not (get-sort-order data-set-view)))
        (setf (get-sort-order data-set-view) t
              (get-sort-column data-set-view) sort-request))
      (let* ((sort-column (find sort-request (get-columns data-set-view) :key #'get-id))
             (data-set (get-data-set data-set-view))
             (key (get-key sort-column)))
        (setf (get-permutations data-set-view) (sort (get-permutations data-set-view)
                                                     (if (get-sort-order data-set-view)
                                                         (get-sort-predicate sort-column) 
                                                       #'(lambda (x y) 
                                                           (not (funcall (get-sort-predicate sort-column) x y))))
                                                     :key #'(lambda (position)
                                                              (funcall key
                                                                       (data-set-elt data-set position)))))))))

(defmethod get-actual-label ((column data-set-column) &optional render-options)
  (declare (ignore render-options))
  (let ((localizer (get-localizer (get-data-set-view column))))
    (funcall localizer (or (get-label column) 
                           (string-capitalize (symbol-name (get-id column)))))))

(defun standard-data-set-view-renderer (data-set-view request-response render-options)
  (ensure-permutations data-set-view)
  (with-slots (page-size permutations data-set sortable columns action) 
      data-set-view
    (let* ((page (or (s-utils:parse-integer-safely (get-request-parameter-value request-response "page"))
                     (get-page data-set-view)))
           (length (length permutations))
           (last-page (if (zerop length) 0 (1- (ceiling length page-size))))
           (sort-request (get-request-parameter-value request-response "sort")))
      (sort-data-set-view data-set-view sort-request)
      (html-part (out request-response)
        (:table :width "100%" :class (get-option render-options :table-class)
         (:tr (:td :colspan (length columns) :class (get-option render-options :navigation-class)
               (:span :class (get-option render-options :class-navigation-arrows)
                (if (zerop page)
                    (htm (:span :class (get-option render-options :class-navigation-arrows-left)
                          (:span (esc "|<")) (:span (esc "<"))))
                  (htm
                   (:span :class (get-option render-options :class-navigation-arrows-left)
                    (:a :href (dynamic-url request-response action :page 0) (esc "|<"))
                    (:a :href (dynamic-url request-response action :page (1- page)) (esc "<")))))
                (if (eql page last-page)
                    (htm (:span :class (get-option render-options :class-navigation-arrows-right)
                          (:span (esc ">")) (:span (esc " >|"))))
                 (htm
                  (:span :class (get-option render-options :class-navigation-arrows-right)
                   (:a :href (dynamic-url request-response action :page (1+ page)) (esc ">"))
                   (:a :href (dynamic-url request-response action :page last-page) (esc ">|"))))))
               (loop :for other-page :from 0 :to last-page 
                     :do
                     (if (eql other-page page)
                         (htm (:span (str (1+ page))))
                       (htm (:a :href (dynamic-url request-response action :page other-page) (str (1+ other-page))))))))
         (:tr
          (dolist (column columns)
            (let ((label (get-actual-label column)))
              (if (and sortable (get-sortable column))
                  (htm
                   (:th (:a :href (dynamic-url request-response 
                                               action 
                                               :page page :sort (get-id column)) 
                         (esc label))))
                (htm
                 (:th (esc label)))))))
         (loop :for view-position :upfrom (* page page-size) :below (min length (* (1+ page) page-size))
               :do
               (let* ((data-set-position (svref permutations view-position))
                      (row (data-set-elt data-set data-set-position)))
                 (htm
                  (:tr :class (if (evenp view-position) 
                                  (get-option render-options :class-even-row)
                                (get-option render-options :class-odd-row))
                   (dolist (column columns)
                     (let* ((key (get-key column))
                            (value (case key
                                     (view-position view-position)
                                     (data-set-position data-set-position)
                                     (t (funcall key row))))
                            (formatted-value (funcall (get-formatter column) value)))
                       (htm (:td (funcall (get-renderer column) formatted-value request-response))))))))))))))

(defparameter *default-data-set-render-options* 
  (make-options '(:table-class "table"
                  :render-even-odd-rows nil
                  :div-even-row "even"
                  :div-odd-row "odd"))
  "The default options for rendering a data set view")

(defmethod render ((data-set-view data-set-view) request-response 
                   &optional (render-options *default-data-set-render-options*))
  (funcall (get-renderer data-set-view) data-set-view request-response render-options))

(defun make-data-set-column (column-spec)
  (destructuring-bind (id key &key label formatter sortable sort-predicate renderer) column-spec
    (let ((data-set-column (make-instance 'data-set-column
                                          :id id
                                          :key key)))
      (when label (setf (get-label data-set-column) label))
      (when formatter (setf (get-formatter data-set-column) formatter))
      (when sortable (setf (get-sortable data-set-column) sortable))
      (when sort-predicate (setf (get-sort-predicate data-set-column) sort-predicate))
      (when renderer (setf (get-renderer data-set-column) renderer))
      data-set-column)))

(defun make-data-set-view (data-set action columns &key (page-size 20) sortable sort-order sort-column renderer localizer)
  "Create a new data set view"
  (let ((data-set-view (make-instance 'data-set-view
                                      :data-set data-set
                                      :action action
                                      :page-size page-size))
        (columns (mapcar #'make-data-set-column columns)))
    (dolist (column columns) (setf (get-data-set-view column) data-set-view))
    (setf (get-columns data-set-view) columns)
    (when sortable (setf (get-sortable data-set-view) sortable))
    (when sort-order (setf (get-sort-order data-set-view) sort-order))
    (when sort-column (setf (get-sort-column data-set-view) sort-column))
    (when renderer (setf (get-renderer data-set-view) renderer))
    (when localizer (setf (get-localizer data-set-view) localizer))
    data-set-view))

;;;; eof