;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: calculator.lisp,v 1.9 2004/09/09 11:21:37 sven Exp $
;;;;
;;;; This example is a web interface to a classic four function calculator
;;;; where the calculator has a real independent model.
;;;; This example also demonstrates the use of a style sheet.
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

;;; the web app definition

(defwebapp :calculator
  (:index 'show-calculator)
  (:session-tracking-style :url)
  (:static-root "static/")
  (:request-tracking t)
  (:unsecure t))

;;; the calculator model

(defclass calculator ()
  ((operand1 :accessor operand1 :initform 0)
   (operand2 :accessor operand2 :initform 0)
   (display :accessor display :initform "0")
   (previous-operator :accessor previous-operator :initform '+)
   (operator-hit :accessor operator-hit :initform t)))

(defmethod clear ((calculator calculator))
  (setf (operand1 calculator) 0
	(operand2 calculator) 0
	(display calculator) "0"
	(previous-operator calculator) '+
	(operator-hit calculator) t))

(defmethod digit ((calculator calculator) digit)
  (setf (display calculator) (if (operator-hit calculator)
				 (format nil "~d" digit)
			       (format nil "~a~d" (display calculator) digit))
	(operator-hit calculator) nil))

(defmethod dot ((calculator calculator))
  (if (operator-hit calculator)
      (setf (display calculator) "0.")
    (unless (find #\. (display calculator))
	(setf (display calculator) (format nil "~a." (display calculator)))))
  (setf (operator-hit calculator) nil))

(defmethod update-display ((calculator calculator))
  (with-slots (operand1) calculator
    (setf (display calculator) (if (= operand1 (round operand1))
				   (format nil "~d" operand1)
				 (format nil "~,3f" operand1)))))

(defmethod operator ((calculator calculator) operator)
  (handler-bind ((error #'(lambda (c)
			    (declare (ignore c))
			    (clear calculator)
			    (setf (display calculator) "ERROR")
			    (return-from operator nil))))
    (if (eq (previous-operator calculator) '=)
	(setf (operand1 calculator) (read-from-string (display calculator)))
      (setf (operand2 calculator) (read-from-string (display calculator))
	    (operand1 calculator) (funcall (previous-operator calculator)
					   (operand1 calculator)
					   (operand2 calculator))))
    (setf (previous-operator calculator) operator
	  (operator-hit calculator) t)
    (update-display calculator)))

;;; the web app main page

(defparameter *id-and-layout* 
  '(("numpad" ((#\7 #\8 #\9) (#\4 #\5 #\6) (#\1 #\2 #\3) (#\0 #\. #\=))) 
    ("operations" ((#\+) (#\-) (#\*) (#\/) (#\C)))))

(defun show-calculator (request-response)
  (let ((calculator (get-attribute (get-session request-response) :calculator)))
    (when (null calculator)
      (setf calculator (make-instance 'calculator)
            (get-attribute (get-session request-response) :calculator) calculator))
    (html-page (out request-response)
      (:html 
       (:header 
        (:title "KPAX Calculator")
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :webapp "calculator.css")))
       (:body
        (:div :id "calculator"
         (:h1 (str (display calculator)))
         (loop :for (id layout) :in *id-and-layout* 
               :do
               (htm
                (:div :id id
                 (dolist (row layout)
                   (htm 
                    (:div :class "row"
                     (dolist (key row)
                       (htm 
                        (:a :href (dynamic-url request-response 'calculator-hit-key :key key)
                         (str key))))))))))))))))

;;; the function that processes key hits from the web page

(defun calculator-hit-key (request-response)
  (let* ((key-string (get-request-parameter-value request-response "key"))
	 (key (when (and key-string (plusp (length key-string))) (char key-string 0)))
	 (calculator (get-attribute (get-session request-response) :calculator)))
    (if (get-attribute request-response :inconsistent-request)
        (html-message request-response "Error" 
                      "Page already submitted (don't go backwards). <a href='~a'>OK</a>"
                      (dynamic-url request-response 'show-calculator))
      (progn
        (cond ((null calculator) nil) ;; fall through & show the calendar (which will create & set a calculator)
              ((null key-string) (setf (display calculator) "ERROR"))
              ((find key "0123456789") (digit calculator (read-from-string key-string)))
              ((find key "+-*/=") (operator calculator (intern key-string)))
              ((equal key #\.) (dot calculator))
              ((equal key #\C) (clear calculator))
              (t (setf (display calculator) "ERROR")))
        (show-calculator request-response)))))

;;;; eof
