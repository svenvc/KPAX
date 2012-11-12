;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: constraints.lisp,v 1.5 2004/08/06 13:12:28 nicky Exp $
;;;;
;;;; A simple contraints specification and validation framework
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(required optional boolean integer-range limited-string list-element list-elements all))

(defun builtin-validate-required (value)
  (if (null value)
      (values nil :required-field)
    t))

;(defun builtin-validate-optional (value)
;  t)

(defun builtin-validate-optional (value)
  (if (null value)
      t
    (values nil :optional)))

(defun builtin-validate-boolean (value)
  (if (or (null value) (eq value t))
      t
    (values nil :boolean-expected)))

(defun builtin-validate-integer-range (value from to)
  (if (integerp value)
      (cond ((< value from) (values nil :too-small))
            ((< to value) (values nil :too-large))
            (t t))
    (values nil :integer-expected)))

(defun builtin-validate-limited-string (value limit &optional (minimum 0))
  (if (stringp value)
      (cond ((> (length value) limit) (values nil :too-long))
            ((< (length value) minimum) (values nil :too-short))
            (t t))
    (values nil :string-expected)))

(defun resolve-static-value (spec)
  (if (symbolp spec)
      (cond ((fboundp spec) (funcall (symbol-function spec)))
            ((boundp spec) (symbol-value spec))
            (t (error "Cannot resolve ~a" spec)))
    spec))

(defun builtin-validate-list-element (value list)
  (setf list (resolve-static-value list))
  (if (member value list :test #'equal)
      t
    (values nil :not-in-list)))

(defun builtin-validate-list-elements (value list)
  (setf list (resolve-static-value list))
  (cond ((atom value) (values nil :not-a-list))
        ((some #'(lambda (x)
                   (not (member x list :test #'equal)))
               value)
         (values nil :some-not-in-list))
        (t t)))

(defun call-validator (validator arg)
  "Calls validator on arg, making sure the optional errors messages are always a list"
  (multiple-value-call #'(lambda (ok &optional error-messages)
                           (if ok
                               t
                             (values nil
                                     (if (consp error-messages)
                                         error-messages
                                       (list error-messages)))))
    (funcall validator arg)))
    
(defun make-validator (function &optional args)
  #'(lambda (value) (apply function value args)))

(defun make-validator-and (args)
  "Invoke validator functions in args in order; first failure is returned; returns t when all are t"
  #'(lambda (value)
      (loop :for term :in args
            :do (multiple-value-bind (ok error-messages)
                    (call-validator term value)
                  (when (not ok)
                    (return (values ok error-messages))))
            :finally (return t))))

(defun make-validator-or (args)
  "Invoke validator functions on all args in order; returns t when any one is t, otherwise return all errors" 
  #'(lambda (value)
      (let ((results (mapcar #'(lambda (term)
                                 (multiple-value-bind (ok error-messages)
                                     (call-validator term value)
                                   (list ok error-messages)))
                             args)))
        (if (some #'first results)
            t
          (if (equal (car (second (car results))) :optional) ;; optional isn't an error, it's a short-circuit!
              (values nil (apply #'append (mapcar #'second (cdr results))))
            (values nil (apply #'append (mapcar #'second results))))))))

(defun make-validator-all (args)
  "Invoke validator functions on all args in order; returns t when all are t, otherwise return all errors"
  #'(lambda (value)
      (let ((results (mapcar #'(lambda (term)
                                 (multiple-value-bind (ok error-messages)
                                     (call-validator term value)
                                   (list ok error-messages)))
                             args)))
        (if (every #'first results)
            t
          (values nil (apply #'append (mapcar #'second results)))))))

(defun resolve-validator-function (function-name)
  (if (fboundp function-name)
      (symbol-function function-name)
    (let ((builtin-function-name (intern (format nil "BUILTIN-VALIDATE-~a" function-name) 
                                         :kpax)))
      (if (fboundp builtin-function-name)
          (symbol-function builtin-function-name)
        (error "Cannot resolve ~a" function-name)))))

(defun compile-validator-expression (expr)
  (if (atom expr)
      (make-validator (resolve-validator-function expr))
    (destructuring-bind (operator &rest args)
        expr
      (case operator
        (and (make-validator-and (mapcar #'compile-validator-expression args)))
        (all (make-validator-all (mapcar #'compile-validator-expression args)))
        (or (make-validator-or (mapcar #'compile-validator-expression args)))
        (t (make-validator (resolve-validator-function operator) args))))))

;;;; eof