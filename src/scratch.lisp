;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: scratch.lisp,v 1.1 2004/02/27 12:39:11 sven Exp $
;;;;
;;;; Scratch area
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(defvar *do-addition-debug-mode* t)

(defun do-addition (x y)
  (loop
   :with current-condition
   :do
   (restart-case 
       (handler-bind ((condition #'(lambda (condition)
                                     (if *do-addition-debug-mode*
                                         (progn
                                           (setf current-condition condition)
                                           (error condition))
                                       (invoke-restart 'accept condition)))))
         (let ((result (+ x y)))
           (format t "do-addition ~d+~d=~d~%" x y result)
           (return t)))
     (retry ()
            :report (lambda (s) (format s "Retry adding ~s and ~s" x y)))
     (accept (condition)
             :interactive (lambda () (list current-condition))
             :report "Continue by returning an error"
             (format t "do-addition encountered error: ~a~%" condition)
             (return nil)))))
   
;;;; eof