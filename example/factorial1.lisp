;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: factorial1.lisp,v 1.8 2004/09/17 08:08:48 sven Exp $
;;;;
;;;; A basic Factorial example: how to ask for input, check that input, 
;;;; process a form commit, show the results and show errors
;;;;
;;;; Copyright (C) 2004 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

;;; the web app definition

(defwebapp :factorial1
  (:index 'factorial1-start)
  (:static-root "static/")
  (:web-functions '(compute-factorial1 factorial1-result factorial1-error))
  (:unsecure t))

;;; a superfast factorial implementation from CLOCC CLLIB

(declaim (ftype (function (integer integer) (values integer))
                product-from-to binomial))

(defun product-from-to (aa bb)
  "Compute the product of integers from AA (EXclusive) to BB (INclusive)."
  (declare (integer aa bb))
  (when (> aa bb)
    (error "~s (~:d ~:d): the first argument must be smaller"
           'product-from-to aa bb))
  (when (minusp (* bb aa))
    (return-from product-from-to 0))
  ;; this algorithm insures that we multiply bignums
  ;; of approximately the same size
  ;; we use `labels' since some compilers optimize `labels' better than
  ;; plain recursion and because this avoids doing the above checks in cycle
  (labels ((pft (aa bb)
		(case (- bb aa)
		  (0 1)
		  (1 bb)
		  (2 (* (- bb 1) bb))
		  (3 (* (- bb 2) (- bb 1) bb))
		  (4 (* (- bb 3) (- bb 2) (- bb 1) bb))
		  (t (let ((mm (ash (+ aa bb) -1)))
		       (* (pft aa mm) (pft mm bb)))))))
    (pft aa bb)))

(declaim (ftype (function (integer) (values integer)) fac))

(defun fac (nn)
  "Compute the factorial: n! = n * (n-1) * (n-2) * ..."
  (declare (integer nn))
  (if (zerop nn)
      1
    (product-from-to 1 nn)))

;;; the web app main and index page

(defun factorial1-start (request-response)
  (html-page (out request-response)
    (:html 
     (:head
      (:title "Factorial1!")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:div :class "NX_panel"
       (:span :class "NX_title" "Factorial1!")
       (:div :class "NX_border"
        (:p "This web application computes the factorial of a positive integer.")
        "Factorial being defined recursively as follows:"
        (:ul
         (:li "the factorial of 0 is 1")
         (:li "the factorial of N is N times the factorial of N-1"))
        (:form :class "NX_form" :action (dynamic-url request-response 'compute-factorial1) :method :post
         (:input :type "text" :name "number" :value 0)
         (:input :type "submit" :value "GO!"))))))))
    
;;; the web action to do the computation

(defun compute-factorial1 (request-response)
  (let* ((number-string (get-request-parameter-value request-response "number"))
	 (number (when number-string 
                   (s-utils:parse-integer-safely (string-trim (list #\Space) number-string)))))
    (cond ((not number)
	   (factorial1-error request-response "Cannot convert ~a to an integer" number-string))
	  ((< number 0)
	   (factorial1-error request-response "Cannot compute factorial of ~d, number must be positive" number))
	  ((> number 12399)
	   (factorial1-error request-response "Cannot compute factorial of ~d, number to large" number))
	  (t
	   (let* ((start (get-internal-run-time))
		  (fac (fac number))
		  (stop (get-internal-run-time))
                  (runtime (float (/ (- stop start) internal-time-units-per-second))))
	     (setf (get-attribute request-response :number) number
		   (get-attribute request-response :factorial) fac
		   (get-attribute request-response :time) runtime)
	     (factorial1-result request-response))))))

;;; a helper function to print long numbers over multiple lines

(defun long-number-to-string (n &key (break-char #\newline) (line-length 64))
  (with-output-to-string (stream)
    (let ((long-number (prin1-to-string n)))
      (do ((i 0 (+ i line-length)))
	  ((>= i (length long-number)))
	(princ (subseq long-number i (min (length long-number) (+ i line-length))) stream)
	(princ break-char stream)))))

;;; the web page to show the result

(defun factorial1-result (request-response)
  (let ((number (get-attribute request-response :number))
        (factorial (get-attribute request-response :factorial))
        (time (get-attribute request-response :time)))
    (html-page (out request-response)
      (:html 
       (:head 
        (:title "Factorial1 Result")
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
       (:body 
        (:div :class "NX_panel"
         (:span :class "NX_title" "Factorial1 Result")
         (:div :class "NX_border"
          (:p (fmt "The factorial of ~d is" number))
          (:pre (str (long-number-to-string factorial)))
          (:p (fmt "Computation took ~dms." time))
          (:div :class "NX_button_group" 
           (:a :class "NX_button" :href (dynamic-url request-response 'factorial1-start)
            "Compute another factorial")))))))))

;;; the page to show errors

(defun factorial1-error (request-response format-string &rest format-args)
  (html-page (out request-response)
    (:html 
     (:head
      (:title "Factorial1 Error")
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:div :class "NX_panel"
       (:span :class "NX_title" "Factorial1 Error")
       (:div :class "NX_border"
        (:p (str (apply #'format nil format-string format-args)))
        (:div :class "NX_button_group" 
         (:a :class "NX_button" :href (dynamic-url request-response 'factorial1-start)
          "Back"))))))))

;;;; eof
