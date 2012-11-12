;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: worldclock.lisp,v 1.3 2005/10/08 07:27:38 sven Exp $
;;;;
;;;; WorldClock allows you to see the time and date in and compare different timezones.
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :worldclock
  (:index 'worldclock-start)
  (:static-root "static/")
  (:unsecure t))

(defclass worldclock ()
  ((id :accessor get-id :initarg :id)
   (name :accessor get-name :initarg :name)
   (description :accessor get-description :initarg :description)
   (timezone :accessor get-timezone :initarg :timezone)
   (longitude :accessor get-longitude :initarg :longitude)
   (latitude :accessor get-latitude :initarg :latitude)
   (dst-start :accessor get-dst-start :initarg :dst-start)
   (dst-stop :accessor get-dst-stop :initarg :dst-stop)
   (dst-delta :accessor get-dst-delta :initarg :dst-delta :initform (* 60 60))))

(defmethod get-sunrise ((worldclock worldclock))
  (with-slots (longitude latitude timezone)
      worldclock
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time (get-universal-time) (- timezone))
      (declare (ignore second minute hour))
    (sunset:time-of-phenomenon month date year :sunrise latitude longitude timezone))))

(defmethod get-sunset ((worldclock worldclock))
  (with-slots (longitude latitude timezone)
      worldclock
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time (get-universal-time) (- timezone))
      (declare (ignore second minute hour))
      (sunset:time-of-phenomenon month date year :sunset latitude longitude timezone))))

(defvar *worldclocks*
  (list (make-instance 'worldclock 
                       :id 0
                       :name "UTC / GMT"
                       :timezone 0
                       :latitude (+ 51 (/ 40 60.0))
                       :longitude 0
                       :dst-start nil
                       :dst-stop nil)
        (make-instance 'worldclock 
                       :id 1
                       :name "Brussels / Belgium"
                       :timezone +1
                       :latitude (+ 50 (/ 51 60.0))
                       :longitude (+ 4 (/ 21 60.0))
                       :dst-start "20050327T020000"
                       :dst-stop "20051030T030000")
        (make-instance 'worldclock 
                       :id 2
                       :name "New York / USA"
                       :timezone -5
                       :latitude (+ 40 (/ 44 60.0))
                       :longitude (- (+ 73 (/ 55 60.0)))
                       :dst-start "20050403T020000"
                       :dst-stop "20051030T020000")
        (make-instance 'worldclock 
                       :id 3
                       :name "Sydney / Australia"
                       :timezone +10
                       :latitude (- (+ 33 (/ 55 60.0)))
                       :longitude (+ 151 (/ 17 60.0))
                       :dst-start "20050403T020000"
                       :dst-stop "20051030T020000")))

(defun worldclock-start (request-response)
  (html-page (out request-response)
    (:html 
     (:head 
      (:title "WorldClock") 
      (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
     (:body 
      (:h1 "WorldClock")
      (:div :class "NX_panel"
       (:span :class "NX_title" "Now")
       (:div :class "NX_border"
        (:p (fmt "It is now ~a UTC/GMT" (util:format-universal-time (get-universal-time) :timezone 0)))
        (:p (fmt "It is now ~a local time" (util:format-universal-time (get-universal-time))))))))))

;;;; eof