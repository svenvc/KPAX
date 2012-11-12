;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: log.lisp,v 1.6 2005/07/12 14:43:22 sven Exp $
;;;;
;;;; A log facility
;;;;
;;;; Copyright (C) 2004,2008 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export
 '(logm
   logger
   log-debug
   log-info
   log-warn
   log-error
   log-fatal
   stream-appender
   debug-stream-appender
   add-appender
   +log-flush-interval+))

(defconstant +log-level-off+ most-negative-fixnum)

(defconstant +log-level-debug+ 100
  "Log messages which are for debugging or tracing a program during development")

(defconstant +log-level-info+ 200
  "Log messages which are informational reporting progress or state changes")

(defconstant +log-level-warn+ 300
  "Log messages which are warnings, abnormal situations that are no errors")

(defconstant +log-level-error+ 400
  "Log messages for reporting errors that are somehow handled by the program")

(defconstant +log-level-fatal+ 500 
  "Log messages which are fatal, causing a program to stop working")

(defconstant +log-level-all+ most-positive-fixnum)

(defun log-level-to-name (level)
  (case level
    (100 :debug)
    (200 :info)
    (300 :warn)
    (400 :error)
    (500 :fatal)))

(defun name-to-log-level (name)
  (case name
    (:debug 100)
    (:info 200)
    (:warn 300)
    (:error 400)
    (:fatal 500)))

(defgeneric logm (object level format-string &rest args)
  (:documentation "Log a message at level defined by format-string and args to object"))

(defmacro log-debug (object format-string &rest args)
  "Log a message at the debug level using format-string and args"
  `(logm ,object +log-level-debug+ ,format-string ,@args))

(defmacro log-info (object format-string &rest args)
  "Log a message at the info level using format-string and args"
  `(logm ,object +log-level-info+ ,format-string ,@args))

(defmacro log-warn (object format-string &rest args)
  "Log a message at the warn level using format-string and args"
  `(logm ,object +log-level-warn+ ,format-string ,@args))

(defmacro log-error (object format-string &rest args)
  "Log a message at the error level using format-string and args"
  `(logm ,object +log-level-error+ ,format-string ,@args))

(defmacro log-fatal (object format-string &rest args)
  "Log a message at the fatal level using format-string and args"
  `(logm ,object +log-level-fatal+ ,format-string ,@args))

(defclass logger ()
  ((cutoff :accessor get-cutoff :initarg :cutoff :initform +log-level-info+)
   (logger-lock :accessor get-logger-lock :initform (s-sysdeps:make-process-lock "logger"))
   (appenders :initform nil)
   (last-flushed-timestamp :accessor get-last-flushed-timestamp :initform 0))
  (:documentation "A logger accepts log messages"))

(defclass appender ()
  ()
  (:documentation "An appender transfers log messages to a destination"))

(defgeneric add-appender (logger appender)
  (:documentation "Add an appender to a logger")
  (:method ((logger logger) (appender appender))
   (with-slots (appenders)
       logger
     (push appender appenders))))

(defclass stream-appender (appender)
  ((stream :accessor get-stream :initarg :stream :initform nil))
  (:documentation "A stream appender writes log messages to a stream"))

(defclass debug-stream-appender (stream-appender)
  ((enabled :accessor get-enabled :initform t))
  (:documentation "A stream appender for debugging including an 'enabled' property"))

(defmethod appendm ((stream-appender stream-appender) time level format-string args)
  (with-slots (stream)
        stream-appender
    (when stream
      (s-utils:format-iso-gmt-time time :stream stream)
      (write-char #\space stream)
      (write (log-level-to-name level) :stream stream :escape nil)
      (write-char #\space stream)
      (apply #'format stream format-string args)
      (terpri stream))))

(defmethod appendm ((stream-appender debug-stream-appender) time level format-string args)
  (with-slots (stream enabled)
      stream-appender
    (when (and stream enabled)
      (format stream
              ";; KPAX ~a ~a ~?~%"
              (s-utils:format-iso-gmt-time time) 
              (log-level-to-name level)
              format-string 
              args))))

(defmethod flush-log ((appender appender))
  t)

(defmethod flush-log ((stream-appender stream-appender))
  (finish-output (get-stream stream-appender)))

(defparameter +log-flush-interval+ 10 
  "Time in seconds after which logs are flushed")

(defmethod logm ((logger logger) level format-string &rest args)
  (with-slots (cutoff appenders last-flushed-timestamp)
      logger
    (unless (or (< level cutoff) (null appenders))
      (let ((now (get-universal-time))) 
        (s-sysdeps:with-process-lock 
            ((get-logger-lock logger))
          (dolist (appender appenders)
            (appendm appender now level format-string args))
          (when (< (+ last-flushed-timestamp +log-flush-interval+) now)
            (mapc #'flush-log appenders)
            (setf last-flushed-timestamp now)))))))

(defmethod logm ((logger null) level format-string &rest args)
  (declare (ignore logger level format-string args)))

;;;; eof
