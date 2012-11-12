;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: crypto.lisp,v 1.5 2004/09/17 09:12:31 sven Exp $
;;;;
;;;; Some cryptographic and security related code used by KPAX
;;;;
;;;; Copyright (C) 2004-2007 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax)

(export '(secure-random secure-random-bytes hmac-sha1 hmac-md5))

;; some random stuff for the paranoid ;-)

(defparameter *random-device* "/dev/urandom")

(defun add-entropy-to-dev-random ()
  "Add some CL based entropy to kernel random device"
  (if (and (probe-file *random-device*)
           (ignore-errors (with-open-file (out *random-device* :direction :output :if-exists :append) 
                            (declare (ignore out))
                            t)))
      (with-open-file (out *random-device* :direction :output :if-exists :append)
        (let ((*standard-output* out))
          (print (get-internal-run-time))
          (room t)
          (print (get-internal-real-time)))
        t)
    (warn "~a does not exist, or isn't writeable, did not add entropy" *random-device*)))

(defun get-secure-random-seed (n)
  "Read n 32-bit integers from the kernel random device and return them as a vector"
  (let ((sequence (make-sequence 'vector n)))
    (with-open-file (in *random-device* 
                        :direction :input 
                        :if-does-not-exist nil
                        :element-type '(unsigned-byte 8))
      (if (null in)
          (let ((random-state (mt:mt-make-random-state-integer (get-universal-time))))
            (warn "~a not exist, fallback to unsecure pseudo random" *random-device*)
            (dotimes (i n sequence)
              (setf (svref sequence i) (mt:mt-random (expt 2 32) random-state))))
        (dotimes (i n sequence)
          (setf (svref sequence i) (+ (read-byte in)
                                      (ash (read-byte in) 8)
                                      (ash (read-byte in) 16)
                                      (ash (read-byte in) 32))))))))

(defvar *kpax-random-state* (progn
                              (add-entropy-to-dev-random)
                              (mt:make-mt-random-state (get-secure-random-seed 624)))
  "Secure random state for MT random generator seeded from /dev/random")

(defun secure-random (limit)
  "Like cl:random, return a random number (<= 0 x limit) [same type as limit], using our secure MT random generator"
  (mt:mt-random limit *kpax-random-state*))

(defun secure-random-bytes (n)
  "Return a byte-vector of size n filled with secure random bytes"
  (let ((bytes (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n bytes)
      (setf (aref bytes i) (secure-random 255)))))

;; Easy entry points into HMACs

(defun hmac-sha1 (data-string key-string)
  "Compute an RFC 2104 HMAC-SHA1 digest on data-string using key-string"
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array key-string) :sha1)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array data-string))
    (ironclad:hmac-digest hmac)))

(defun hmac-md5 (data-string key-string)
  "Compute an RFC 2104 HMAC-MD5 digest on data-string using key-string"
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array key-string) :md5)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array data-string))
    (ironclad:hmac-digest hmac)))

;;;; eof
