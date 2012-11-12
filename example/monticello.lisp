(in-package :cl-user)

(defvar *repository-location* #p"~/tmp/monticello/")

(defvar *authenticated-users* '(("svc" . "villerspark")))

(defun list-mcz-repository ()
  (mapcar (lambda (p)
            (concatenate 'string (pathname-name p) ".mcz"))
          (directory (merge-pathnames "*.mcz" *repository-location*))))

(defun get-mcz-entry (name)
  (probe-file (merge-pathnames name *repository-location*)))

(defun is-valid-mcz-name (name)
  (and (stringp name)
       (< 0 (length name))
       (string-equal ".mcz" name :start2 (position #\. name :from-end t))
       (null (find #\/ name))))

(defun new-mcz-entry (name data)
  (when (is-valid-mcz-name name)
    (let ((pathname (merge-pathnames name *repository-location*)))
      (unless (probe-file pathname)
        (with-open-file (out 
                         pathname 
                         :direction :output 
                         :if-does-not-exist :create 
                         :element-type '(unsigned-byte 8))
          (write-sequence data out)
          t)))))

(defun s-http-server-monticello-handler (s-http-server handler http-request stream)
  "The handler for the MONTICELLO protocol"
  (declare (ignore handler s-http-server))
  (let* ((method (s-http-server:get-method http-request))
         (path (s-http-server:get-path http-request)))
    (cond ((and (eql method :get) (equal path "/"))
           (values t 200
                   (s-http-server:standard-http-html-message-response 
                    http-request stream "Monticello Repository" 
                    (with-output-to-string (out)
                      (format out "<ul>")
                      (loop :for name :in (list-mcz-repository) :do
                            (format out "<li><a href=\"~a\">~a</a></li>" name name))
                      (format out "</ul>")))))
          ((eql method :get)
           (let ((pathname (get-mcz-entry (subseq path 1))))
             (if pathname
                 (s-http-server::host-static-resource http-request stream pathname)
               (values t 404 (s-http-server::standard-http-html-error-response 
                              http-request stream 404 "Resource Not Found" path)))))
          ((eql method :put)
           (if (let* ((content-length (s-http-server:request-header-value http-request "Content-Length"))
                      (buffer (when content-length
                                (make-string (s-utils:parse-integer-safely content-length)))))
                 (when buffer
                   (read-sequence buffer stream)
                   (new-mcz-entry (subseq path 1) buffer)))
               (values t 201 (s-http-server::standard-http-html-error-response 
                              http-request stream 201 "Created" path))
             (values t 400 (s-http-server::standard-http-html-error-response 
                            http-request stream 400 "Bad Request" path))))
          (t
           (values t 406 (s-http-server::standard-http-html-error-response 
                          http-request stream 406 "Method Not Acceptable" path))))))

(defun make-s-http-monticello-server (&key (port 1701))
  "Make an actual S-HTTP-SERVER with the MONTICELLO handler installed"
  (let ((s-http-server (s-http-server:make-s-http-server :port port :name "monticello-server")))
    (push (list (s-http-server:wrap-with-basic-authentication 's-http-server-monticello-handler
                                                              :authenticator *authenticated-users*
                                                              :realm "Monticello Repository")
                "/")
          (s-http-server:get-contexts s-http-server))
    s-http-server))

