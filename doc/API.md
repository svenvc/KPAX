# API for package S-HTTP-SERVER

S-HTTP-SERVER is a minial standalone Common Lisp HTTP Server

## *http-server-identification*   variable

Identification string sent as value of the 'Server' HTTP Response Header

Initial value: "S-HTTP-SERVER LispWorks 4.4.6"

## *http-server-port*   variable

Default port used when creating a new S-HTTP-SERVER

Initial value: "1701"

## (find-handler server http-request)   generic-function

Given http-request select a handler from server

## (get-contexts s-http-server)   generic-function

Get the current list of context bindings used by this S-HTTP-SERVER

## (get-headers http-request)   generic-function

Get the dotted alist (:keyword . 'value') of request headers of this HTTP request

## (get-http-version http-request)   generic-function

Get the HTTP version string of this HTTP request

## (get-keep-alive http-request)   generic-function

Is this a keep-alive request (either 1.0 or 1.1)

## (get-log-stream s-http-server)   generic-function

Get the current stream used by this S-HTTP-SERVER for logging, nil means no logging

## (setf (get-log-stream s-http-server) value)   generic-function

Set the stream this S-HTTP-SERVER uses for logging, nil means no logging

## (get-method http-request)   generic-function

Get the method (keyword :get :put :post :delete ..) of this HTTP request

## (get-name s-http-server)   generic-function

Get the current name of this S-HTTP-SERVER

## (setf (get-name s-http-server) value)   generic-function

Set the name of this S-HTTP-SERVER

## (get-path http-request)   generic-function

Get the path of this HTTP request

## (get-port s-http-server)   generic-function

Get the TCP port used by this S-HTTP-SERVER

## (get-server-process s-http-server)   generic-function

Get the current server process used by this S-HTTP-SERVER, nil if not running

## (get-uri http-request)   generic-function

Get the URI object of this HTTP request

## (handle-http-server-connection server connection-id client-socket-stream)   generic-function

Handle a new connection request in a new process

## http-request   class

The object representing an HTTP request as being handled by the S-HTTP-SERVER

Class precedence list: http-request standard-object t

Class init args: :keep-alive :headers :http-version :uri :method

## (logm server format-string &rest args)   generic-function

Log a formatted message

## (make-s-http-server &key (port *http-server-port*) (name s-http-server) (log-stream *standard-output*))   function

Create a new object representing an S-HTTP-SERVER

## (register-context-handler server context-prefix handler-function &key arguments at-end-p do-not-replace-p)   generic-function

Configure server so that every request starting with context-prefix is sent to handler-function

## s-http-server   class

The object representing a minimal standalone HTTP Server

Class precedence list: s-http-server standard-object t
Class init args: :contexts :log-stream :debug-mode :name :port

## (s-http-server-handler s-http-server handler http-request stream)   function

The builtin S-HTTP-SERVER testing/debugging handler returning a simple status/echo/snoop page

## (standard-http-html-error-response http-request stream code reason extra)   function

Generate and write a standard HTML error as HTTP Response using code, reason and extra

## (standard-http-html-message-response http-request stream title message &optional (status 200) (string ok))   function

Generate and write a standard HTML message as HTTP Response using title, message, status and string

## (standard-http-response-headers http-request &key (content-type text/plain) content-length)   function

Generate the standard headers alist given context-type and context-length, managing old-style Keep-Alive

## (start-server server)   generic-function

Start the server

## (static-resource-handler s-http-server handler http-request stream)   function

Host static resources from a document root

## (stop-server server)   generic-function

Stop the server

## (unregister-context-handler server context-prefix &key only-first-p only-last-p)   generic-function

Remove any configuration of server for context-prefix

## (write-http-response-headers headers stream)   function

Write the headers alist as HTTP Response Headers to stream

## (write-http-response-line string &optional (stream *standard-output*))   function

Write string to stream, ending with the HTTP end of line convention (CR+LF)

## (write-http-response-status-line stream &optional (status-code 200) (string ok) (http-version http/1.1))   function

Write an HTTP Response Status line to stream, using status-code string and http-version

