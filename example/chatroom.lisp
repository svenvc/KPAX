;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: chatroom.lisp,v 1.2 2005/10/17 15:21:49 sven Exp $
;;;;
;;;; A ChatRoom accessible with just a web browser.
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :kpax-user)

(defwebapp :chatroom
  (:index 'chatroom-start)
  (:static-root "static/")
  (:login-hook 'chatroom-login-hook)
  (:authorizer 'chatroom-login-authorizer))

;; Security (currently: login code = user name)

(defun chatroom-login-hook (request-response &optional logout)
  (let ((session (get-session request-response))
        (web-app (get-application request-response)))
    (when logout (setf (get-attribute session :user) nil))
    (html-page (out request-response)
      (:html 
       (:head
        (:title "KPAX ChatRoom Login")
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css")))
       (:body :style "padding:20px" 
        (:div :align "center"
         (:div :class "NX_login_panel"
          (:form :action (dynamic-url request-response (get-index web-app)) :method :post
           (:div :class "NX_title" "KPAX ChatRoom Login")
           (:div :class "NX_fields"
            (:table
             (:tr 
              (:td (:label "Code:"))
              (:td (:input :type "text" :name "code" :size 24 :value "")))))
           (:div :class "NX_buttons"
            (:input :type "submit" :value "Login"))))))))))

(defun chatroom-login-authorizer (request-response)
  (let* ((session (get-session request-response))
         (code (get-request-parameter-value request-response "code")))
    (if (and session
             code)
        (setf (get-attribute session :user) code)
      nil)))

(defun chatroom-logout (request-response)
  (logout request-response))

;; Model

(defclass message ()
  ((id :accessor get-id :initarg :id :initform -1)
   (user :accessor get-user :initarg :user :initform "Anonyous")
   (timestamp :accessor get-timestamp :initarg :timestamp :initform (get-universal-time))
   (text :accessor get-text :initarg :text :initform "")))

(defvar *messages* '())

(defvar *message-id-counter* 1000)

(defvar *messages-lock* (s-sysdeps:make-process-lock "Chatroom Messages Store"))

(defun add-message (message)
  "Add a new message to the shared store"
  (s-sysdeps:with-process-lock (*messages-lock*)
    (setf (get-id message) (incf *message-id-counter*))
    (push message *messages*)
    message))

(defun get-messages (&key (since 0) (limit 10))
  "Get a list of all message with ID strictly larger than since, but no more than limit, sorted last first"
  (let ((messages (remove-if-not #'(lambda (m) 
                                     (< since (get-id m))) 
                                 (reverse *messages*))))
    (subseq messages 0 (min limit (length messages)))))

;; Multi Threading Synchronization

(defvar *mailboxes* '())

(defvar *mailboxes-lock* (s-sysdeps:make-process-lock "Chatroom Mailboxes Store"))

(defun notify-all-outstanding-updates (&optional (token :update))
  "Tell all waiting processes (through their mailbox) that token has happened"
  (loop :for mailbox :in (s-sysdeps:with-process-lock (*mailboxes-lock*) 
                           (copy-list *mailboxes*))
        :do (mp:mailbox-send mailbox token)))

(defun wait-for-update (&optional (timeout 60))
  "Wait timeout seconds for an (update) event, or nil (create and wait on mailbox)"
  (let ((mailbox (mp:make-mailbox))
        token)
    (s-sysdeps:with-process-lock (*mailboxes-lock*)
      (push mailbox *mailboxes*))
    (setf token (mp:mailbox-read mailbox "Waiting for Chatroom update" timeout))
    (s-sysdeps:with-process-lock (*mailboxes-lock*)
      (setf *mailboxes* (remove mailbox *mailboxes*)))
    token))

;; JavaScript Support

(defun write-json-string (string stream)
  (write-char #\" stream)
  (loop :for char :across string 
        :do (case char
              ((#\") (write-string "\\\"" stream))
              ((#\\) (write-string "\\\\" stream))
              ((#\/) (write-string "\\/" stream))
              ((#\Tab) (write-string "\\t" stream))
              ((#\Backspace) (write-string "\\b" stream))
              ((#\Newline) (write-string "\\n" stream))
              ((#\Return) (write-string "\\r" stream))
              ((#\VT) (write-string "\\f" stream))
              (t (if (<= 32 (char-code char) 126)
                     (write-char char stream)
                   (format stream "\\u~2,'0x" (char-code char))))))
  (write-char #\" stream))

(defun message->js (message)
  (with-output-to-string (out)
    (write-char #\{ out)
    (format out "id:~d" (get-id message))
    (format out ",user:")
    (write-json-string (get-user message) out)
    (format out ",time:'~a'" (s-utils:format-universal-time (get-timestamp message) 
                                                            :format '(:hour #\: :minute #\: :second)))
    (format out ",text:")
    (write-json-string (get-text message) out)
    (write-char #\} out)))

(defun messages->js (messages)
  (with-output-to-string (out)
    (write-string "[" out)
    (write-string (message->js (first messages)) out)
    (loop :for message :in (rest messages) :do 
          (write-string ", " out)
          (write-string (message->js message) out))
    (write-string "]" out)))

;; Web App
  
(defun chatroom-start (request-response)
  (let* ((session (get-session request-response))
         (user (get-attribute session :user))
         (messages (get-messages))
         (last-message-id (if messages (get-id (first (last messages))) "null")))
    (html-page (out request-response)
      (:html 
       (:head 
        (:title "ChatRoom") 
        (:link :rel "stylesheet" :type "text/css" :href (static-url request-response :server "nx.css"))
        (:script :type "text/javascript" :src (static-url request-response :webapp "prototype.js") "")
        (:script :type "text/javascript" :src (static-url request-response :webapp "chatroom.js") ""))
       (:body 
        :onload "install_auto_updater();"
        (:script :type "text/javascript" 
         (fmt "base_url=\"~a\"; last_message_id=~a;" (dynamic-url request-response "") last-message-id))
        (:h1 "ChatRoom")
        (:div :class "NX_panel"
         (:span :class "NX_title" "KPAX")
         (:div :class "NX_border"
          (:p (fmt "Good ~a! Welcome to the KPAX Chat Room." (get-daypart)))
          (:div :style "width:580px; height:300px; overflow:auto; border: 1px solid white;"
           (:ul :id "messages"
            (loop :for message :in messages :do
                  (htm
                   (:li (fmt "[~a ~a]> ~a" 
                             (get-user message) 
                             (s-utils:format-universal-time (get-timestamp message) 
                                                            :format '(:hour #\: :minute #\: :second)) 
                             (get-text message)))))))
          (:div :style "width:580px; border: 1px solid white;"
           (:form
            :style "margin:4px;"
            :method :POST
            :onsubmit "send_message(); return false;"
            :action (dynamic-url request-response 'chatroom-new-message)
            (:label (str user))
            (:input :type "text" :name "message" :value "" :size 50 :id "message")
            (:input :type "submit" :value "Send")
            :br
            "[ "
            (:a :href (dynamic-url request-response nil) "Reload") 
            " | "
            (:a :href (dynamic-url request-response 'chatroom-logout) "Logout")
            " ] "
            (:span :id "progress" ""))))))))))

(defun chatroom-new-message (request-response)
  (let* ((session (get-session request-response))
         (user (get-attribute session :user))
         (message-text (get-request-parameter-value request-response "message")))
    (when message-text
      (add-message (make-instance 'message :text message-text :user user))
      (log-info request-response "notify-all ~s" mp:*current-process*) 
      (notify-all-outstanding-updates))
    (chatroom-start request-response)))

(defun chatroom-new-message-subrequest (request-response)
  (let* ((session (get-session request-response))
         (user (get-attribute session :user))
         (message-text (get-request-parameter-value request-response "message")))
    (when message-text
      (add-message (make-instance 'message :text message-text :user user))
      (log-info request-response "notify-all ~s" mp:*current-process*)
      (notify-all-outstanding-updates))
    (write-string "OK" (get-content-stream request-response))))

(defun chatroom-get-updates-subrequest (request-response)
  (let ((since (s-utils:parse-integer-safely (get-request-parameter-value request-response "since"))))
    (if since
        (let (messages)
          (let ((new-messages (get-messages :since since)))
            (if new-messages
                (setf messages (messages->js new-messages))
              (let ((event (progn
                             (log-info request-response "wait ~s" mp:*current-process*)
                             (wait-for-update))))
                (if (eql event :update)
                    (let ((new-messages (get-messages :since since)))
                      (when messages (messages->js new-messages))))))
            (log-info request-response "update with ~d messages" (length new-messages))
            (write-string (or messages "null") 
                          (get-content-stream request-response))))
      (write-string "null" (get-content-stream request-response)))))

;;;; eof
