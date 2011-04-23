;;; NOTE
;;; To start the web side, call (hunchenoot:start (make-instance 'huchentoot:acceptor :port [port number]))

;;; Ensure we have Quicklisp running
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;;; Load web server and AJAX support
(ql:quickload 'hunchentoot)
(ql:quickload 'ht-simple-ajax)
(ql:quickload 'cl-who)
(ql:quickload 'parenscript)

(defpackage :kindle-presentation-ctl
  (:use :common-lisp
		:hunchentoot
		:ht-simple-ajax
		:cl-who
		:parenscript))

(in-package :kindle-presentation-ctl)

(defparameter *ajax-processor* (make-instance 'ajax-processor :server-uri "/ajax"))

;;; Functions that actually do things (ie. call Actuator)
(defun prev-slide ()
  (format t "prev-slide called~%")	  ; FIXME send message to actuator
  )

(defun next-slide ()
  (format t "next-slide called~%")		; FIXME as above
  )

(defun prev-notes ()
  (format t "prev-notes called~%")		; FIXME as above
  "Previous notes")

(defun next-notes ()
  (format t "next-notes called~%")		; FIXME as above
  "Next notes")

;;; Website handling functions

(defun-ajax say-hi (name) (*ajax-processor*)
  (concatenate 'string "Hi " name ", nice to meet you!"))

(defun-ajax handle-keypress (event) (*ajax-processor*)
	(format t "Pressed key: ~a~%" event)
	event)

(setq *dispatch-table* (list 'dispatch-easy-handlers
							 (create-ajax-dispatcher *ajax-processor*)))

(define-easy-handler (ajax-test :uri "/atest") ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html :xmlns "http://www.w3.org/1999/xhtml"
     (:head
      (:title "ht-simple-ajax demo")
      (princ (generate-prologue *ajax-processor*))
      (:script :type "text/javascript" "
// will show the greeting in a message box
function callback(response) {
  alert(response.firstChild.firstChild.nodeValue);
}

// calls our Lisp function with the value of the text field
function sayhi() {
  ajax_say_hi(document.getElementById('name').value, callback);
}
")
	  (:script :type "text/javascript" (str (ps (defun handle-keys (event)
											  (ajax_handle_keypress event callback))))))
	 
     (:body :onkeypress (ps (handle-keys event))
      (:p "Please enter your name: " 
          (:input :id "name" :type "text"))
      (:p (:a :href "" :onclick (ps (sayHi) (return false)) "Say Hi!"))))))


;;; The Webapp Handler

(define-easy-handler (presentation :uri "/presentation") ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
	(:html :xmlns "http://www.w3.org/1999/xhtml"
		   (:head (:title "Presentation Control")
				  (:script :language "text/javascript"
						   (str (ps* *ps-lisp-library*))
						   (str (ps
								  ;; Define control keys
								  ;; TODO more bindings
								  (defvar *next-slide-keys* (array #\i #\o))
								  (defvar *prev-slide-keys* (array #\w #\e))
								  (defvar *next-notes-keys* (array #\p))
								  (defvar *prev-notes-keys* (array #\q))
								  (defun change-notes (new-content)
									(setf (chain document (get-element-by-id "presentation-notes") inner-html)
										  new-content))
								  (defun change-title (new-title)
									(setf (chain document (get-element-by-id "presentation-title") inner-html)
										  new-title))
								  (defun handle-keypress (key)
									(member x y))))))
		   (:body
			(:div
			 (:h1 :id "presentation-title" "Presentation Title")
			 (:span "Javascript clock will go here."))
			(:hr)
			(:div :id "presentation-notes"
				  "Presentation notes will go here.")))))