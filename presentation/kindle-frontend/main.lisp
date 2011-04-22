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

(defun-ajax say-hi (name) (*ajax-processor*)
  (concatenate 'string "Hi " name ", nice to meet you!"))

(defun-ajax handle-keypress (event) (*ajax-processor*)
	(format t "Pressed key: ~a~%" key)
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