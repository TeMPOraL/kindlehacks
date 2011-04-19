;;; Ensure we have Quicklisp running
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;;; Load web server and AJAX support
(ql:quickload 'hunchentoot)
(ql:quickload 'ht-simple-ajax)

(defpackage :kindle-presentation-ctl
  (:use :common-lisp
		:hunchentoot
		:ht-simple-ajax))

(in-package :kindle-presentation-ctl)

(defparameter *ajax-processor* (make-instance 'ajax-processor :server-uri "/ajax"))

(defun-ajax say-hi (name) (*ajax-processor*)
  (concatenate 'string "Hi " name ", nice to meet you!"))

(setq *dispatch-table* (list 'dispatch-easy-handlers
							 (create-ajax-dispatcher *ajax-processor*)))

(ql:quickload 'cl-who)
(use-package :cl-who)

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
function sayHi() {
  ajax_say_hi(document.getElementById('name').value, callback);
}
"))
     (:body
      (:p "Please enter your name: " 
          (:input :id "name" :type "text"))
      (:p (:a :href "javascript:sayHi()" "Say Hi!"))))))