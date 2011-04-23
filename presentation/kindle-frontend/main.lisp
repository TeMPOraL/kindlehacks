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
(defun-ajax prev-slide () (*ajax-processor*)
  (format t "prev-slide called~%")    ; FIXME send message to actuator
  )

(defun-ajax next-slide () (*ajax-processor*)
  (format t "next-slide called~%")      ; FIXME as above
  )

(defun-ajax prev-notes () (*ajax-processor*)
  (format t "prev-notes called~%")      ; FIXME as above
  "Previous notes")

(defun-ajax next-notes () (*ajax-processor*)
  (format t "next-notes called~%")      ; FIXME as above
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
                  (princ (generate-prologue *ajax-processor*))
                  (:script :type "text/javascript"
                           (str (ps* *ps-lisp-library*))
                           (str (ps
                                  ;; TODO the most important part still remains to be written - the code
                                  ;; for actually fetching keypress events and converting them to usable char codes.

                                  ;; Define control keys
                                  ;; TODO more bindings
                                  (defun chars->codes (arr) (map (lambda (c) (chain c (char-code-at))) arr))
                                  (defvar *next-slide-keys* (chars->codes (array #\Y #\U #\I #\O #\H #\J #\K #\L #\N #\M #\.)))
                                  (defvar *prev-slide-keys* (chars->codes (array #\W #\E #\R #\T #\A #\S #\D #\F #\G #\Z #\X #\C #\V #\B)))
                                  (defvar *next-notes-keys* (chars->codes (array #\P)))
                                  (defvar *prev-notes-keys* (chars->codes (array #\Q)))
                                  (defvar *refresh-page-keys* (array #\Space))
                                  (alert (+ "keys: " *next-slide-keys*))
                                  (defun change-notes (new-content)
                                    (setf (chain document (get-element-by-id "presentation-notes") inner-h-t-m-l)
                                          (@ new-content first-child first-child node-value)))
                                  (defun change-title (new-title)  ; FIXME unused - remove or use
                                    (setf (chain document (get-element-by-id "presentation-title") inner-h-t-m-l)
                                          new-title))
                                  (defun handle-keypress (evt)
                                    ;; Key scanning code is based on some network research. There might be a browser-compatibility
                                    ;; workaround hidden in it, I don't know. As long as it works on Kindle, I don't care at the moment.
                                    ;; I'll investigate it when refactoring one day ;).
                                    (let ((key (if (@ window event)
                                                   (@ window event key-code)
                                                   (if evt (@ e which)))))
                                      (cond ((member key *next-slide-keys*) (ajax_prev_slide nil))
                                            ((member key *prev-slide-keys*) (ajax_prev_slide nil))
                                            ((member key *next-notes-keys*) (ajax_next_notes #'change-notes))
                                            ((member key *prev-notes-keys*) (ajax_prev_notes #'change-notes))))
                                    (refresh-page-content))
                                  (defun refresh-page-content ()
                                    ;; TODO refresh clocks, whatever
                                    )
                                  (setf (@ document onkeydown) #'handle-keypress)))))
           (:body
            (:div
             (:h1 :id "presentation-title" "Presentation Title")
             (:span :id "wall-clock" "Javascript wall clock will go here.")
             (:span :id "presentation-clock" "Presentation clock (ie. how long are you talking) will go here."))
            (:hr)
            (:div :id "presentation-notes"
                  "Presentation notes will go here.")))))

(defun start-this-thing (port)
  (start (make-instance 'acceptor :port port)))