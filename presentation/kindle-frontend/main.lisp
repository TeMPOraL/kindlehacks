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
  (format t "prev-slide called~%")      ; DEBUG, remove?
  (send-command "prev-slide"))

(defun-ajax next-slide () (*ajax-processor*)
  (format t "next-slide called~%")
  (send-command "next-slide"))

(defun-ajax prev-notes () (*ajax-processor*)
  (format t "prev-notes called~%")      ; FIXME implement...
  (get-prev-note))

(defun-ajax next-notes () (*ajax-processor*)
  (format t "next-notes called~%")      ; FIXME as above
  (get-next-note))

;;; Website handling functions

(setq *dispatch-table* (list 'dispatch-easy-handlers
                             (create-ajax-dispatcher *ajax-processor*)))

;;; The Webapp Handler

(define-easy-handler (presentation :uri "/presentation") ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html :xmlns "http://www.w3.org/1999/xhtml"
           (:head (:title "Presentation Control")
                  (princ (generate-prologue *ajax-processor*))
                  (:script :type "text/javascript"
                           ;; Parenscript 2.3 has a buggy `member' function in *ps-lisp-library*.
                           ;; TODO Remove this workaround when PS gets fixed.
                           "
function member_trc(item, arr) {
    for (var el = null, _js_idx126 = 0; _js_idx126 < arr.length; _js_idx126 += 1) {
        el = arr[_js_idx126];
        if (el === item) {
            return true;
        };
    };
    return false;
};"
                           (str (ps* *ps-lisp-library*))
                           (str (ps
                                  ;; TODO the most important part still remains to be written - the code
                                  ;; for actually fetching keypress events and converting them to usable char codes.

                                  ;; Define control keys
                                  ;; TODO more bindings
                                  (defun chars->codes (arr) (map (lambda (c) (chain c (char-code-at))) arr))
                                  (defvar *next-slide-keys* (chars->codes (array #\Y #\U #\I #\O #\H #\J #\K #\L #\N #\M)))
                                  (defvar *prev-slide-keys* (chars->codes (array #\W #\E #\R #\T #\A #\S #\D #\F #\G #\Z #\X #\C #\V #\B)))
                                  (defvar *next-notes-keys* (chars->codes (array #\P)))
                                  (defvar *prev-notes-keys* (chars->codes (array #\Q)))
                                  (defvar *refresh-page-keys* (array #\Space))
                                  (defvar *reset-timer-keys* (array #\.))

                                  (defun reset-timer ()
                                    )
                                  
                                  (defun enforce-2digits (number)
                                    (if (< number 10)
                                        (+ "0" number)
                                        number))

                                  (defun make-reasonably-looking-time (dateobj)
                                    (+ (enforce-2digits (chain dateobj (get-hours)))
                                       ":"
                                       (enforce-2digits (chain dateobj (get-minutes)))
                                       ":"
                                       (enforce-2digits (chain dateobj (get-seconds)))))

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
                                      (cond ((member_trc key *next-slide-keys*) (ajax_next_slide nil))
                                            ((member_trc key *prev-slide-keys*) (ajax_prev_slide nil))
                                            ((member_trc key *next-notes-keys*) (ajax_next_notes #'change-notes))
                                            ((member_trc key *prev-notes-keys*) (ajax_prev_notes #'change-notes))
                                            ((member_trc key *reset-timer-keys* (reset-timer)))))
                                    (refresh-page-content))
                                  (defun refresh-page-content ()
                                    ;; TODO refresh clocks, whatever
                                    (let ((current-time (new  (-date))))
                                      (setf (chain document (get-element-by-id "wall-clock") inner-h-t-m-l)
                                            (make-reasonably-looking-time current-time))))



                                  (setf (@ document onkeydown) #'handle-keypress)))))
           (:body
            (:div
             (:h1 :id "presentation-title" "Presentation Title")
             (:span :id "wall-clock" "Javascript wall clock will go here.")
             (:span :id "presentation-clock" "Press any key to start the clock."))
            (:hr)
            (:div :id "presentation-notes"
                  "Presentation notes will go here.")))))

(defun start-this-thing (port)
  "Start up the web server and (TODO) presentation controller interface on given ports."
  (start (make-instance 'acceptor :port port)))


;;; Client-server communication handler

(defun clamp (a b what)
  "Clamp input value to range <a, b>. If a > b, values are swapped."
  (let ((a (min a b))
        (b (max a b)))
    (min b (max a what))))

(defvar *command-stream* nil)


(let ((command-stream-lock (ccl:make-lock)))
  (defun send-command (command)
    "Send a command (any lisp object) to the connected client. If the client is not connected, the message will be ignored."
    (ccl:with-lock-grabbed (command-stream-lock)
      (ignore-errors
        (write-line command *command-stream*)
        (force-output *command-stream*))))

  (defun change-command-stream-to (new-command-stream)
    "Change the stream to which commands are written."
    (ccl:with-lock-grabbed (command-stream-lock)
      (setf *command-stream* new-command-stream))))

;;; NOTE there is no locking for reading from *command-stream* as it will be done from the same thread
;;; from which change-command-stream-to is called.

(let ((notes-lock (ccl:make-lock))
      (current-note 0)
      (notes '()))

  (defun get-next-note ()
    "Returns a next note, and as additional values, index of this note (counting from 1) and number of notes available. Notes do not wrap around."
    (ccl:with-lock-grabbed (notes-lock)
      (if (not (null notes))
          (let ((len (length notes)))
            (setf current-note (clamp 0 (1- len) (1+ current-note)))
            (values (nth current-note notes)
                    (1+ current-note)
                    len)))))

  (defun get-prev-note ()
    "Returns a previous note, and as additional values, index of this note (counting from 1) and number of notes available. Notes do not wrap around."
    (ccl:with-lock-grabbed (notes-lock)
      (if (not (null notes))
          (let ((len (length notes)))
            (setf current-note (clamp 0 (1- len) (1- current-note)))
            (values (nth current-note notes)
                    (1+ current-note)
                    len)))))

  (defun set-notes (new-notes)
    (ccl:with-lock-grabbed (notes-lock)
      (setf notes new-notes))))


(defun run-command-server (port)
  "Initializes a command server on given port. The server will loop indefinetly accepting a single connection,
then reading and interpreting commands from the other side, until the other side terminates the connection."
  ;; TODO maybe put a nicely named restart here?
  (let ((socket (ccl:make-socket :local-port port :connect :passive)))
    (unwind-protect
         (loop (with-open-stream (stream (ccl:accept-connection socket))
                 (read-hello stream)
                 (write-hello stream)
                 ;; we got so far, so it seems that hello went ok - let's rebind command stream
                 (change-command-stream-to stream)
                 (read-notes-until-end-of-connection stream)
                 (change-command-stream-to nil)))
      (close socket :abort t)
      (format t "RUN-COMMAND-SERVER: command server aborted; socked freed."))))

(defun read-hello (stream)
  "Reads and verifies 'hello' message from client. TODO do something on failure."
  ;; TODO implement verification - maybe signal an 'unauthorized' condition?
  (format t "READ-HELLO: RECVD: ~a~%" (read-line stream)))

(defun write-hello (stream)
  "Writes a 'hello, go ahead' message to client."
  (write-line "THIS IS OVERLORD, SEND YOUR TRAFFIC, OVER." stream)
  (force-output stream))

(defun read-notes-until-end-of-connection (stream)
  "Will read note-change requests until other end terminates the connection."
  (ignore-errors
    (let ((end-connection nil))         ; will be set to t if client requests ending the connection gracefully
      (while (not end-connection)
        (let ((stuff (read-line stream)))
          (if (string= stuff "end" :end1 2 :end2 2)
              (setf end-connection t)
              (format t "READ-NOTES-UNTIL-END-OF-CONNECTION: READ ~a~%" stuff)))))))