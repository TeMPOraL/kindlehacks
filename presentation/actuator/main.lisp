;;; Actuator module for Kindle Presentation Controller.

;;; Load our protocol
(load "../protocol/protocol.lisp")

(defpackage :kindle-presentation-actuator
  (:use :common-lisp
        :kindle-presentation-protocol)
  (:export :connect-to-presentation-controller))

(in-package :kindle-presentation-actuator)

(defparameter *cookie* "OVERLORD, THIS IS RAPTOR 2-1, STANDBY FOR SITREP, OVER.")
(defparameter *notes* '("Sample notes." "More sample notes." "Even more sample notes."
                        "A
multiline

note"))

;;; Interface for controlling presentation
(defun next-slide ()
  ;; do magic
  (print :next-slide))

(defun prev-slide ()
  ;; do magic
  (print :next-slide))

;;; Running the whole system
(defun connect-to-presentation-controller (host port)
  (let ((socket (ccl:make-socket :remote-host host :remote-port port)))
    (unwind-protect
         (progn
           (authorize socket)
           (send-all-notes socket)
           (receive-commands-until-bored socket))
      (close socket))))

(defun authorize (stream)
  (write-line *cookie* stream)
  (force-output stream)
  (let ((response (read-line stream)))
    (format t "RECVD: ~a~%" response)))

(defun send-all-notes (socket)
  (write-line "notes" socket)
  (send-notes *notes* socket))

(defun receive-commands-until-bored (stream)
  (loop (let ((line (read-line stream)))
          (format t "RCVD (loop): ~a~%" line))))


;;; Implementation of presentation controll
(eval-when (:compile-toplevel :load-toplevel :execute)
  (open-shared-library "user32.dll"))

(defun testkey ()
  (sleep 2)
  (#_keybd_event #x25 0 0 0)
  (sleep 1)
  (#_keybd_event #x25 0 #$KEYEVENTF_KEYUP 0))