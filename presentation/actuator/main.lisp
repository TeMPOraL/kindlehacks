;;; Actuator module for Kindle Presentation Controller.
(defpackage :kindle-presentation-actuator
  (:use :common-lisp))

(in-package :kindle-presentation-actuator)


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
           (receive-commands-until-bored socket))
      (close socket))))

(defun authorize (stream)
  (write-line "OVERLORD, THIS IS RAPTOR 2-1, STANDBY FOR SITREP, OVER." stream)
  (force-output stream)
  (let ((response (read-line stream)))
    (format t "RECVD: ~a~%" response)))

(defun receive-commands-until-bored (stream)
  (loop (let ((line (read-line stream)))
          (format t "RCVD (loop): ~a~%" line))))


;;; Implementation of presentation controll