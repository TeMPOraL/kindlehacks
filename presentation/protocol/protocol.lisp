;;; This file describes transport layer protocol for notes.
;;; In sending notes from presentation location to presentation controller we care about following formats:
;;; * Input format - how notes are stored and put into program
;;; * Transport format - for sending it between two applications
;;; * Output format - the way the notes are displayed by presentation controller
;;; * Intermediate format - how we store notes as Lisp data for easy processing
;;;
;;; Format conversions are basically as follows:
;;; Input format  --> Intermediate format --> Transport format --> Intermediate format --> Output format
;;;
;;; This file defines a common transport format for note data. Functions defined here will allow to encode
;;; and decode notes without having to know anything specific about the format.
;;; Intermediate format is assumed to be a list of strings, each string representing a single note.
;;; Functions defined in this file accept and return data in Intermediate format.


(defpackage :kindle-presentation-protocol
  (:use :common-lisp)
  (:export :intermediate->transport
           :transport->intermediate
           :recv-notes
           :send-notes))

(in-package :kindle-presentation-protocol)

(defun recv-notes (stream)
  (read stream))

(defun send-notes (notes stream)
  (print notes stream)
  (force-output stream))