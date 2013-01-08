
;;; -*- Mode: LISP; Syntax: common-lisp; Package: read-csv; Base: 10 -*-

;;; Copyright (c) 2012, Warren Wilkinson.  All rights reserved.

;;; BEGIN_LICENSE:LGPL2
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Library General Public License as published by
;;; the Free Software Foundation; version 2.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public License
;;; along with this library; see the file COPYING.LIB.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301, USA.
;;;
;;; END_LICENSE 

(defpackage :read-csv
  (:use :common-lisp)
  (:export read-csv parse-csv))

(in-package :read-csv)

(defvar *records*)
(defvar *record*)
(defvar *white-char-count*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun noop (c) (declare (ignore c)))
  (defun addc (c) (push c *record*))
  (defun addl (c) (declare (ignore c)) (push #\Newline *record*))
  (flet ((white (c) (or (char= c #\Space) (char= c #\Tab))))
    (defun next (c) 
      (declare (ignore c))
      (let ((eow (or (position-if-not #'white *record*) (length *record*))))
        (push (coerce (nreverse (nthcdr (max 0 (min eow (1- *white-char-count*))) *record*)) 'string) *records*))
      (setf *record* nil)))
  (defun ship (c) (next c) (setf *records* (nreverse *records*))))

(defconstant done! -1)
(defconstant start 0) ;; Start
(defconstant retur 1)  ;; Return (as in CRLF seen)
(defconstant unquo 2)  ;; Unquoted text
(defconstant quote 3)  ;; Quoted text
(defconstant q+ret 4)  ;; Quoted and have seen return
(defconstant q+quo 5)  ;; Quoted and have seen quote
(defconstant q+q&w 6)  ;; Quoted and have seen quote, in following whitespace

(defconstant +csv-table+
  (if (boundp '+csv-table+) +csv-table+
      (make-array '(7 6 2) 
       :initial-contents
    ;;WHITE,         RETURN,          LF,              QUOTE,           SEP,             OTHER
`(((noop ,start) (ship ,retur) (ship ,done!) (noop ,quote) (next ,start) (addc ,unquo))
  ((noop ,start) (ship ,retur) (noop ,done!) (noop ,start) (next ,start) (addc ,unquo))
  ((addc ,unquo) (ship ,retur) (ship ,done!) (addc ,unquo) (next ,start) (addc ,unquo))
  ((addc ,quote) (noop ,q+ret) (addl ,quote) (noop ,q+quo) (addc ,quote) (addc ,quote))
  ((addc ,quote) (noop ,q+ret) (addl ,quote) (noop ,q+quo) (addc ,quote) (addc ,quote))
  ((noop ,q+q&w) (ship ,retur) (ship ,done!) (addc ,quote) (next ,start) (addc ,unquo))
  ((noop ,q+q&w) (ship ,retur) (ship ,done!) (addc ,quote) (next ,start) (addc ,unquo))))))

(defun char-class (sep char)
  (case char (#\Space 0) (#\Return 1) (#\Linefeed 2) (#\" 3) (otherwise (if (char= sep char) 4 5))))

(defun read-csv (stream &optional (sep #\,) (eof-error-p t) eof-value)
  "Return CSV data and a second value that's true if row ended by EOF."
  (let ((*records* nil)
        (*record* nil)
        (*white-char-count* 0))
    (declare (special *record* *records* *white-char-count*))
    (loop with state = start
          for char = (read-char stream (and (null *records*) eof-error-p) :eof)
          when (eq char :eof) 
          do (return-from read-csv (values (if *records* (ship :eof) eof-value) t))
          do (incf *white-char-count*)
          do (let ((class (char-class sep char)))
               (when (= class quote) (setf *white-char-count* 0))
               (funcall (aref +csv-table+ state class 0) char)
               (setf state (aref +csv-table+ state class 1)))
       until (eq state done!))
    (values *records* (eq :eof (peek-char nil stream nil :eof)))))
 
(defun parse-csv (stream  &optional (sep #\,))
  (loop for (line end-p) = (multiple-value-list (read-csv stream sep nil :eof))
        unless (eq line :eof) collect line
        until end-p))
