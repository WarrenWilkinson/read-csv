(defpackage :read-csv
  (:documentation "A simple CSV file reader.")
  (:use :common-lisp)
  (:export
   #:read-csv
   #:parse-csv))

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
(defconstant myquo 3)  ;; Quoted text
(defconstant q+ret 4)  ;; Quoted and have seen return
(defconstant q+quo 5)  ;; Quoted and have seen quote
(defconstant q+q&w 6)  ;; Quoted and have seen quote, in following whitespace

(defconstant +csv-table+
  (if (boundp '+csv-table+) +csv-table+
      (make-array '(7 6 2) 
       :initial-contents
       ;;WHITE,         RETURN,       LF,           QUOTE,        SEP,          OTHER           ;; STATE 
       `(((noop ,start) (ship ,retur) (ship ,done!) (noop ,myquo) (next ,start) (addc ,unquo))  ;; start
	 ((noop ,start) (ship ,retur) (noop ,done!) (noop ,start) (next ,start) (addc ,unquo))  ;; return seen
	 ((addc ,unquo) (ship ,retur) (ship ,done!) (addc ,unquo) (next ,start) (addc ,unquo))  ;; unquoted text
	 ((addc ,myquo) (noop ,q+ret) (addl ,myquo) (noop ,q+quo) (addc ,myquo) (addc ,myquo))  ;; in-quote
	 ((addc ,myquo) (noop ,q+ret) (addl ,myquo) (noop ,q+quo) (addc ,myquo) (addc ,myquo))  ;; in-quote, seen return
	 ((noop ,q+q&w) (ship ,retur) (ship ,done!) (addc ,myquo) (next ,start) (addc ,unquo))  ;; in-quote, seen quote
	 ((noop ,q+q&w) (ship ,retur) (ship ,done!) (addc ,myquo) (next ,start) (addc ,unquo)))))) ;; in quote, seen quote, now whitespace

(defun char-class (sep char)
  (case char (#\Space 0) (#\Return 1) (#\Linefeed 2) (#\" 3) (otherwise (if (char= sep char) 4 5))))

(defun read-csv (stream &optional (sep #\,) (eof-error-p t) eof-value)
  "Read a single line of CSV data from stream. Return the parsed CSV
   data and a boolean that is true when you've just read the last
   record in the stream.

   eof-error-p controls how this function behaves if the first
   character read is end-of-file.  If true, an error is thrown, if
   false, the eof-value is returned."
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
               (when (= class myquo) (setf *white-char-count* 0))
               (funcall (aref +csv-table+ state class 0) char)
               (setf state (aref +csv-table+ state class 1)))
       until (eq state done!))
    (values *records* (eq :eof (peek-char nil stream nil :eof)))))
 
(defun parse-csv (stream  &optional (sep #\,))
  "Read CSV data from a stream until end-of-file is encountered."
  (loop for (line end-p) = (multiple-value-list (read-csv stream sep nil :eof))
        unless (eq line :eof) collect line
        until end-p))
