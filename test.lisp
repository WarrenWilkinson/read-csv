
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CHANGED-STREAM.TEST; Base: 10 -*-

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

(defpackage :read-csv.test
  (:use :common-lisp :read-csv)
  (:export run-tests))

(in-package :read-csv.test)

(defvar *all-statements*
  (list ;; Blank Tests
        (cons (concatenate 'string "") (concatenate 'string ""))
        (cons (concatenate 'string " " (list #\Tab) " ") (concatenate 'string ""))
        (cons (concatenate 'string "\"\"") (concatenate 'string ""))
        (cons (concatenate 'string "\" " (list #\Tab) " \"") (concatenate 'string " " (list #\Tab) " "))
        ;; Quote Tests
        (cons (concatenate 'string "\"multi" (list #\Newline) "line\"") (concatenate 'string "multi" (list #\Newline) "line"))
        (cons (concatenate 'string "\",\"") (concatenate 'string ","))
        (cons (concatenate 'string "\"\"\"\"") (concatenate 'string "\""))
        ;; international Tests
        (cons (concatenate 'string "\"êve,y" (list #\Newline) "th还ng" (list #\Tab) "at\"\"once\"") (concatenate 'string "êve,y" (list #\Newline) "th还ng" (list #\Tab) "at\"once"))))

 
(defun concat-with (strings item)
  (if (null strings)
      ""
      (apply #'concatenate 'string (first strings) (mapcan #'(lambda (a) (list item a)) (rest strings)))))

(defun build-answers (i strings)
  (loop while strings
        collect (loop for n upto (1- i)
                      while strings 
                      collect (car strings)
                      do (setf strings  (cdr strings)))))

(defun build-string (i strings)
  (concatenate 'string 
     (concat-with (mapcar #'(lambda (s) (concat-with s ",")) (build-answers i strings)) (list #\Newline))
     '(#\Newline)))

(defun all-combinations (patterns) 
   (if (null (cdr patterns))
       (list patterns)
       (loop for i in patterns
             nconc (mapcar #'(lambda (p) (cons i p)) (all-combinations (remove i patterns))))))

(defun make-test (description)
  #'(lambda ()
      (block test
        ;(loop for pattern in (all-combinations description)
        (format t "~%Pattern: ~s" (substitute #\' #\" (remove #\Newline (build-string (length description) (mapcar #'car description)))))
        (dotimes (i (length description) t)
          (format t "~%  @~d" i)
          (let ((string (build-string (1+ i) (mapcar #'car description)))
                (answers (build-answers (1+ i) (mapcar #'cdr description))))
            (with-input-from-string (s string)
              (loop for answer in answers
                    for (got end) = (multiple-value-list (read-csv s))
                    unless (equalp answer got)
                    do (format t "~%Expected ~a, got ~a" answer got)
                    and do (return-from test nil)
                    if (eq answer (car (last answers)))
                    unless end
                    do (format t "~%Expected EOF, but didn't see it!")
                    unless (not end)
                    do (format t "~%Did not expect EOF, but saw it!"))
              (let ((read-more (read-csv s #\, nil :eof)))
              (unless (eq read-more :eof)
                (format t "~%Could read past end: ~s" read-more)
                (return-from test nil)))))))))



(defmacro deftest (name code result)
  `(defun ,name ()
     (format t "~%~a" ',name)
     (let ((expect ,result)
           (got ,code))
       (if (equalp expect got)
           t
           (progn (format t "~%Expected~% ~s~%but got~%~s" expect got)
                  nil)))))

(defvar *a-tough-example-answer* '(("very tough" "easier to do")))
(defun a-tough-example () 
  (with-input-from-string (s "  \"very tough\"   ,    easier to do     
")
    (parse-csv s)))

(deftest test-tough (a-tough-example) *a-tough-example-answer*)

(defvar *big-example-answer*
  '(("first name" "last name"   "job \"title\""                      "number of hours" "id")
    ("Russ"       "Tyndall"     "Software Developer's, \"Position\"" "26.2"            "1")
    ("Adam"       "Smith"       "Economist"                          "37.5"            "2")
    ("John"       "Doe"         "Anonymous Human"                    "42.1"            "3")
    ("Chuck"      "Darwin"      "Natural Philosopher"                "17.68"           "4")
    ("Bill"       "Shakespeare" "Bard"                               "12.2"            "5")
    ("James"      "Kirk"        "Starship Captain"                   "13.1"            "6")
    ("Bob"        "Anon"        ""                                   "13.1"            "6")
    ("Mr"         "Iñtërnâtiônàlizætiøn" ""                          "1.1"             "0")))

(defun big-example () 
  (with-input-from-string (s "first name,last name,\"job \"\"title\"\"\",number of hours,id
Russ,Tyndall,\"Software Developer's, \"\"Position\"\"\",26.2,1
Adam,Smith,Economist,37.5,2
John,Doe,Anonymous Human,42.1,3
Chuck,Darwin,Natural Philosopher,17.68,4
Bill,Shakespeare,Bard,12.2,5
James,Kirk,Starship Captain,13.1,6
Bob,Anon,,13.1,6
Mr,Iñtërnâtiônàlizætiøn,,1.1,0")
    (parse-csv s)))


(defun quoted-big-example () 
  (with-input-from-string 
      (s "\"first name\",\"last name\",\"job \"\"title\"\"\",\"number of hours\",\"id\"
\"Russ\",\"Tyndall\",\"Software Developer's, \"\"Position\"\"\",\"26.2\",\"1\"
\"Adam\",\"Smith\",\"Economist\",\"37.5\",\"2\"
\"John\",\"Doe\",\"Anonymous Human\",\"42.1\",\"3\"
\"Chuck\",\"Darwin\",\"Natural Philosopher\",\"17.68\",\"4\"
\"Bill\",\"Shakespeare\",\"Bard\",\"12.2\",\"5\"
\"James\",\"Kirk\",\"Starship Captain\",\"13.1\",\"6\"
\"Bob\",\"Anon\",\"\",\"13.1\",\"6\"
\"Mr\",\"Iñtërnâtiônàlizætiøn\",\"\",\"1.1\",\"0\"")
    (parse-csv s)))

(deftest test-big        (big-example)        *big-example-answer*)
(deftest test-quoted-big (quoted-big-example) *big-example-answer*)

(defvar *multiline-answer*
  '(("this" "is" "a" "test
of
multiline" "data")
    ("row2" "of" "the" "test
of
multiline" "data")))

(defun multiline-unix-example () 
  (with-input-from-string (s "this,is,a,\"test
of
multiline\", data
row2,of,the,\"test
of
multiline\", data")
    (parse-csv s)))

(defun multiline-dos-example ()
  (with-input-from-string 
      (s (concatenate 'string "this,is,a,\"test" (list #\Return #\Linefeed)
                      "of" (list #\Return #\Linefeed)
                      "multiline\", data" (list #\Return #\Linefeed)
                      "row2,of,the,\"test" (list #\Return #\Linefeed)
                      "of" (list #\Return #\Linefeed)
                      "multiline\", data" (list #\Return #\Linefeed)))
    (parse-csv s)))

(defun multiline-mixed-example ()
  (with-input-from-string
      (s (concatenate 'string "this,is,a,\"test" (list #\Linefeed)
                      "of" (list #\Return #\Linefeed)
                      "multiline\", data" (list #\Return #\Linefeed)
                      "row2,of,the,\"test" (list #\Linefeed)
                      "of" (list #\Return #\Linefeed)
                      "multiline\", data" (list #\Linefeed)))
    (parse-csv s)))

(deftest test-multiline-unix  (multiline-unix-example)  *multiline-answer*)
(deftest test-multiline-dos   (multiline-dos-example)   *multiline-answer*)
(deftest test-multiline-mixed (multiline-mixed-example) *multiline-answer*)


(defstruct results
  (tests 0)
  (failures nil))
(defun results-failure-count (results)
  (length (results-failures results)))
(defun results-successes (results)
  (- (results-tests results)
     (results-failure-count results)))

(defun runtest (fun results)
  (let* ((success t)
         (output (with-output-to-string (*standard-output*)
                   (setf success (handler-case (funcall fun)
                                   (error (e) (princ e) nil))))))
    (make-results
     :tests (1+ (results-tests results))
     :failures (if success
                   (results-failures results)
                   (acons fun output (results-failures results))))))

(defun present-failures (results)
  (format t "~%READ-CSV FAILURES:~%")
  (loop for (fn . problems) in (results-failures results)
        do (format t "~%~a~a~%" fn problems)))
(defun present-results (results)
  (format t "~%READ-CSV TEST RESULTS:")
  (format t "~%     Tests: ~a~%   Success: ~a~%  Failures: ~a" 
          (results-tests results)
          (results-successes results)
          (results-failure-count results))
  (when (results-failures results)
    (present-failures results)))
    
(defun run-combination-tests (starting-results)
  (reduce #'(lambda (description results) (runtest (make-test description) results))
           (nreverse (mapcan #'(lambda (thing) (all-combinations (subseq thing 0 (min (length thing) 5))))
                    (loop for i on (reverse *all-statements*) collecting i)))
           :from-end t
           :initial-value starting-results))
    
(defun run-explicit-tests (starting-results)
  (reduce #'(lambda (results function) (runtest function results))
           (list #'test-tough
                 #'test-big
                 #'test-quoted-big
                 #'test-multiline-unix
                 #'test-multiline-dos
                 #'test-multiline-mixed)
           :initial-value starting-results))
  
(defun run-tests ()
  (format t "~%RUNNING READ-CSV TESTS...")
  (present-results (run-explicit-tests (run-combination-tests (make-results)))))
