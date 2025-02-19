(defpackage :read-csv.test
  (:use :common-lisp :read-csv)
  (:export run-tests))

(in-package :read-csv.test)

(defparameter *all-statements*
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

(defparameter *a-tough-example-answer* '(("very tough" "easier to do")))
(defun a-tough-example () 
  (with-input-from-string (s "  \"very tough\"   ,    easier to do     
")
    (parse-csv s)))

(deftest test-tough (a-tough-example) *a-tough-example-answer*)

(defparameter *big-example-answer*
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

(defparameter *multiline-answer-data*
  ;; List of csv lines. Each csv line is a list elements, each of
  ;; which is either a string or a list of strings. A string is
  ;; interpreted as itself. For a list of strings, it is interpreted
  ;; as a string with line endings between each element. The specific
  ;; line ending is determined at run time: either a CR (Mac), LF
  ;; (Unix), or a CR-LF sequence (DOS).
  '(("this" "is" "a" ("test" "of" "multiline") "data")
    ("row2" "of" "the" ("test" "of" "multiline") "data")))

(defun eol-to-line-ending (eol)
  (ecase eol
    (:lf '#.(format nil "~c" #\Newline))
    (:cr '#.(format nil "~c" #\Return))
    (:crlf '#.(format nil "~c~c" #\Return #\Newline))))


(defparameter *default-eol* ':lf)

(defun multiline-datum-to-answer (datum)
  (loop with line-ending = (eol-to-line-ending *default-eol*)
        for x in datum
        if (atom x)
          collect x
        else collect
             (with-output-to-string (out)
               (loop for (y . more?) on x
                     do (write-string y out)
                     when more?
                       do (write-string line-ending out)))))

(defun multiline-data-to-answer-data (data)  
  (loop for datum in data
        collect (multiline-datum-to-answer datum)))

(defun multiline-datum-to-example-datum (datum &optional eol)
  (with-output-to-string (out)
    (loop with line-ending = (eol-to-line-ending (or eol *default-eol*))
          for (x . more?) on datum
          if (atom x)
            do (write-string x out)
          else do (write-char #\" out)
                  (loop for (y . more?) on x
                        do (write-string y out)
                        when more?
                          do (write-string line-ending out))
                  (write-char #\" out)
          when more?
            do (write-string "," out))))

(defun multiline-data-to-example-data (data &optional eol)
  (when (null eol)
    (setq eol *default-eol*))
  (let ((flip t))                       ; for eol = :mixed
    (flet ((linebreak-out (out)
             (ecase eol
               (:mixed 
                (write-string (eol-to-line-ending (if flip :lf :crlf)) out)
                (setq flip (not flip)))
               ((:cr :lf :crlf)
                (write-string (eol-to-line-ending eol) out)))))
      (with-output-to-string (out)
        (loop for datum in data
              do (loop for (x . more?) on datum
                       if (atom x)
                         do (write-string x out)
                       else do (write-char #\" out)
                               (loop for (y . more?) on x
                                     do (write-string y out)
                                     when more?
                                       do (linebreak-out out))
                               (write-char #\" out)
                       when more?
                         do (write-string "," out))
                 (linebreak-out out))))))
        

(defparameter *multiline-answer*
  (multiline-data-to-answer-data *multiline-answer-data*))
        

(defun multiline-example (example-type)
  (let ((eol
          (ecase example-type
            (:unix :lf)
            (:dos :crlf)
            (:mac :cr)
            (:mixed :mixed))))
    (with-input-from-string
        (s (multiline-data-to-example-data *multiline-answer-data* eol))
      (parse-csv s))))
    

(defun multiline-unix-example ()
  (multiline-example ':unix))

(defun multiline-dos-example ()
  (multiline-example ':dos))

(defun multiline-mac-example ()
  (multiline-example ':mac))

(defun multiline-mixed-example ()
  (multiline-example ':mixed))

(deftest test-multiline-unix  (multiline-unix-example)  *multiline-answer*)
(deftest test-multiline-dos   (multiline-dos-example)   *multiline-answer*)
(deftest test-multiline-mac   (multiline-mac-example)   *multiline-answer*)
(deftest test-multiline-mixed (multiline-mixed-example) *multiline-answer*)

(defun misc-tests ()
  (handler-case (eval '(let ((quote 'cl:quote))
                        (declare (ignore quote))
                        :ok))
    (error () :fail)))

(deftest test-misc-tests (misc-tests) :ok)

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
                 #'test-multiline-mac
                 #'test-multiline-mixed
                 #'test-misc-tests)
           :initial-value starting-results))
  
(defun run-tests ()
  (format t "~%RUNNING READ-CSV TESTS...")
  (present-results (run-explicit-tests (run-combination-tests (make-results)))))
