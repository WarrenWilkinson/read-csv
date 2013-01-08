
;;; -*- Mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
  
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
  
(defsystem :read-csv
  :name "read-csv"
  :version "1.0.0"
  :author "Warren Wilkinson <warrenwilkinson@gmail.com>"
  :license "lgpl2"
  :description "A library for reading CSV data from streams."
  :components ((:file "read-csv"))
  :in-order-to ((test-op (load-op read-csv.test))))

(defsystem :read-csv.test
  :name "read-csv.test"
  :version "1.0.0"
  :author "Warren Wilkinson <warrenwilkinson@gmail.com>"
  :description "Testing code for the read-csv library"
  :licence "LGPL2"
  :depends-on (:read-csv)
  :components ((:file "test")))
    
(defmethod perform ((op asdf:test-op) (system (eql (find-system :read-csv))))
  (funcall (intern "RUN-TESTS" :read-csv.test)))
