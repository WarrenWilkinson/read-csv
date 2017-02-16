(defsystem :read-csv
  :name "read-csv"
  :version "1.0.2"
  :author "Warren Wilkinson <warrenwilkinson@gmail.com>"
  :license "cc0 (public domain)"
  :description "A library for reading CSV data from streams."
  :components ((:file "read-csv"))
  :in-order-to ((test-op (load-op read-csv.test))))

(defsystem :read-csv.test
  :name "read-csv.test"
  :version "1.0.2"
  :author "Warren Wilkinson <warrenwilkinson@gmail.com>"
  :description "Testing code for the read-csv library"
  :licence "cc0 (public domain)"
  :depends-on (:read-csv)
  :components ((:file "test")))
    
(defmethod perform ((op asdf:test-op) (system (eql (find-system :read-csv))))
  (funcall (intern "RUN-TESTS" :read-csv.test)))
