read-csv
========

read-csv is a stream oriented CSV (comma-separated value) reader.

   * Small (~50 lines of code)
   * Supports quotes, including newlines and separator characters within the quotes.
   * Supports Unix style LF line endings and Dos CRLF line endings. (automatically)

```lisp
(ql:quickload 'read-csv)
(use-package :read-csv)
(with-open-file (s "/path/to/csv")
  (parse-csv s))
;; Returns a list of lists of strings.
```

Tests
============

Once the system is loaded, it can be tested with asdf. 

```lisp
(asdf:operate 'asdf:test-op :read-csv)
RUNNING READ-CSV TESTS...
READ-CSV TEST RESULTS:
   Tests: 519
 Success: 519
Failures: 0
```

License 
==========

Read-csv is public domain.
