(in-package :cl-user)

(defpackage :bptr-plist-tests
  (:use :cl :bptr-plist :bptr-tests-from-docstrings)
  (:export #:retest-plist-tests))

(in-package :bptr-plist-tests)

(defun retest-plist-tests ()
  (let ((plist-tests (make-empty-testsdb)))
    (make-tests-for-package :bptr-plist :testsdb plist-tests)
    (format *error-output* "~s" (with-output-to-string (ostr)
                                  (run-testsdb :testsdb plist-tests :ostream ostr)))))
