(in-package :cl-user)

(defpackage bptr-plist-tests-asd
  (:use :cl :asdf))

(in-package :bptr-plist-tests-asd)

(defsystem bptr-plist-tests
  :depends-on (:bptr-tests-from-docstrings :bptr-plist)
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "bptr-plist") (:file "bptr-plist-tests")))))

(defmethod operation-done-p
    ((o test-op) (c (eql (find-system :bptr-plist-tests))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :bptr-plist-tests))))
  (load-system :bptr-plist-tests)
  (flet ((run-tests (&rest args)
           (apply (intern (string '#:retest-plist-tests) '#:bptr-plist-tests) args)))
    (run-tests)))
