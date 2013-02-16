#|
  Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
|#


(in-package :cl-user)

(defpackage bptr-plist-asd
  (:use :cl :asdf))

(in-package :bptr-plist-asd)

(defsystem bptr-plist
  :version "0.1"
  :author "Constantin Kulikov"
  :license "GPL v2 or any higher."
  :description "My property list(:key value) functions.
Allow 'empty' keys. Keywords can't be the values of keys."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :serial t
  :components ((:module "src"
                        :components
                        ((:file "bptr-plist")))))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :bptr-plist))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :bptr-plist))))
  (operate 'load-op :bptr-plist-tests)
  (operate 'test-op :bptr-plist-tests))
