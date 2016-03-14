(in-package :cl-user)
(defpackage kv-kneader-test.test-utils
  (:use :cl)
  (:import-from :cl-ppcre
                :regex-replace))
(in-package :kv-kneader-test.test-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (make-dispatch-macro-character #\$)
  (set-dispatch-macro-character
   #\$ #\:
   #'(lambda (stream &rest rest)
       (declare (ignore rest))
       (intern (symbol-name (read stream nil))
               (regex-replace "-TEST" (package-name *package*) "")))))
