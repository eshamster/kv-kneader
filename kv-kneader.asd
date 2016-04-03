#|
  This file is a part of kv-kneader project.
  Copyright (c) 2016 eshamster (hamgoostar@gmail.com)
|#

#|
  Author: eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage kv-kneader-asd
  (:use :cl :asdf))
(in-package :kv-kneader-asd)

(defsystem kv-kneader
  :version "0.1"
  :author "eshamster"
  :license "MIT"
  :depends-on (:alexandria
               :anaphora
               :cl-annot
               :parse-number)
  :components ((:module "src"
                :serial t
                :components
                ((:file "errors")
                 (:file "type-converter")
                 (:file "kv-pair")
                 (:file "kneader")
                 (:file "kv-kneader"))))
  :description "Kv-Kneader generates new key-value pairs by editting other key-value pairs."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op kv-kneader-test))))
