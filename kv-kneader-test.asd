#|
  This file is a part of kv-kneader project.
  Copyright (c) 2016 eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage kv-kneader-test-asd
  (:use :cl :asdf))
(in-package :kv-kneader-test-asd)

(defsystem kv-kneader-test
  :author "eshamster"
  :license ""
  :depends-on (:kv-kneader
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "kv-kneader"))))
  :description "Test system for kv-kneader"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
