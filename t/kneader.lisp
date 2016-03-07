(in-package :cl-user)
(defpackage kv-kneader-test.kneader
  (:use :cl
        :prove
        :kv-kneader))
(in-package :kv-kneader-test.kneader)

(plan 1)

(subtest
    "Test some internals"
  (subtest
      "Test do-options"
    (let ((opts '(:a 1 :c 2))
          (temp 0))
      ($:do-options (value opts)
        (:a (incf temp value))
        (:b (incf temp (* value 10)))
        (:c (incf temp (* value 100))))
      (is temp 201))
    (is-error ($:do-options (value '(:a 1 :b))
                (:a (print value))
                (:b (print 'b)))
              'simple-error)
    (is-error ($:do-options (value '(:a 1 :not-option 100))
                (:a (print value)))
              'simple-error)))

(finalize)
