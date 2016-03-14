(in-package :cl-user)
(defpackage kv-kneader-test.type-converter
  (:use :cl
        :prove
        :kv-kneader.type-converter))
(in-package :kv-kneader-test.type-converter)

(plan 1)

(subtest
    "Test convert-type"
  (subtest
      "Test string to number"
    (is (convert-type "12" :number) 12)
    (is (convert-type "1.5" :number) 1.5)
    (is (convert-type "5/3" :number) 5/3))
  (subtest
      "Test number to string"
    (is (convert-type 12 :string) "12")
    (is (convert-type 1.5 :string) "1.5")
    (is (convert-type 5/3 :string) "5/3")))

(finalize)
