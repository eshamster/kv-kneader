(in-package :cl-user)
(defpackage kv-kneader-test.kneader
  (:use :cl
        :prove
        :kv-kneader))
(in-package :kv-kneader-test.kneader)

(plan 2)

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
              'simple-error))
  (subtest
      "Test parsers"
    (subtest
        "Test parse-a-key-description"
      (is ($:parse-a-key-description nil)
          #S($:KEY-DESC :KEY NIL :TYPE-OPTION NIL :READER-NAME NIL)
          :test #'equalp)
      (is ($:parse-a-key-description '(test :type :number :reader ab))
          #S($:KEY-DESC :KEY TEST :TYPE-OPTION :NUMBER :READER-NAME AB)
          :test #'equalp)
      (is-error ($:parse-a-key-description '(test :type number))
                'type-error)
      (is-error ($:parse-a-key-description '(test :reader 'ab))
                'type-error)
      (is-error ($:parse-a-key-description '(test :wrong-opt -1))
                'simple-error))
    (subtest
        "Test parse-keys-options"
      (let ((desc ($:make-key-lst-desc)))
        (is-error ($:parse-keys-options '(:wrong-opt -1) desc)
                  'simple-error)
        (is-error ($:parse-keys-options '(:new-name (a b)) desc)
                  'type-error)
        (ok ($:parse-keys-options '(:new-name "ABC") desc))
        (is ($:key-lst-desc-new-name desc) "ABC")))
    (subtest
        "Test parse-keys-descriptions (easy test)"
      (labels ((prove-length (desc expected-length)
                 (is (length ($:key-lst-desc-key-descs desc))
                     expected-length)))
        (prove-length ($:parse-keys-descriptions 'abc) 1)
        (prove-length ($:parse-keys-descriptions '(abc def)) 2)
        (prove-length ($:parse-keys-descriptions '(abc (def :type :number))) 2)
        (let ((desc ($:parse-keys-descriptions '(abc (def :type :number) (:new-name new)))))
          (prove-length desc 2)
          (is ($:key-lst-desc-new-name desc) 'new)))))
  (subtest
      "Test auxiliary functions to make list"
    (subtest
        "Test make-lambda-for-processing-values"
      (macrolet ((test (x y z)
                   `(,($:make-lambda-for-processing-values
                       ($:parse-keys-descriptions '(ab (cd :reader cde) fg))
                       '((list fg cde ab)))
                      ,x ,y ,z)))
        (is (test 1 2 3) '(3 2 1) :test #'equal))
      (macrolet ((test (x)
                   `(,($:make-lambda-for-processing-values
                       ($:parse-keys-descriptions '(ab))
                       '())
                      ,x)))
        (is (test 100) 100)))
    (subtest
        "Test make-extracting-arg-values"
      (is ($:make-extracting-arg-values ($:parse-keys-descriptions
                                         '(a ("b" :reader test) (c :type :number)))
                                        'temp)
          '((KV-KNEADER.KV-PAIR:FIND-VALUE-BY-KEY 'a TEMP)
            (KV-KNEADER.KV-PAIR:FIND-VALUE-BY-KEY "b" TEMP)
            (KV-KNEADER.TYPE-CONVERTER:CONVERT-TYPE
             (KV-KNEADER.KV-PAIR:FIND-VALUE-BY-KEY 'c TEMP) :NUMBER))
          :test #'equal))
    (subtest
        "Test make-new-name"
      (let ((lst-desc '("Abc" (b :reader test) c (:new-name "New"))))
        (is ($:make-new-name ($:parse-keys-descriptions '(a))) '(quote a))
        (is ($:make-new-name ($:parse-keys-descriptions lst-desc)) "New")
        (is ($:make-new-name ($:parse-keys-descriptions (butlast lst-desc))) "Abc-TEST-C")
        (is ($:make-new-name ($:parse-keys-descriptions (list (car lst-desc)))) "Abc")))))

(subtest
    "Test knead"
  (let ((pairs (make-lists-pair :header '(ab "cd" ef)
                                :values '(1 "2" 3))))
    (is (knead pairs
          (ab)
          ((ab ef) (+ ab ef))
          (((ab :reader test-reader) (:new-name "test-reader")) (* 100 test-reader))
          ((ab (cd :type :number) (:new-name "test-converter")) (+ ab cd)))
        #S(LISTS-PAIR
           :HEADER (AB "AB-EF" "test-reader" "test-converter")
           :VALUES (1 4 100 3))
        :test #'equalp) 
    (is-error (knead pairs
                ((ab not-found) (declare (ignore ab not-found)) 1000))
              'key-not-found-error)
    (is-error (knead pairs
                (ab)
                (ab))
              'key-duplication-error)))

(finalize)
