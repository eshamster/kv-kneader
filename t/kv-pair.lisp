(in-package :cl-user)
(defpackage kv-kneader-test.kv-pair
  (:use :cl
        :prove
        :kv-kneader))
(in-package :kv-kneader-test.kv-pair)

(plan 1)

(defun prove-pair-methods (type empty-pairs)
  (let ((pairs (init-pairs 2 empty-pairs)))
    (ok (typep pairs type))
    (push-pair 'test1 100 pairs)
    (push-pair "TeST2" 200 pairs)
    (is-error (push-pair 'test2 300 pairs)
              'key-duplication-error)
    (is (find-value-by-key "TEST1" pairs)
        100)
    (is (find-value-by-key 'test1 pairs)
        100)
    (is (find-value-by-key 'test2 pairs)
        200)
    (is-error (find-value-by-key 'not-registered pairs)
              'key-not-found-error)))

(subtest
    "Test base methods for each type"
  (subtest
      "Test list-pair"
    (prove-pair-methods 'lists-pair (make-lists-pair))))

(finalize)
