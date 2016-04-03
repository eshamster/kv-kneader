(in-package :cl-user)
(defpackage kv-kneader-test.test-kneader
  (:use :cl
        :kv-kneader)
  (:export :use-kneaded-symbol
           :test-use-kneaded-symbol))
(in-package :kv-kneader-test.test-kneader)

(defmacro use-kneaded-symbol (body-a body-c)
  `(knead '((a . 10) (b . 20)) ()
     ((a (:new-name "A")) ,body-a)
     (((b :reader c) (:new-name "C")) ,body-c)))

;; In naive implementation, 'a' is 'kv-kneader-test.test-kneader::a' (bound)
;; 'c' is the same as 'a'.
(defun test-use-kneaded-symbol ()
  (use-kneaded-symbol (* a 2)
                      (* c 2)))
