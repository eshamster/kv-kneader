(in-package :cl-user)
(defpackage kv-kneader.errors
  (:use :cl))
(in-package :kv-kneader.errors)

(annot:enable-annot-syntax)

@export
(define-condition kv-kneader-error (error) ())

@export
(define-condition key-not-found-error (kv-kneader-error)
  ((key :initarg :key
        :reader key-not-found-key))
  (:report (lambda (condition stream)
             (format stream "The key \"~A\" is not found"
                     (key-not-found-key condition)))))

@export
(define-condition key-duplication-error (kv-kneader-error)
  ((key :initarg key
        :reader key-duplication-key))
  (:report (lambda (condition stream)
             (format stream "The key \"~A\" is duplicated"
                     (key-duplication-key condition)))))
