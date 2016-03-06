(in-package :cl-user)
(defpackage kv-kneader.type-converter
  (:use :cl)
  (:import-from :parse-number
                :parse-number))
(in-package :kv-kneader.type-converter)

(annot:enable-annot-syntax)

@export
(defgeneric convert-type (data target-type))

(defmethod convert-type ((data string) (target-type (eql :number)))
  (parse-number data))

(defmethod convert-type ((data number) (target-type (eql :string)))
  (format nil "~A" data))
