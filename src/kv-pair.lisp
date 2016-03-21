(in-package :cl-user)
(defpackage kv-kneader.kv-pair
  (:use :cl
        :anaphora
        :kv-kneader.errors))
(in-package :kv-kneader.kv-pair)

(annot:enable-annot-syntax)

@export
(defgeneric find-value-by-key (key pairs)
  (:documentation "Find key by string-equal function. If the key is not found, throw key-not-found-error"))

@export
(defgeneric init-pairs (size base-pairs)
  (:documentation "Make an empty pairs whose type is same to the base-pairs. The size indicates guaranteed value not limit."))

@export
(defmacro push-pair (key value pairs)
  "Push key & value pair to pairs. If the key is already included in pairs, thorw key-duplication-error"
  `(setf ,pairs (n-add-pair ,key ,value ,pairs)))

@export
(defgeneric n-add-pair (key value pairs)
  (:documentation "Return pairs with new key & value. If the key is already included in pairs, thorw key-duplication-error. The argument 'pairs' might be destroyed."))

@export
(defgeneric map-pairs (function pairs)
  (:documentation "This is similar function to maphash for kv-pair. The 'function' takes 2 arguments; the first is key and the second is value. The order of keys is not guaranteed."))

;; --- lists-pair --- ;;

@export
@cl-annot.class:export-structure
(defstruct lists-pair
  (header nil :type list)
  (values nil :type list))

(defmethod init-pairs ((size integer) (base-pairs lists-pair))
  (make-lists-pair))

(defmethod find-value-by-key (key (pairs lists-pair))
  (aif (position key (lists-pair-header pairs) :test #'string-equal)
       (nth it (lists-pair-values pairs))
       (error 'key-not-found-error :key key)))

(defmethod n-add-pair (key value (pairs lists-pair))
  (if (find key (lists-pair-header pairs) :test #'string-equal)
      (error 'key-duplication-error :key key)
      (progn (push key (lists-pair-header pairs))
             (push value (lists-pair-values pairs))))
  pairs)

(defmethod map-pairs (function (pairs lists-pair))
  (loop
     for key in (lists-pair-header pairs)
     for value in (lists-pair-values pairs)
       do (funcall function key value)))

