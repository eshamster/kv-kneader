(in-package :cl-user)
(defpackage kv-kneader.kv-pair
  (:use :cl
        :anaphora
        :kv-kneader.errors)
  (:import-from :alexandria
                :make-keyword))
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
  (:documentation "This is the similar method to maphash for kv-pair. The 'function' takes 2 arguments; the first is key and the second is value. The order of keys is not guaranteed."))

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

;; --- alist (list) --- ;;

(defmethod init-pairs ((size integer) (base-pairs list))
  nil)

(defmethod find-value-by-key (key (pairs list))
  (aif (cdr (assoc key pairs :test #'string-equal))
       it
       (error 'key-not-found-error :key key)))

(defmethod n-add-pair (key value (pairs list))
  (if (assoc key pairs :test #'string-equal)
      (error 'key-duplication-error :key key)
      (push (cons key value) pairs)))

(defmethod map-pairs (function (pairs list))
  (dolist (pair pairs)
    (funcall function (car pair) (cdr pair))))


;; --- hash-table --- ;;

(defmethod init-pairs ((size integer) (base-pairs hash-table))
  (make-hash-table))

(defmethod find-value-by-key (key (pairs hash-table))
  (multiple-value-bind (value present-p)
      (gethash (make-keyword (string-upcase key)) pairs)
    (if present-p
        value
        (error 'key-not-found-error))))

(defmethod n-add-pair (key value (pair hash-table))
  (let ((mod-key (make-keyword (string-upcase key))))
    (symbol-macrolet ((get-place (gethash mod-key pair)))
      (multiple-value-bind (temp present-p) get-place
        (declare (ignore temp))
        (if present-p
            (error 'key-duplication-error)
            (setf get-place value)))))
  pair)

(defmethod map-pairs (function (pairs hash-table))
  (maphash function pairs))
