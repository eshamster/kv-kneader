(in-package :cl-user)
(defpackage kv-kneader.kneader
  (:use :cl)
  (:import-from :alexandria
                :once-only
                :with-gensyms))
(in-package :kv-kneader.kneader)

(defstruct key-desc
  (type-option nil))

(defun parse-key-description ()
  "({key-description}*)
key-description::= key-name | (key-name [[options]])
options::= type-option |
           varialble-name-option
type-option::= (:type type)
variable-name-option::= (:name name)")


#|
(defmacro convert-raw-data-one-line (head-line data-line &body body)
  (once-only (head-line data-line)
    (with-gensyms (new-header)
      `(let ((,new-header))
         (values (list ,@(mapcar
                          (lambda (process)
                            (let* ((names (car process))
                                   (arg-lst (if (listp names)
                                                (mapcar (lambda (str)
                                                          (intern (string-upcase str)))
                                                        names)
                                                '(it)))
                                   (find-lst (if (listp names)
                                                 (car process)
                                                 (list names))))
                              `((lambda (,@arg-lst)
                                  (setf ,new-header
                                        (add-to-new-header ',names ,new-header))
                                  ,@(if (cdr process)
                                        (re-intern-symbols (cdr process) arg-lst)
                                        arg-lst))
                                ,@(mapcar
                                   (lambda (arg) 
                                     `(find-target-value ,arg ,head-line ,data-line))
                                   find-lst))))
                          body))
                 (reverse ,new-header))))))
|#
