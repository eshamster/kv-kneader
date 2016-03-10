(in-package :cl-user)
(defpackage kv-kneader.kneader
  (:use :cl)
  (:import-from :alexandria
                :once-only
                :with-gensyms)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :cl-annot.doc
                :doc))
(in-package :kv-kneader.kneader)

(annot:enable-annot-syntax)

(defstruct key-desc
  (key nil)
  (type-option nil)
  (reader-name nil))

(defstruct key-lst-desc
  (key-descs nil)
  (new-name nil))

(defmacro do-options ((var option-lst) &body body)
  (with-gensyms (rest rec)
    `(labels ((,rec (,rest)
                (when ,rest
                  (let ((,var (cadr ,rest)))
                    (case (car ,rest)
                      ,@body
                      (t (error "The key \"~A\" is not defined" (car ,rest)))))
                  (,rec (cddr ,rest)))))
       (unless (evenp (length ,option-lst))
         (error "The length of option-lst should be even")) 
       (,rec ,option-lst))))

(defun check-if-keyword (var)
  (unless (and (symbolp var)
               (string= (package-name (symbol-package var)) "KEYWORD"))
    (error 'type-error :expected-type :keyword :datum var)))

(defun parse-a-key-description (key-description)
  (cond ((atom key-description)
         (make-key-desc :key key-description))
        ((listp key-description)
         (let ((desc (make-key-desc :key (car key-description))))
           (do-options (value (cdr key-description))
             (:type (check-if-keyword value)
                    (setf (key-desc-type-option desc) value))
             (:reader (check-type value symbol)
                      (setf (key-desc-reader-name desc) value)))
           desc))))

#|
Examples.
  (id ...)
  ((id :type fixnum)
  ((id (:new-name :name)) ...)
  ((id age) ...)
  (((id :type fixnum)
    (age :type fixnum)) ...)
  ((id (age :type fixnum) (:new-name "NA"))) ...)
|#
@doc
"keys-description::= key-name | 
                     ({key-description}* [[keys-options]])
key-description::= key-name | (key-name [[options]])
options::= type-option |
           varialble-name-option
keys-options::= {:reader name}
type-option::= (:type type)
variable-name-option::= (:name name)"
(defun parse-keys-description (keys-description) 
  (let ((desc (make-key-lst-desc)))
    (cond ((atom keys-description)
           (push (make-key-desc :key keys-description)
                 (key-lst-desc-key-descs desc)))
          ((listp keys-description)))
    desc))


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
