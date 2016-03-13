(in-package :cl-user)
(defpackage kv-kneader.kneader
  (:use :cl
        :anaphora)
  (:import-from :kv-kneader.type-converter
                :convert-type)
  (:import-from :kv-kneader.kv-pair
                :find-value-by-key
                :push-pair
                :init-pairs)
  (:import-from :alexandria
                :once-only
                :with-gensyms)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :cl-annot.doc
                :doc))
(in-package :kv-kneader.kneader)

(annot:enable-annot-syntax)

;; ----- keys parser ----- ;;

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

(defun is-keyword (var)
  (and (symbolp var)
       (string= (package-name (symbol-package var)) "KEYWORD")))

(defun check-if-keyword (var)
  (unless (is-keyword var)
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

(defun parse-keys-options (keys-options key-lst-desc)
  (do-options (value keys-options)
    (:new-name (unless (atom value)
                 (error 'type-error :expected-type :atom :datum value))
               (setf (key-lst-desc-new-name key-lst-desc) value)))
  key-lst-desc)

(defun is-keys-options (target)
  (and (listp target)
       (is-keyword (car target))))

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
(defun parse-keys-descriptions (keys-description)
  (labels ((push-key-desc (desc lst-desc)
             (push desc (key-lst-desc-key-descs lst-desc)))
           (push-atom-key (sym desc)
             (push-key-desc (make-key-desc :key sym) desc)))
    (let ((desc (make-key-lst-desc)))
      (if (atom keys-description)
          (push-atom-key keys-description desc) 
          (dolist (a-key-description keys-description)
            (if (atom a-key-description)
                (push-atom-key a-key-description desc)
                (if (is-keyword (car a-key-description))
                    (parse-keys-options a-key-description desc)
                    (push-key-desc (parse-a-key-description a-key-description)
                                   desc)))))
      (slet (key-lst-desc-key-descs desc)
        (setf it (nreverse it)))
      desc)))

;; ----- auxiliary functions to make list ----- ;;

(defun make-arg-name (a-key-desc)
  (with-slots (key reader-name) a-key-desc
    (if reader-name reader-name key)))

(defun make-lambda-for-processing-values (key-lst-desc body-lst)
  (let ((desc-lst (key-lst-desc-key-descs key-lst-desc)))
    `(lambda ,(mapcar (lambda (desc)
                        (make-arg-name desc))
                      desc-lst)
       ;; In this if codition, simply return the argument
       ,@(if (and (null body-lst)
                  (= (length desc-lst) 1))
             (list (make-arg-name (car desc-lst)))
             body-lst))))

(defun modify-key (key)
  (if (symbolp key) (list 'quote key) key))

(defun make-extracting-arg-values (key-lst-desc pairs)
  (labels ((make-finder (key pairs)
             `(find-value-by-key ,(modify-key key) ,pairs)))
    (mapcar (lambda (desc)
              (with-slots (key type-option) desc
                (if type-option
                    `(convert-type ,(make-finder key pairs)
                                   ,type-option)
                    (make-finder key pairs))))
            (key-lst-desc-key-descs key-lst-desc))))

(defun join-name (key-descs)
  (labels ((rec (result rest)
             (if rest
                 (let ((name (make-arg-name (car rest))))
                   (if (string= result "")
                       (rec name (cdr rest))
                       (rec (format nil "~A-~A" result name)
                            (cdr rest))))
                 result)))
    (rec "" key-descs)))

(defun make-new-name (key-lst-desc)
  (modify-key
   (aif (key-lst-desc-new-name key-lst-desc)
        it
        (join-name (key-lst-desc-key-descs key-lst-desc)))))
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
