![Build Status](https://circleci.com/gh/eshamster/kv-kneader.png?style=shield)

# Kv-Kneader

Kv-Kneader generates new key-value pairs by editting other key-value pairs.

## Usage

### Simple example

```lisp
> (ql:quickload :kv-kneader)
(:KV-KNEADER)
> (defvar base-data '(("a" . 1) (b . "2") (str . "test")))
BASE-DATA
> (kneader:knead base-data ()          ;; (the second argument is options)
    (a (+ a 10))                       ;; search data by the key using string-equal and bind the value
    (((b :type :number)) (+ b 20))     ;; convert the type (now support :number and :string)
    ((a (b :type :number)) (+ a b))    ;; can use multiple keys in the same time
    (((str :reader abc) (:new-name new)) (format nil "str: ~A" abc)))
                                       ;; can change the name of reader
                                       ;; can change the name of key
((A . 11) (B . 22) ("A-B" . 3) (NEW . "str: test"))
```

### Type

Kv-Kneader can use various key-value types. Now, it supports `alist` and `hash-table`. In addition, it provides original structures, `list-pairs`.

Following examples use a sample function, `my-knead`.

```lisp
> (defun my-knead (data &key (base-pairs :default))
    (kneader:knead data (:base-pairs base-pairs)
      (a (+ a 10))
      (((b :type :number)) (+ b 20))
      ((a (b :type :number)) (+ a b))
      (((str :reader abc) (:new-name new)) (format nil "str: ~A" abc))))
MY-KNEAD
```

- `alist`

```lisp
> (print (my-knead '((a . 1) (b . "2") (str . "test"))))
((A . 11) (B . 22) ("A-B" . 3) (NEW . "str: test"))
```

- `hash-table`
  - ***Limitation: a key must be represented as a keyword. (`kneader:push-pair` internally converts a key to a keyword)***

```lisp
> (let ((hash (make-hash-table)))
    (setf (gethash :a hash) 1)
    (setf (gethash :b hash) "2")
    (kneader:push-pair "str" "test" hash)
    (maphash (lambda (k v) (format t "key: ~A, value: ~A~%" k v))
             (my-knead hash)))
key: NEW, value: str: test
key: A-B, value: 3
key: B, value: 22
key: A, value: 11
```

- `list-pairs`

```lisp
> (print (my-knead (kneader:make-lists-pair :header '(a b str)
                                            :values '(1 "2" "test"))))
#S(KV-KNEADER.KV-PAIR:LISTS-PAIR
   :HEADER (A B "A-B" NEW)
   :VALUES (11 22 3 "str: test"))
```

### Type conversion

In default, `kneader:knead` outputs new key-value pairs according to the type of input pairs. It is possible to output another type by using the `:base-pairs` option. Note that this option receives an object instead of a type specifier ((It depends on the implementation of the method `kneader:init-pairs` whether `kneader:knead` re-uses the object)).

Example: alist -> list-pairs

```lisp
> (my-knead '((a . 1) (b . "2") (str . "test"))
            :base-pairs (kneader:make-lists-pair))
#S(KV-KNEADER.KV-PAIR:LISTS-PAIR
   :HEADER (A B "A-B" NEW)
   :VALUES (11 22 3 "str: test"))
```

## Other interfaces to process key-value pairs

Following examples use alist as key-value pairs. But can use other supported types in the same way.

### find-value-by-key

- Find key by string-equal function. If the key is not found, throw `kneader:key-not-found-error`

```lisp
> (defvar data '((a . 1) (b . "2") (str . "test")))
DATA
> (kneader:find-value-by-key :b data)
"2"
> (kneader:find-value-by-key "b" data)
"2"
> (kneader:find-value-by-key "B" data)
"2"
> (kneader:find-value-by-key 'b data)
"2"
```

### push-pair

- Push key & value pair to pairs. If the key is already included in pairs, thorw `kneader:key-duplication-error`

```lisp
> (defvar data '((a . 1) (b . "2") (str . "test")))
DATA
> (kneader:push-pair :c 123 data)
((:C . 123) (A . 1) (B . "2") (STR . "test"))
> data
((:C . 123) (A . 1) (B . "2") (STR . "test"))
```

### map-pairs

- This is the similar method to maphash for kv-pair.

```lisp
> (defvar data '((a . 1) (b . "2") (str . "test")))
DATA
> (kneader:map-pairs (lambda (k v) (format t "key: ~A, value: ~A~%" k v))
                     data)
key: A, value: 1
key: B, value: 2
key: STR, value: test
NIL
```

## To support other types of key-value pairs

To support another key-value pairs type, following methods should be implemented for the type. Please see the `src/kv-pair.lisp` for more detail.

- `find-value-by-key`
- `init-pairs`
- `n-add-pair`
- `map-pairs`

## Installation

This project has not been registered in the quicklisp repository.

1. Clone this repository into a directory in `ql:*local-project-directories*` (If you use [Roswell](https://github.com/snmsts/roswell, the default is "~/.roswell/local-projects/")
2. In REPL, for example, `(ql:quickload :kv-kneader)`

## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2016 eshamster (hamgoostar@gmail.com)

## License

Licensed under the MIT License. 
