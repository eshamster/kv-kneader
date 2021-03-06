(in-package :cl-user)
(defpackage kv-kneader
  (:nicknames :kneader)
  (:import-from :kv-kneader.errors
                :kv-kneader-error
                :key-not-found-error
                :key-duplication-error)
  
  (:import-from :kv-kneader.kv-pair
                :find-value-by-key
                :init-pairs
                :n-add-pair
                :push-pair
                :map-pairs
                
                :lists-pair
                :make-lists-pair
                :lists-pair-header
                :lists-pair-values)

  (:import-from :kv-kneader.kneader
                :knead)
  
  (:export :kv-kneader-error
           :key-not-found-error
           :key-duplication-error

           :find-value-by-key
           :init-pairs
           :n-add-pair
           :push-pair 
           :map-pairs
           
           :lists-pair
           :make-lists-pair
           :lists-pair-header
           :lists-pair-values

           :knead))
