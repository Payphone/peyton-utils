;;;; package.lisp

(defpackage #:peyton-utils
  (:use #:cl)
  (:export #:aif
           #:asetf
           #:awhen
           #:cat
           #:compose
           #:flatten
           #:it
           #:octets->integer
           #:octets->string
           #:read-file
           #:read-until
           #:read-until-not
           #:with-gensyms
           #:snoc))
