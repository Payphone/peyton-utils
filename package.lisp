;;;; package.lisp

(defpackage #:peyton-utils
  (:use #:cl)
  (:export #:aif
           #:asetf
           #:cat
           #:compose
           #:it
           #:octets->integer
           #:octets->string
           #:read-until
           #:read-until-not
           #:with-gensyms
           #:snoc))
