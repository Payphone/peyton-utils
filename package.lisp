;;;; package.lisp

(defpackage #:peyton-utils
  (:use #:cl)
  (:export #:aif
           #:asetf
           #:awhen
           #:cat
           #:collect-between
           #:collect-until
           #:compose
           #:flatten
           #:it
           #:octets->integer
           #:octets->string
           #:read-file
           #:read-between
           #:read-until
           #:read-until-not
           #:unread-1
           #:remove-until
           #:with-gensyms
           #:snoc))
