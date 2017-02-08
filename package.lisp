;;;; package.lisp

(defpackage #:peyton-utils
  (:use #:cl)
  (:export #:accumulate
           #:aif
           #:asetf
           #:awhen
           #:cat
           #:collect
           #:collect-between
           #:collect-n
           #:collect-to
           #:collect-until
           #:compose
           #:elt0
           #:flatten
           #:it
           #:last1
           #:octets->integer
           #:octets->string
           #:read-file
           #:read-between
           #:read-until
           #:read-until-not
           #:unread-1
           #:until
           #:remove-until
           #:with-gensyms
           #:snoc))
