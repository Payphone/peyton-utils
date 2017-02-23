;;;; package.lisp

(defpackage #:peyton-utils
  (:use #:cl #:alexandria)
  (:export #:accumulate
           #:aif
           #:asetf
           #:awhen
           #:collect
           #:collect-around
           #:collect-between
           #:collect-n
           #:collect-to
           #:collect-until
           #:elt0
           #:internalize-symbols
           #:it
           #:last1
           #:metalist
           #:mkstr
           #:octets->integer
           #:octets->string
           #:read-file
           #:read-between
           #:read-until
           #:read-until-not
           #:unread-1
           #:until
           #:remove-until
           #:split-string
           #:symb
           #:value
           #:snoc))
