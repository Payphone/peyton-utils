;;;; package.lisp

(defpackage #:peyton-utils
  (:use #:cl #:alexandria)
  (:export #:aif
           #:asetf
           #:awhen
           #:collect
           #:collect-around
           #:collect-between
           #:collect-n
           #:collect-to
           #:collect-until
           #:it
           #:octets->integer
           #:octets->string
           #:read-file
           #:read-between
           #:read-until
           #:read-until-not
           #:unread-1
           #:until
           #:remove-until
           #:snoc))
