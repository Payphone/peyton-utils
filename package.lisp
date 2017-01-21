;;;; package.lisp

(defpackage #:peyton-utils
  (:use #:cl)
  (:export #:aif
           #:asetf
           #:cat
           #:compose
           #:it
           #:octets->integer
           #:read-until
           #:read-until-not))
