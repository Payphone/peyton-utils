;;;; package.lisp

(defpackage #:peyton-utils
  (:use #:cl)
  (:export #:aif
           #:asetf
           #:cat
           #:compose
           #:octets->integer
           #:read-until
           #:read-until-not))
