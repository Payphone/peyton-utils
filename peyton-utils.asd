;;;; peyton-utils.asd

(asdf:defsystem #:peyton-utils
  :description "Commonly used utilities."
  :author "Peyton Farrar <peyton@peytonfarrar.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "peyton-utils")))
