(asdf:defsystem #:ml
  :description "Machine Learning Library for Common Lisp"
  :author "Bibek Panthi <bpanthi977@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:serapeum
                            (:version #:einsum "0.0.2"))
  :components ((:file "package")
               (:file "utils")
               (:file "tensor")
               (:file "adam")
               (:file "environments/environment")
               (:file "environments/cart-pole")
               (:file "environments/cart-pole-classic")))
