;;;; cl-callbreak-bot.asd

(asdf:defsystem #:cl-callbreak-bot
  :description "Describe cl-callbreak-bot here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:serapeum #:hunchentoot #:yason #:let-plus #:trees)
  :build-operation "program-op"
  :build-pathname "build/cl-callbreak-bot"
  :entry-point "cl-callbreak-bot::main"
  :components ((:file "package")
               (:file "utils")
               (:file "data-types")
               (:file "generate-random-game")
               (:file "ismcts")
               (:file "cl-callbreak-bot")))
