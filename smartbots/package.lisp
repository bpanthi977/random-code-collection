;;;; package.lisp

(uiop:define-package #:cl-callbreak-bot.utils
  (:use #:cl #:alexandria #:serapeum #:let-plus)
  (:reexport #:alexandria
             #:serapeum
             #:let-plus))

(defpackage #:cl-callbreak-bot
  (:use #:cl)
  (:local-nicknames (#:u #:cl-callbreak-bot.utils)
                    (#:h #:hunchentoot)
                    ))


