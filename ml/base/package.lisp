(uiop:define-package #:ml/utils
    (:use #:cl #:serapeum #:alexandria)
  (:reexport
   #:serapeum
   #:alexandria))

(defpackage #:ml
  (:use #:cl))

(defpackage #:ml/tensor
  (:use #:cl #:einsum)
  (:local-nicknames (#:u #:ml/utils)))

(defpackage #:ml/optimizer
  (:use #:cl)
  (:local-nicknames (#:t #:ml/tensor)
                    (#:u #:ml/utils))
  (:export
   #:adam))

(defpackage #:ml/environment
  (:use #:cl)
  (:local-nicknames (#:u #:ml/utils))
  (:export
   ;; environment
   #:defstate
   #:initialize
   #:take-action
   #:terminated?
   ;; integrator
   #:rk4
   #:euler-forward
   ;; samplers
   #:sampler/n-steps-return))
