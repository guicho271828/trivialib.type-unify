#|
  This file is a part of trivialib.type-unify project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  unifies a polimorphic type specifier with type variables against actual type specifiers

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage trivialib.type-unify-asd
  (:use :cl :asdf))
(in-package :trivialib.type-unify-asd)


(defsystem trivialib.type-unify
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:iterate :alexandria :trivia)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description "unifies a polimorphic type specifier with type variables against actual type specifiers"
  :in-order-to ((test-op (load-op :trivialib.type-unify.test))))
