#|
  This file is a part of trivialib.type-unify project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  unifies a polimorphic type specifier with type variables against actual type specifiers

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#

(defsystem trivialib.type-unify
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:alexandria :trivia :introspect-environment :type-r)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description "unifies a polimorphic type specifier with type variables against actual type specifiers"
  :in-order-to ((test-op (test-op :trivialib.type-unify.test))))
