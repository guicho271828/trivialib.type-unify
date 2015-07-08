#|
  This file is a part of trivialib.type-unify project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage trivialib.type-unify.test-asd
  (:use :cl :asdf))
(in-package :trivialib.type-unify.test-asd)


(defsystem trivialib.type-unify.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of trivialib.type-unify"
  :license "LLGPL"
  :depends-on (:trivialib.type-unify
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (load-op :after (op c) (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run! :trivialib.type-unify))"))
))
