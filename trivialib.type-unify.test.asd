#|
  This file is a part of trivialib.type-unify project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

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
  :perform (test-op (op c) (eval (read-from-string "(5am:run! :trivialib.type-unify)"))))
