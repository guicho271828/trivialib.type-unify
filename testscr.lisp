
(in-package :cl-user)

(uiop:quit (if (handler-case
                   (progn
                     (ql:quickload :trivialib.type-unify.test)
                     (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run! :trivialib.type-unify))"))
)
                 (serious-condition (c)
                   (describe c)
                   (uiop:quit 2)))
               0 1))


