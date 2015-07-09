
(in-package :cl-user)
(ql:quickload '(:alexandria :trivia :introspect-environment :type-r))

(uiop:quit (if (handler-case
                   (progn
                     (asdf:load-system :trivialib.type-unify.test)
                     (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run! :trivialib.type-unify))"))
)
                 (serious-condition (c)
                   (describe c)
                   (uiop:quit 2)))
               0 1))


