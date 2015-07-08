#|
  This file is a part of trivialib.type-unify project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :trivialib.type-unify.test
  (:use :cl
        :trivialib.type-unify
        :fiveam
        :iterate :alexandria :trivia))
(in-package :trivialib.type-unify.test)



(def-suite :trivialib.type-unify)
(in-suite :trivialib.type-unify)

;; run test with (run! test-name) 

(test remove-larger/smaller
  (is (equal '(1) (trivialib.type-unify::remove-larger '(1 5 2 3 4) #'<)))

  (is (set-equal '(1 "aa")
                 (trivialib.type-unify::remove-larger
                  '(2 5 "aa" 1 "c" 3 4)
                  (lambda (a b)
                    (ematch* (a b)
                      (((integer) (integer)) (< a b))
                      (((type string) (type string)) (string< a b))
                      (_ (values nil t)))))))

  (is (set-equal '(5 "c")
                 (trivialib.type-unify::remove-smaller
                  '(2 5 "aa" 1 "c" 3 4)
                  (lambda (a b)
                    (ematch* (a b)
                      (((integer) (integer)) (< a b))
                      (((type string) (type string)) (string< a b))
                      (_ (values nil t))))))))

(test trivialib.type-unify::merge-mappings-as-or
  (is (equal '((a . fixnum) (b . fixnum))
             (trivialib.type-unify::merge-mappings-as-or
              '((a . fixnum)) '((b . fixnum)))))

  (is (equal '((a . fixnum))
             (trivialib.type-unify::merge-mappings-as-or
              '((a . fixnum)) '((a . fixnum)))))
  
  (is (equal '((a . integer))
             (trivialib.type-unify::merge-mappings-as-or
              '((a . fixnum)) '((a . integer)))))
  (is (equal '((a . (or fixnum character)))
             (trivialib.type-unify::merge-mappings-as-or
              '((a . fixnum)) '((a . character))))))

(test trivialib.type-unify::merge-mappings-as-and
  (is (equal '((a . fixnum) (b . fixnum))
             (trivialib.type-unify::merge-mappings-as-and
              '((a . fixnum)) '((b . fixnum)))))

  (is (equal '((a . fixnum))
             (trivialib.type-unify::merge-mappings-as-and
              '((a . fixnum)) '((a . fixnum)))))
  
  (is (equal '((a . fixnum))
             (trivialib.type-unify::merge-mappings-as-and
              '((a . fixnum)) '((a . integer)))))
  (is-false (trivialib.type-unify::merge-mappings-as-and
             '((a . fixnum)) '((a . character)))))

(test type-unify1
  (is (equal '((a . fixnum))
             (type-unify1 '(a) 'a 'fixnum)))
  
  (is (equal '((a . fixnum))
             (type-unify1 '(a) '(or float a) 'fixnum)))
  
  (is (equal nil
             (type-unify1 '(a) '(or float fixnum) 'fixnum)))

  (is-false
   (nth-value 1 (type-unify1 '(a) 'float 'fixnum)))

  (is (equal '((a . simple-string))
             (type-unify1 '(a)
                          '(and simple-array a)
                          'simple-string)))
  
  (is (equal '((a . char))
             (type-unify1 '(a)
                          '(array a *)
                          '(array char (3)))))

  (is (equal '((a . char) (b . (1 *)))
             (type-unify1 '(a b)
                          '(array a b)
                          '(array char (1 *)))))

  (is (equal '((a . char) (b . 4))
             (type-unify1 '(a b)
                          '(array a (* b))
                          '(array char (3 4)))))
  
  (is-false
   (type-unify1 '(a)
                '(array a (* 2))
                '(array char (3 4)))))

(test type-unify
  (is (equal '((a . fixnum)) (type-unify '(a) '(a) '(fixnum))))
  (is (equal '((a . fixnum)) (type-unify '(a) '(a a) '(fixnum fixnum))))
  (is (equal '((a . fixnum)) (type-unify '(a) '(a a) '(fixnum integer))))
  (is (equal '((a . (and (array * 6) (array char *))))
             (type-unify '(a) '(a a) '((array * 6) (array char *)))))
  (is-false
   (type-unify '(a) '(a a) '(fixnum float)))
  (is-false
   (nth-value 1 (type-unify '(a) '(a a) '(fixnum float))))
  (is-false
   (type-unify '(a) '(a b) '(fixnum float)))
  (is-false
   (nth-value 1 (type-unify '(a) '(a b) '(fixnum float))))
  (is (equal '((a . fixnum) (b . float))
             (type-unify '(a b) '(a b) '(fixnum float)))))




