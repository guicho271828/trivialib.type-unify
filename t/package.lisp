#|
This file is a part of trivialib.type-unify project.
Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :trivialib.type-unify.test
  (:use :cl
        :trivialib.type-unify
        :fiveam :alexandria :trivia))
(in-package :trivialib.type-unify.test)

(defun unification-tuple-equal (a b)
  (print (list a b))
  (ematch* (a b)
    (((cons a-sym a-type)
      (cons b-sym b-type))
     (and (eq a-sym b-sym)
          (handler-case (type= a-type b-type)
            ;; not a type specifier, but a fragment of type specifier, e.g.
            ;; (1 *) in (array fixnum (1 *))
            (error ()
              (equalp a-type b-type)))))))

(defmacro unify-empty (&body body)
  `(multiple-value-bind (unification unified?)
       (progn ,@body)
     (is-false unification)
     (is-true unified?)))

(defmacro unify-fail (&body body)
  `(multiple-value-bind (unification unified?)
       (progn ,@body)
     (is-false unification)
     (is-false unified?)))

(defmacro unify-with (assignment &body body)
  `(multiple-value-bind (unification unified?)
       (progn ,@body)
     (is (set-equal unification ,assignment :test #'unification-tuple-equal)
         "Failed ~A~% expected:~%  ~a~% result:~%  ~a" ',(car body) ,assignment unification)
     (is-true unified?)))

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
                      (_ (values nil t)))))
                 :test #'equal))

  (is (set-equal '(5 "c")
                 (trivialib.type-unify::remove-smaller
                  '(2 5 "aa" 1 "c" 3 4)
                  (lambda (a b)
                    (ematch* (a b)
                      (((integer) (integer)) (< a b))
                      (((type string) (type string)) (string< a b))
                      (_ (values nil t)))))
                 :test #'equal)))

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
  (unify-with '((a . fixnum))
    (type-unify1 '(a) 'a 'fixnum))
  
  (unify-with '((a . fixnum))
    (type-unify1 '(a) '(or float a) 'fixnum))
  
  (unify-empty
   (type-unify1 '(a) '(or float fixnum) 'fixnum))

  (unify-fail
   (type-unify1 '(a) 'float 'fixnum))

  (unify-with '((a . simple-string))
    (type-unify1 '(a)
                 '(and simple-array a)
                 'simple-string))

  (unify-with '((a . character))
    (type-unify1 '(a)
                 '(array a *)
                 '(array character (3))))

  (unify-with '((a . character) (b . (1 *)))
    (type-unify1 '(a b)
                 '(array a b)
                 '(array character (1 *))))

  (unify-with '((a . character) (b . 4))
    (type-unify1 '(a b)
                 '(array a (* b))
                 '(array character (3 4))))

  (unify-fail
   (type-unify1 '(a)
                '(array a (* 2))
                '(array character (3 4))))

  ;; unification within template
  (unify-with '((a . 3))
    (type-unify1 '(a)
                 '(array * (a a))
                 '(array character (3 3))))

  (unify-fail
   (type-unify1 '(a)
                '(array * (a a))
                '(array character (3 4))))


  (unify-with `((a . ,MOST-POSITIVE-FIXNUM))
    (type-unify1 '(a) '(integer * a) 'fixnum))

  (unify-with '((a . 31))
    (type-unify1 '(a) '(integer * a) '(unsigned-byte 5)))
  
  (unify-empty
    (type-unify1 '() `(integer ,most-negative-fixnum ,most-positive-fixnum) 'FIXNUM))

  (unify-empty
    (type-unify1 '() `(float * *) 'float)))



(test cons
  (unify-with '((A . FIXNUM) (B . STRING))
    (type-unify1 '(a b)
                 '(cons (cons a b) (cons b a))
                 '(cons (cons fixnum string) (cons string fixnum)))))


(test deftype
  (deftype cons2 (x) `(cons ,x ,x))

  (unify-with '((a . fixnum))
    (type-unify1 '(a)
                 '(cons2 (cons2 a))
                 '(cons (cons fixnum fixnum) (cons fixnum fixnum))))
  (unify-with '((a . fixnum))
    (type-unify1 '(a b)
                 '(cons (cons a a) (cons a a))
                 '(cons2 (cons2 fixnum)))))


(test type-unify
  (unify-with '((a . fixnum)) (type-unify '(a) '(a) '(fixnum)))
  (unify-with '((a . fixnum)) (type-unify '(a) '(a a) '(fixnum fixnum)))
  (unify-with '((a . fixnum)) (type-unify '(a) '(a a) '(fixnum integer)))
  (unify-empty (type-unify '()  '(fixnum) '(fixnum)))
  (unify-empty (type-unify '()  '(number) '(fixnum)))
  (unify-fail (type-unify '()  '(fixnum) '(number)))

  (unify-fail (type-unify '(a) '(a a) '(fixnum float)))
  (unify-fail (type-unify '(a) '(a a) '(complex real)))   ; -> nil, nil
  (unify-fail (type-unify '(a) '(a a) '(rational float))) ; -> nil, nil
  (unify-fail (type-unify '(a) '(a a) '(integer ratio)))  ; -> nil, nil
  (unify-fail (type-unify '(a) '(a a) '((array * 6) (array * 7))))        ; -> nil, nil
  (unify-fail (type-unify '(a) '(a a) '((array * 6) (array * (* *)))))    ; -> nil, nil
  (unify-fail (type-unify '(a) '(a a) '((array * (* *)) (array * 6))))    ; -> nil, nil
  (unify-fail (type-unify '(a) '(a a) '((array * (* * *)) (array * (* *)))))    ; -> nil, nil
  (unify-fail (type-unify '(a) '(a a) '((array * (* *)) (array * (* * *)))))    ; -> nil, nil
  (unify-fail (type-unify '(a) '(a a) '((array base-char) (array extended-char)))) ; -> nil, nil
  (unify-fail (type-unify '(a) '(a a) '((array extended-char) (array base-char))))

  ;; implimentation specified feature
  (if (eq (upgraded-array-element-type 'character)
          (upgraded-array-element-type 'extended-char))
      (unify-with '((a . (array character)))
        (type-unify '(a) '(a a) '((array character) (array extended-char))))
      (unify-fail
        (type-unify '(a) '(a a) '((array character) (array extended-char)))))
  (if (eq (upgraded-array-element-type 'character)
          (upgraded-array-element-type 'base-char))
      (unify-with '((a . (array character)))
        (type-unify '(a) '(a a) '((array character) (array base-char))))
      (unify-fail
        (type-unify '(a) '(a a) '((array character) (array base-char)))))

  (unify-with '((a . (and (array * 6) (array character *))))
    (type-unify '(a)
                '(a a)
                '((array * 6) (array character *))))
  (unify-with '((a . fixnum) (b . float))
    (type-unify '(a b) '(a b) '(fixnum float))))




