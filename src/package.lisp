#|
This file is a part of trivialib.type-unify project.
Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage trivialib.type-unify
  (:use :cl :alexandria :trivia :trivia.fail :type-r)
  (:export #:type-unify
           #:type-unify1
           #:type-unification-error))
(in-package :trivialib.type-unify)

;; blah blah blah.

(defpattern every (subpattern)
  (with-gensyms (it)
    `(and (type list)
          (guard ,it
                 (every (lambda-match
                          (,subpattern t))
                        ,it)))))

(defpattern dimension ()
  `(or '* (integer)))
(defpattern dimensions ()
  `(every (dimension)))

;;; type unification
;; 1. Any type variable unifies with any type expression, and is
;; instantiated to that expression. A specific theory might restrict this
;; rule with an occurs check.
;; 
;; 2. Two type constants unify only if they are the same type.
;; 
;; 3. Two type constructions unify only if they are applications of the
;; same type constructor and all of their component types
;; recursively unify.

(defun type-unify (typevars templates types)
  "Unify the type templates against types.
TYPES is a list of type specifiers.
TYPEVARS is a list of symbols.
TEMPLATES is a list of type specifiers, but may also contain
the elements of TYPEVARS somewhere in the tree.

TYPE-UNIFY assumes that the unification is the conjunction of each pair of a template and a type.
If some pair fails to unify, then the whole pairs fail to unify.
Also, if some unification (assignment to type variable) in a given pair
contradicts the other assignment, then the whole pairs fail to unify.

Returns (values result unify-p), where the result is an alist containing the assignment of unification and
unify-p is a boolean indicating if the given template unifies against the given types."
  (ematch* (templates types)
    ((nil nil)
     (values nil t))
    (((list* tmpl tmpl*) (list* type type*))
     (multiple-value-match (type-unify1 typevars tmpl type)
       ((mapping t)
        (multiple-value-match (type-unify typevars tmpl* type*)
          ((mapping* t)
           (merge-mappings-as-and mapping mapping*))))))
    ((_ _)
     (error "Different number of templates and types! ~%~a~%~a" templates types))))

(defun type-unify1 (typevars template type)
  "Unify the type template against a type.
TYPE is a type specifiers.
TYPEVARS is a list of symbols.
TEMPLATE is a type specifiers, but may contain the elements of TYPEVARS somewhere in the tree.

Returns (values result unify-p), where the result is an alist containing the assignment of unification and
unify-p is a boolean indicating if the given template unifies against the given types."
  (let ((template (introspect-environment:typexpand template))
        (type     (introspect-environment:typexpand type)))
    (ematch* (template type)
      ;; typexpand is implementation dependent, and it may not expand some
      ;; types e.g. string -> (array character).
      ;; Therefore we need TYPE-R.
      (((array-subtype) (array-subtype))  ; array, simple-array, ...
       (unify-arrayoid typevars template type))
      (((real-subtype) (real-subtype))    ; mod, unsigned-byte, bignum...
       (unify-numeroid typevars template type))
      (((cons-type car1 cdr1) (cons-type car2 cdr2))
       (multiple-value-match (type-unify1 typevars car1 car2)
         ((mapping1 t)
          (multiple-value-match (type-unify1 typevars cdr1 cdr2)
            ((mapping2 t)
             (multiple-value-match (merge-mappings-as-and mapping1 mapping2)
               ((mapping3 t) (values mapping3 t))))))))
      (((guard typevar (member typevar typevars)) _)
       ;; template is an atomic typevar
       (values (list (cons typevar type)) t))
      (('* _) (values nil t))
      (((symbol) _)
       ;; template is a standard atomic type, and not a typevar
       (if (subtypep type template)
           (values nil t)
           nil))
      ;; compound types in file:///usr/share/doc/hyperspec/Body/04_bc.htm
      (((list* (or 'eql 'member 'satisfies) args) _)
       (when (intersection args typevars)
         (error "Type specifier ~a with type variables are not supported! ~% ~a" args template))
       (if (subtypep type template)
           (values nil t)
           nil))
      (((list* 'or templates) _)
       (let ((some nil))
         (values (reduce (lambda (mapping template)
                           (multiple-value-match (type-unify1 typevars template type)
                             ((mapping2 t)
                              (setf some t)
                              (merge-mappings-as-or mapping mapping2))
                             ((_ nil)
                              mapping)))
                         templates :initial-value nil)
                 some)))
      (((list* 'and templates) _)
       (values (reduce (lambda (mapping template)
                         (multiple-value-match (type-unify1 typevars template type)
                           ((mapping2 t)
                            (multiple-value-match (merge-mappings-as-and mapping mapping2)
                              ((mapping3 t) mapping3)
                              ((_ nil)
                               (return-from type-unify1))))
                           ((_ nil)
                            (return-from type-unify1))))
                       templates :initial-value nil)
               t)))))

(defun remove-larger (sequence partial-order<)
  (let (acc)
    (map nil (lambda (a)
               (let ((flag t))
                 (setf acc (delete-if (lambda (b)
                                        (multiple-value-ematch (funcall partial-order< a b)
                                          ;; indifferent
                                          ((_ t) nil)
                                          ((nil nil) (setf flag nil) nil)
                                          ((_ nil) t)))
                                      acc))
                 (when flag (push a acc))))
         sequence)
    acc))

(defun remove-smaller (sequence partial-order<)
  (let (acc)
    (map nil (lambda (a)
               (let ((flag t))
                 (setf acc (delete-if (lambda (b)
                                        (multiple-value-ematch (funcall partial-order< b a)
                                          ;; indifferent
                                          ((_ t) nil)
                                          ((nil nil) (setf flag nil) nil)
                                          ((_ nil) t)))
                                      acc))
                 (when flag (push a acc))))
         sequence)
    acc))


(defun strict-subtypep-or-indifferent (a b)
  (if (not (subtypep a b))
      (if (not (subtypep b a))
          (values nil t)
          nil)
      t))

(defun merge-mappings-as-or (mapping1 mapping2)
  (let (plist)
    (dolist (pair mapping1)
      (pushnew (cdr pair) (getf plist (car pair)) :test #'equal))
    (dolist (pair mapping2)
      (pushnew (cdr pair) (getf plist (car pair)) :test #'equal))
    (nreverse
     (loop for (typevar typevals) on plist by #'cddr
           ;; note: the one occurring earlier in sequence is discarded
           ;; file:///usr/share/doc/hyperspec/Body/f_rm_dup.htm
           ;; The order of the elements remaining in the result is the same as the order in which they appear in sequence. 
           for %typevals = (remove-if (lambda-match ((or (dimension) (dimensions)) t)) typevals)
           for %dimensions = (remove-if-not (lambda-match ((or (dimension) (dimensions)) t)) typevals)
           for %%typevals = (remove-smaller typevals #'strict-subtypep-or-indifferent)
           collect
           (cons typevar
                 (if %%typevals
                     (if (= (length %%typevals) 1)
                         (car %%typevals)
                         `(or ,@%%typevals))
                     %dimensions))))))

(defun merge-mappings-as-and (mapping1 mapping2)
  (let (plist)
    (dolist (pair mapping1)
      (pushnew (cdr pair) (getf plist (car pair)) :test #'equal))
    (dolist (pair mapping2)
      (pushnew (cdr pair) (getf plist (car pair)) :test #'equal))
    (values
     (nreverse
      (loop for (typevar typevals) on plist by #'cddr
            for %typevals = (remove-if (lambda-match ((or (dimension) (dimensions)) t)) typevals)
            for %dimensions = (remove-if-not (lambda-match ((or (dimension) (dimensions)) t)) typevals)
            for %%typevals = (remove-larger %typevals #'strict-subtypep-or-indifferent)
            for %%dimensions = (handler-case (reduce #'merge-dimensions %dimensions :initial-value '*)
                                 (match-error () (return-from merge-mappings-as-and)))
            ;; a variable should not be a type and a dimension at the same time
            do (when %typevals
                 (when (or %dimensions (subtypep `(and ,@%typevals) nil))
                   (return-from merge-mappings-as-and)))
            collect (cons typevar
                          (if %%typevals
                              (if (= (length %%typevals) 1)
                                  (car %%typevals)
                                  `(and ,@%%typevals))
                              %%dimensions))))
     t)))

(defun unify-numeroid (typevars template type)
  (match* (template type)
    ;; integers (fixnum and bignum are disjoint subtypes of integer)
    (((fixnum-type) (fixnum-type))
     (%unify-numeroid typevars template type))
    (((bignum-type) (bignum-type))
     (%unify-numeroid typevars template type))
    (((integer-type) (integer-subtype))
     (%unify-numeroid typevars template type))
    ;; floats (*-float are NOT disjoint subtypes. However we do not unify them.)
    (((single-float-type) (single-float-type))
     (%unify-numeroid typevars template type))
    (((double-float-type) (double-float-type))
     (%unify-numeroid typevars template type))
    (((long-float-type) (long-float-type))
     (%unify-numeroid typevars template type))
    (((short-float-type) (short-float-type))
     (%unify-numeroid typevars template type))
    (((float-type) (float-subtype))
     (%unify-numeroid typevars template type))
    ;; ratio (integer and ratio are disjoint subtypes of rational)
    (((ratio-type) (ratio-type))
     (%unify-numeroid typevars template type))
    (((rational-type) (rational-subtype))
     (%unify-numeroid typevars template type))
    (((real-type) (real-subtype))
     (%unify-numeroid typevars template type))))

(defun %unify-numeroid (typevars template type)
  (match* (template type)
    (((real-subtype low1 high1) (real-subtype low2 high2))
     (multiple-value-match (unify-dimensions typevars low1 low2)
       ((mapping1 t)
        (multiple-value-match (unify-dimensions typevars high1 high2)
          ((mapping2 t)
           (merge-mappings-as-and mapping1 mapping2))))))))

(defun unify-arrayoid (typevars template type)
  (ematch* (template type)
    (((array-subtype element-type1 dimensions1)
      (array-subtype element-type2 dimensions2))
     (multiple-value-match (type-unify1 typevars element-type1 element-type2)
       ((mapping1 t)
        (multiple-value-match (unify-dimensions typevars dimensions1 dimensions2)
          ((mapping2 t)
           (multiple-value-match (merge-mappings-as-and mapping1 mapping2)
             ((mapping3 t) (values mapping3 t))))))))))

(defun unify-dimensions (typevars dimensions1 dimensions2)
  (match* (dimensions1 dimensions2)
    (('* _) (values nil t))
    (((type integer) (type integer))
     (when (= dimensions1 dimensions2) (values nil t)))
    (((type integer) (type list))
     (when (= dimensions1 (length dimensions2)) (values nil t)))
    (((list* first1 rest1) (list* first2 rest2))
     (multiple-value-match (unify-dimensions typevars first1 first2)
       ((mapping1 t)
        (multiple-value-match (unify-dimensions typevars rest1 rest2)
          ((mapping2 t)
           (merge-mappings-as-and mapping1 mapping2))))))
    (((guard x (member x typevars)) _)
     (values `((,x . ,dimensions2)) t))
    ((nil nil)
     (values nil t))))

(defun merge-dimensions (&optional a b)
  (ematch* (a b)
    (('* _) b)
    ((_ '*) a)
    (((type integer) (and (type integer) (= a))) a)
    (((type integer) (dimensions)) (if (= a (length b)) b (fail)))
    (((dimensions) (type integer)) (if (= b (length a)) a (fail)))
    (((dimensions) (dimensions))   (mapcar #'merge-dimensions a b))))



