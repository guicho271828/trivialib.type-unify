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
  (let ((type (introspect-environment:typexpand type))
        (template (introspect-environment:typexpand template)))
    (if (atom type)
        (type-unify1-atomic typevars template type)
        (type-unify1-compound typevars template type))))

(defun type-unify1-atomic (typevars template type)
  (ematch template
    ((guard typevar (member typevar typevars))
     ;; template is an atomic typevar
     (values (list (cons typevar type)) t))
    ('* (warn "Avoid using * in type specification, it matches everything!")
        (values nil t))
    ((symbol)
     ;; template is a standard atomic type, and not a typevar
     (if (subtypep type template)
         (values nil t)
         nil))
    ((integer)
     ;; FIXME possibly a dimension specifier of array
     (ematch type
       ((eq template) (values nil t))
       ('* (values nil t))
       (_ nil)))
    ;; compound types in file:///usr/share/doc/hyperspec/Body/04_bc.htm
    ((list* (or 'eql 'member 'satisfies) args)
     (when (intersection args typevars)
       (error "Type specifier ~a should not contain a type variable! ~% ~a" args template))
     (if (subtypep type template)
         (values nil t)
         nil))
    ((list* 'or templates)
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
    ((list* 'and templates)
     (values (reduce (lambda (mapping template)
                       (multiple-value-match (type-unify1 typevars template type)
                         ((mapping2 t)
                          (multiple-value-match (merge-mappings-as-and mapping mapping2)
                            ((mapping3 t) mapping3)
                            ((_ nil)
                             (return-from type-unify1-atomic))))
                         ((_ nil)
                          (return-from type-unify1-atomic))))
                     templates :initial-value nil)
             t))
    ((list* typespec _)
     (error "~a is not supported" typespec))))

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
           for %typevals = (remove-smaller typevals #'strict-subtypep-or-indifferent)
           if (= (length %typevals) 1)
             collect (cons typevar (car %typevals))
           else
             collect (cons typevar `(or ,@%typevals))))))

(defun merge-mappings-as-and (mapping1 mapping2)
  (let (plist)
    (dolist (pair mapping1)
      (pushnew (cdr pair) (getf plist (car pair)) :test #'equal))
    (dolist (pair mapping2)
      (pushnew (cdr pair) (getf plist (car pair)) :test #'equal))
    (values (nreverse
             (loop for (typevar typevals) on plist by #'cddr
                   for %typevals = (remove-larger typevals (lambda (a b)
                                                             (if (subtypep a b)
                                                                 t (values nil t))))
                   if (subtypep `(and ,@typevals) nil) ;; (and ,@typevals) == nil
                     do (return-from merge-mappings-as-and)
                   else
                     collect (cons typevar (if (= (length %typevals) 1)
                                               (car %typevals)
                                               `(and ,@%typevals)))))
            t)))

(defun type-unify1-compound (typevars template type)
  (ematch* (template type)
    ;; typexpand is implementation dependent, and it may not expand some
    ;; types e.g. string -> (array character).
    ;; Therefore we need TYPE-R.
    (((array-subtype) (array-subtype))
     (unify-arrayoid typevars template type))
    (((real-subtype) (real-subtype))
     (unify-numeroid typevars template type))
    ((list* head elements1)
     (ematch template
       ((and (symbol)
             (guard typevar (member typevar typevars)))
        ;; template is an atomic typevar
        (values (list (cons typevar type)) t))
       ('* (warn "Avoid using * in type specification, it matches everything!")
           (values nil t))
       ((symbol)
        ;; template is a standard atomic type.
        ;; FIXME: string -> (array char *)
        nil)
       ((list* (eq head) elements2)
        (let ((max (max (length elements1) (length elements2))))
          (values (mappend (lambda (e1 e2)
                             (multiple-value-match
                                 (type-unify1 typevars e1 e2)
                               ((mapping t) mapping)
                               ((_ nil) (return-from type-unify1-compound nil))))
                           (pad max '* elements2)
                           (pad max '* elements1))
                  t)))))))

(defun unify-numeroid (typevars template type)
  (ematch* (template type)
    ;; integers (fixnum and bignum are disjoint subtypes of integer)
    (((fixnum-type) (fixnum-type))
     (%unify-numeroid template type))
    (((bignum-type) (bignum-type))
     (%unify-numeroid template type))
    (((integer-type) (integer-subtype))
     (%unify-numeroid template type))
    ;; floats (*-float are NOT disjoint subtypes. However we do not unify them.)
    (((single-float-type) (single-float-type))
     (%unify-numeroid template type))
    (((double-float-type) (double-float-type))
     (%unify-numeroid template type))
    (((long-float-type) (long-float-type))
     (%unify-numeroid template type))
    (((short-float-type) (short-float-type))
     (%unify-numeroid template type))
    (((float-type) (float-subtype))
     (%unify-numeroid template type))
    ;; ratio (integer and ratio are disjoint subtypes of rational)
    (((ratio-type) (ratio-type))
     (%unify-numeroid template type))
    (((rational-type) (rational-subtype))
     (%unify-numeroid template type))
    (((real-type) (real-subtype))
     (%unify-numeroid template type))))

(defun-ematch* %unify-numeroid (template type)
  (((real-subtype low1 high1) (real-subtype low2 high2))
   (multiple-value-match (number-unify1 typevars low1 low2)
     ((mapping1 t)
      (multiple-value-match (number-unify1 typevars high1 high2)
        ((mapping2 t)
         (multiple-value-match (merge-mappings-as-and mapping mapping2)
           ((mapping3 t) mapping3))))))))

(defun unify-arrayoid (typevars template type)
  (ematch* (template type)
    (((array-subtype element-type1 dimensions1)
      (array-subtype element-type2 dimensions2))
     (multiple-value-match (type-unify1 typevars element-type1 element-type2)
       ((mapping1 t)
        (multiple-value-match (dimentions-unify typevars dimensions1 dimensions2)
          ((mapping2 t)
           (multiple-value-match (merge-mappings-as-and mapping1 mapping2)
             ((mapping3 t) mapping3)))))))))

(defun dimensions-unify (typevars dimensions1 dimensions2)
  )

(defun pad (max thing list)
  (append list
          (make-list (- max (length list)) :initial-element thing)))
