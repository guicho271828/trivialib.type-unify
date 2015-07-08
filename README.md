
# Trivialib.Type-Unify - unifies a polimorphic type specifier with type variables against actual type specifiers

## Usage

```cl
(type-unify1 '(a)
             '(array a *)
             '(array char (3)))
;; '((a . char)), t



(type-unify '(a)
            '(a a)
            '((array * 6) (array char *)))

;; --> '((a . (and (array * 6) (array char *)))), t



(type-unify '(a) '(a a) '(fixnum float))
;; -> nil, nil



(type-unify '(a) '() '())
;; -> nil, t  (a is unassigned)
```


## Dependencies
This library is at least tested on implementation listed below:

+ SBCL 1.2.8 on X86 Linux 3.13.0-57-generic (author's environment)

Also, it depends on the following libraries:

+ iterate by ** :
    Jonathan Amsterdam's iterator/gatherer/accumulator facility
+ alexandria by ** :
    Alexandria is a collection of portable public domain utilities.
+ trivia by *Masataro Asai* :
    NON-optimized pattern matcher compatible with OPTIMA, with extensible optimizer interface and clean codebase

## Installation

## Author

* Masataro Asai (guicho2.71828@gmail.com)

## Copyright

Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)

# License

Licensed under the LLGPL License.


