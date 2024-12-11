#!/usr/bin/env scheme-script
(library-directories '("."))
(import (chezscheme)
        (utils))

(define (help)
  (display "usage: aoc.ss new <name>")
  (newline)
  (exit))

(define (write-lisp)
  (write '(library-directories '(".")))
  (newline)
  (write '(import (chezscheme)
           (utils)))
  (newline)
  (display "
(define (main input)
  (let ((lines (read-lines input)))
    (display lines)))
"))

(define (main args)
  (if (zero? (length args))
      (help)
      (void))
  (let ((name (cadr args)))
    (with-output-to-file (string-append name ".ss")
      write-lisp
      'truncate)
    (with-output-to-file (string-append name "2.ss")
      write-lisp
      'truncate)
    (with-output-to-file (string-append name "input")
      (lambda () (newline))
      'truncate)))

(main (command-line-arguments))
