#lang racket
(require "utility.rkt")
(require "runner1.rkt")
(require "parcer.rkt")



(define env '((global (a 1) (b 2) (c 5))))



(define sample-code '(print a)) ;local variable a 
(displayln (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)