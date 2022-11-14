#lang racket
(define elementAt
  (lambda(lst index)
    (cond
      ((not (list? lst)) "this is not a list")
      ((null? lst) "this is an empty list or index out of bound.")
      ((equal? index 0) (car lst))
      (else (elementAt (cdr lst) (- index 1)))
      )
    )
  )

(define getVarnames
  (lambda (lst)
    (if (null? lst) '()
        (cons (car (car lst)) (getVarnames (cdr lst)))
        )
    )
  )

(define getValues
  (lambda (lst)
    (if (null? lst) '()
        (cons (car (cdr (car lst))) (getValues (cdr lst)))
        )
    )
  )

(provide (all-defined-out))