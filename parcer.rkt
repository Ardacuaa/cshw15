#lang racket
(require "utility.rkt")
(define neo-parser
  (lambda (neo-code)
    (cond
      ((null? neo-code) '())
      ((number? neo-code) (list 'num-exp neo-code))
      ((symbol? neo-code) (list 'var-exp neo-code))
      ;(bool op num1 num2) > (bool-exp op (neo-exp) (neo-exp))
       ((equal? (car neo-code) 'bool) (neo-bool-code-parser neo-code))
      ;(math op num1 num2) > (math-exp op (neo-exp) (neo-exp))
      ((equal? (car neo-code) 'math) (neo-math-code-parser neo-code))
      ;(ask (bool op num1 num2) (neo-exp1) (neo-exp2)) > (ask-exp (bool-exp ...) (parsed-neo-exp1) (parsed-neo-exp2))
      ((equal? (car neo-code) 'ask) (neo-ask-code-parser neo-code))
      ((equal? (car neo-code) 'function) (neo-function-code-parser neo-code))
      ;(call (function (x y z) (math + (math + x y) z)) (1 2 3)) ->
      ;(app-exp (func-exp (params (identifier1, identifier2, identifer3 ...)) (body-exp)) ((neo-exp1 neo-exp2 neo-exp3 ...))
      ((equal? (car neo-code) 'call) (neo-call-code-parser neo-code))
      ((equal? (car neo-code) 'local-vars) (neo-let-code-parser neo-code))
      ;(print a) -> (print-exp (var-exp a)
      ((equal? (car neo-code)'print) (list 'print-exp (neo-parser (cadr neo-code))))
      (else (map neo-parser neo-code)) ;((neo-parser 1) (neo-parser 'a) (neo-parser (math + 1 2)))
      )
    )
  )

;a parser only for boolean expression
(define neo-bool-code-parser
  (lambda (neo-code)
     (if (equal? (length neo-code) 3)
            (list 'bool-exp (elementAt neo-code 1) (neo-parser (caddr neo-code)) '())
        (cons 'bool-exp (cons (cadr neo-code) (map neo-parser (cddr neo-code)))))     
    )
  )

(define neo-math-code-parser
  (lambda (neo-code)
    (list 'math-exp (cadr neo-code)
             (neo-parser (caddr neo-code))
             (neo-parser (cadddr neo-code)))
    )
  )

(define neo-function-code-parser
  (lambda (neo-code)
    (list 'func-exp
             (list 'params (cadr neo-code))
             (list 'body-exp (neo-parser (caddr neo-code))))
    )
  )

(define neo-ask-code-parser
  (lambda (neo-code)
    (cons 'ask-exp
             (map neo-parser (cdr neo-code)))
    )
  )

(define neo-call-code-parser
  (lambda(neo-code)
    (list 'app-exp
             (neo-parser (cadr neo-code))
             (neo-parser (caddr neo-code)))
    )
  )

;(local-vars ((a 1) (b 2) (c 3)) (neo-exp)) = neo-code
;(let-exp ((a 1) (b 2) (c 3)) (parsed-neo-exp))
(define neo-let-code-parser
  (lambda (neo-code)
    (list 'let-exp
          (map (lambda (pair) (list (car pair) (neo-parser (elementAt pair 1))))
               (elementAt neo-code 1))
          (neo-parser (elementAt neo-code 2)))
                                                                     
    )
  )

(provide (all-defined-out))

             