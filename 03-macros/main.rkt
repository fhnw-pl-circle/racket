#lang racket

(define-syntax (swap! stx)
  (syntax-case stx (swap!)
    [(swap! a b) 
      #'(let ([tmp a])
          (set! a b)
          (set! b tmp))
    ]))

(define x "Hey")
(define y 20)

(printf "{x=~v, y=~v}" x y)
(swap! x y)
(printf "{x=~v, y=~v}" x y)

; https://docs.racket-lang.org/reference/lambda.html

(define-syntax (parameters stx)
    (syntax-case stx (parameters define)
      [(_ (outer ...)
           (define (name param ...) body ...) ...) 
           
           #'(define-values (name ...)
                (let ([f (lambda (outer ...)
                            (local [(define (name param ...) body ...) ...]
                                (list name ...)))])
                                
                     (values
                       (case-lambda 
                         [(outer ...)
                            (match (f outer ...)
                                [(list name ...) name])]
                         [(outer ... . rest)
                            (match (f outer ...)
                                [(list name ...) (apply name rest)])])
                       ...)))]))

(parameters [tag]
  (define (collatz n)
    (printf "[~a] Called Collatz with ~v~n" tag n)
    (cond 
      [(= 1 n)            (void)]
      [(= 0 (modulo n 2)) (even-case n)]
      [else               (odd-case  n)]))
  (define (even-case n)
    (printf "[~a] Even Case of ~v~n" tag n)
    (collatz (quotient n 2)))
  (define (odd-case n)
    (printf "[~a] Odd Case of ~v~n" tag n)
    (collatz (+ 1 (* n 3))))
)

(collatz "hey" 17)

(module typed typed/racket
    (provide bad-point-2d good-point-2d)

    (struct bad-point-2d ([x : Integer] [y : Integer]))

    (define-for-syntax (split-fields stx)
      (syntax-case stx ()
        [(type name) #'([name : type])]
        [(type name rest ...) #`([name : type] #,@(apply split-fields (list #'(rest ...))))]))
    
    (define-syntax (c-struct stx)
      (syntax-case stx (:)
        [(_ struct-name fields)
          #`(struct struct-name #,(split-fields #'fields) #:transparent)
        ]
      )
    )

    (c-struct good-point-2d {
      Integer x;
      Integer y;
    })
)

(require 'typed)