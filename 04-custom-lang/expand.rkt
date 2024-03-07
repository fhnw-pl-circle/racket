(module expand racket
  (provide (except-out (all-from-out racket)
                       #%module-begin)
           (rename-out [my-module-begin #%module-begin])
           assign literal func return var op arrow show call)

  (require (for-syntax racket/syntax))

  (define-syntax-rule (my-module-begin rest ...)
    (#%plain-module-begin rest ...))

  (define-for-syntax (syn-to-id syn)
    (datum->syntax syn (string->symbol (syntax-e syn))))

  (define-syntax (var stx)
    (syntax-case stx ()
        [(_ n)
          (with-syntax ([name (syn-to-id #'n)])
            #'name)]
        
        
        ))

  (define-syntax (assign stx)
    (syntax-case stx ()
        [(_ n v)
          (with-syntax ([name (syn-to-id #'n)])
            #`(define name v))]
        
        
        ))

  (define-syntax (func stx)
    (syntax-case stx ()
        [(_ n (param ...) (body ...))
          (with-syntax ([name (syn-to-id #'n)]
                        [params (map syn-to-id (syntax->list #'(param ...)))])
            #`(define name (lambda params body ...)))]
        
        
        ))

  (define-syntax-rule (literal v)
    v)

  (define-syntax-rule (return v)
    v)

  (define-syntax (arrow stx)
    (syntax-case stx (var)
     [(_ (var n) body)
        (with-syntax ([name (syn-to-id #'n)])
          #'(lambda (name) body))]))

  (define-syntax-rule (show v)
    (println v))

  (define-syntax (call stx)
    (syntax-case stx ()
        [(_ fun (args ...))
          #'(fun args ...)]
        
        
        ))

  (define-syntax (op stx)
    (syntax-case stx ()
      [(_ #\* a b) #'(* a b)]))

)