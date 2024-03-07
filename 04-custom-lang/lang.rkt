#lang racket

(require parsack)

(define (read-syntax path port)
    (define result (match (parse-result program port)
        [(list 'program stmts) stmts]))
    ; (println result)
    (define out-mod `(module unnamed-module "expand.rkt" ,@result))
    ; (println out-mod)
    (datum->syntax #f out-mod))

(provide read-syntax)

(define (parenthesized p)
    (between (char #\() (char #\)) p))

(define whitespace $spaces)
(define whitespace1 (>> $space $spaces))

(define program
    (parser-compose whitespace
                    (stmts <- stmts)
                    whitespace
                    (return (list 'program stmts))))

(define stmt
    (local
        [(define assign 
           (parser-compose (string "let")
                           whitespace1
                           (id <- $identifier)
                           whitespace
                           (char #\=)
                           whitespace
                           (value <- expr)
                           whitespace
                           (char #\;)
                           (return (list 'assign (list->string id) value))))
         (define func
           (parser-compose (string "function")
                           whitespace1
                           (name <- $identifier)

                           (char #\()
                           (params <- (sepBy $identifier 
                                             (between whitespace whitespace (char #\,))))
                           (char #\))

                           whitespace

                           (char #\{)
                           (body <- stmts)
                           (char #\})
                           

                           (return (list 'func (list->string name) (map list->string params) body))
                           ))
         (define show 
           (parser-compose (string "show")
                           whitespace
                           (value <- (parenthesized expr))
                           whitespace
                           (char #\;)
                           (return (list 'show value))))
         (define return-st 
           (parser-compose (string "return")
                           whitespace
                           (value <- expr)
                           whitespace
                           (char #\;)
                           (return (list 'return value))))]
        (<any> assign func show return-st)))

(define stmts
    (<any> (parser-compose (lookAhead (char #\}))
                           (return null))
           (parser-compose $eof
                            (return null))
           (parser-compose whitespace
                           (s <- stmt)
                           whitespace
                           (ss <- stmts)
                           whitespace
                           (return (cons s ss)))))

(define (expr-cont ex)
    (local [(define op-cont
                (parser-compose (op <- (oneOf "*-+/"))
                                whitespace
                                (right <- expr)
                                whitespace
                                (expr-cont (list 'op op ex right))
                                ))
            (define arrow-cont
                (parser-compose (string "=>")
                                (if (eq? 'literal (car ex))
                                    (err "=> after literal is not allowed")
                                    (return (void)))
                                whitespace
                                (right <- expr)
                                whitespace
                                (return (list 'arrow ex right))
                                ))
            (define call-cont
                (parser-compose (char #\()
                                (if (eq? 'literal (car ex))
                                    (err "call after literal not allowed")
                                    (return (void)))
                                whitespace
                                (args <- (sepBy expr (parser-compose whitespace
                                                                    (char #\,)
                                                                    whitespace)))
                                whitespace
                                (char #\))
                                whitespace
                                (expr-cont (list 'call ex args))
                                ))
            (define no-cont (return ex))]
        (<any> op-cont arrow-cont call-cont no-cont)))

(define expr
    (local [(define var
                (parser-compose (id <- $identifier)
                                whitespace
                                (expr-cont (list 'var (list->string id)))))
            (define literal
                (parser-compose (digits <- (many1 $digit))
                                whitespace
                                (expr-cont (list 'literal (string->number (list->string digits))))
                                ))]
        (<any> literal var)))
