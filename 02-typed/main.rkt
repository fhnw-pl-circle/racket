#lang typed/racket

(struct Point2D ([x : Number] [y : Number])
  #:transparent)

(define (exception-test [v : Number])
  (with-handlers ([exn:fail:contract:divide-by-zero?     (lambda (exn) +inf.0)]
                  [(lambda (v) (equal? v 2))             (lambda (exn) "Two" )])
    (cond [(= 0 v) "Zero"]
          [(= 1 v) (/ 1 0)]
          [(= 2 v) (raise 2)]
          [else    (error 'exception-test "Cannot support values other than 0, 1, 2: ~v" v)])))

; (:print-type exception-test)
; (:type Number)
; (:type #:verbose Number)

(define my-favorite-things
  '(42 1729 3.14 5/7 'More "Things" #\I #rx"Couldn't" #px"come" |up| with more))

(define (twice [x : (U Number String)])
    (if (string? x)
        (string-append x x)
        (+ x x)))



(define #:forall (A B) (make-pair [a : A] [b : B]) : (Pairof A B)
  (cons a b))



(: my-map (All (A B) ((A -> B) (Listof A) -> (Listof B))))
(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (my-map f (cdr xs)))))

(: my-filter (All (A) ((A -> Boolean) (Listof A) -> (Listof A))))
(define (my-filter f xs)
  (cond [(null? xs) null]
        [(f (car xs)) (cons (car xs) (my-filter f (cdr xs)))]
        [else (my-filter f (cdr xs))]))


(require/typed "../01-main/contracts.rkt" [snow-size Integer]
                                          [safe-sqrt (Nonnegative-Real -> Nonnegative-Real)])

(safe-sqrt 20)


; #lang typed/racket/no-check
; Syntax only, 0 checks

; #lang typed/racket/deep
;       (Listof String) checks a) list of b) only strings
; #lang typed/racket/shallow
;       (Listof String) checks a) list of. "String" is only checked when necessary, per-element.
; #lang typed/racket/optional
;       (Listof String) checks nothing *at runtime*.

; Refinement or Dependent types
; https://docs.racket-lang.org/ts-reference/Experimental_Features.html#(part._.Logical_.Refinements_and_.Linear_.Integer_.Reasoning)