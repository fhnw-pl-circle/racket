#lang racket

(collect-garbage 'major)

;;; Numbers

23
2.4
1.3e10
1/3
1+2i

2 * 2 + 3 * 3

; Strings and Symbols

"Strings"
'Symbols

#"Byte S\164ring"
(make-bytes 20 0)

#rx"regexes"
(regexp-match #px"^(.+)@(.+)$" "alpha.beta@students.fhnw.ch")

; #:keywords

; Characters

#\a
#\space
#\newline
#\nul
#\033
#\u1234
#\U00012345

; Booleans

#t #f

; Pairs Lists

(cons 1 2)
(list 1 2 3 4)
(cons 1 (cons 2 (cons 3 (cons 4 null))))

null

(car (list 1 2 3 4))
(first (list 1 2 3 4))
(cdr (list 1 2 3 4))
(rest (list 1 2 3 4))

(null? null)
(cons? null)

(length (list 1 2 3 4))
(list-ref (list 1 2 3 4) 1)

(map sqrt (list 1 4 9 16 25))

(map sqrt (filter positive? (list 1 -4 9 -16 -25)))

; Need mutable? Use mcons, set-mcar! p v and set-mcdr! p v
; Why so difficult? Because you shouldn't use them.

; Vectors - Fixed-length arrays, constant-time access

#(1 2 3)

; others:

(void)
(require (only-in racket/undefined undefined))
undefined

;;; Syntax

(list? (quote (1 2 3 4)))

'(1 2 3 4)

(eq? 'hello-world (quote hello-world))

'21
'"hello world"

(define ns (module->namespace 'racket))
(eval '(map sqrt (list 1 4 9 16)) ns)

; Reader layer  : Chars --> Lists, Symbols, Constantss
; Expander layer: L, S, C --> Expressions

'(this is a list value consisting of symbols and some constants like 20, 3/2 and "hello world.")

'(0 1 2)            ; Quote
'(0 1 2 (+ 1 2))    ; Quote
`(0 1 2 ,(+ 1 2))   ; Quasiquote + Unquote
`(0 1 2 ,@(+ 1 2))  ; Quasiquote + Unquote

;;; Definitions

(define n 20)

n

(define double
    (lambda (n) (* n 2)))
(double 12)

(define (square n)
    (* n n))
(square 2+5i)

(define my-very_weird/and+funny#name%is<all&legal/syntax 20)

(define |there even is support for: names like this!| 30)
; messes up syntax highlighting though, | to restore the later lines.

(define my\ awesome\ value 20)

(let ([x 1]
      [y 2])
  (+ x y))

(let* ([x 1]
       [y (* x 2)])
  (+ x y))

; letrec for recursive cases, only allowed for lazily evaluated cases.

;;; Conditionals

#|
(define (twice x)
    (if (string? x)
        (string-append x x)
        (+ x x)))
|#

; (twice "Hello FHNW!")
; (twice 2024)

; Tomatometer: XX / 100
(define (rotten-tomato-score movie)
    51)
; IMDb Ratings: X.X / 10
(define (imdb-score movie)
    7.2)

; Movie is good if: Tomatometer > 60
;               AND IMDb        > 7.5
(define (good-rated? movie)
    (and ((rotten-tomato-score movie) . > . 60)
         ((imdb-score movie) . > . 7.5)))

; I/O:

(define (fizzbuzz)
    'todo)

#|
(define 
  (fizzbuzz n #:mod1 [mod1 3]
              #:mod2 [mod2 5])
    (for ([i (in-range 1 (+ n 1))]) 
        (printf "Enter the Fizzbuzz for ~v: " i)
        (define result 
            (cond [(= 0 (modulo i mod1) (modulo i mod2))  "Fizzbuzz"]
                  [(= 0 (modulo i mod1))                  "Fizz"]
                  [(= 0 (modulo i mod2))                  "Buzz"]
                  [else (~v i)]))
        (define user-input (read-line (current-input-port) 'any))

        (printf "You entered ~s.\n" user-input)

        (if (equal? result user-input)
            (displayln "Well done!")
            (printf "You fail! It was ~v!" result))
        #:break (not (equal? result user-input))

        (void)))
|#

; Exceptions

(define (exception-test v)
    (with-handlers ([exn:fail:contract:divide-by-zero?     (lambda (exn) +inf.0)]
                    [(lambda (v) (equal? v 2))             (lambda (exn) "Two" )])
        (cond [(= 0 v) "Zero"]
              [(= 1 v) (/ 1 0)]
              [(= 2 v) (raise 2)]
              [else    (error 'exception-test "Cannot support values other than 0, 1, 2: ~v" v)])))



;;; Custom Data types

(struct Point2D (x y)
;    #:transparent
   #:methods gen:equal+hash
    [(define (equal-proc a b eq-inner?)
        (and (eq-inner? (Point2D-x a) (Point2D-x b))
             (eq-inner? (Point2D-y a) (Point2D-y b))))
     (define (hash-proc  v hash-inner)
        (+ (hash-inner (Point2D-x v))
           (* 3 (hash-inner (Point2D-y v)))))
     (define (hash2-proc v hash-inner)
        (+ (hash-inner (Point2D-x v))
           (* 5 (hash-inner (Point2D-y v)))))
    ]
  )

(define origin (Point2D 0 0))
(define origin-x (Point2D-x origin))
(define origin-y (Point2D-y origin))
(define origin-is-point (Point2D? origin))

(define x-axis (struct-copy Point2D origin [x 1]))


(struct Point3D Point2D (z))

(define origin3D (Point3D 0 0 0))
(Point2D-x origin3D)
(Point2D? origin3D)

(equal? (Point2D 10 20) (Point2D 10 20))

; #:prefab Option: Like #:transparent, but more restrictive and globally shared

; More Structures:

(struct Ref ([x #:mutable])
    #:constructor-name ref
    #:reflection-name '<opaque\ reference>)
(let [(value (ref 20))]
    (begin (set-Ref-x! value 10)
           (Ref-x      value)))

(struct Counter ([count #:mutable #:auto])
    #:auto-value 0
    #:extra-constructor-name make-counter)
(define cnt (make-counter))

(struct Range (start end)
    #:methods gen:custom-write
        [(define (write-proc range port mode)
            (fprintf port "[~a..~a]" (Range-start range) (Range-end range)))]
    #:guard (lambda (start end type-name)
                (if (start . < . end)
                    (values start end)
                    (error type-name "invalid range: ~v to ~v" start end))))

; Generic Interfaces

(require racket/generic)
(define-generics has-color
    (get-color has-color)
    (set-color has-color color))

(struct Ball (size [color #:mutable])
    #:methods gen:has-color
        [(define (get-color ball) (Ball-color ball))
         (define (set-color ball color) (set-Ball-color! ball color))])

(define my-red-ball (Ball 10 'red))

(get-color my-red-ball)

(define (color-or-default dflt o)
    (if (has-color? o)
        (get-color o)
        dflt))

; Classes & Interfaces

(define fish-interface (interface () get-size grow eat))

(define fish% (class* object% (fish-interface)
  (init size)                ; initialization argument
 
  (define current-size size) ; field
 
  (super-new)                ; superclass initialization
 
  (define/public (get-size)
    current-size)
 
  (define/public (grow amt)
    (set! current-size (+ amt current-size)))
 
  (define/public (eat other-fish)
    (grow (send other-fish get-size)))))

(define my-koi (new fish% [size 10]))

(send my-koi get-size)
(send my-koi eat my-koi)

; Modularity

(module mod-c typed/racket
    (module mod-a plait
        (define val-a : Number 20))

    (module mod-b typed/racket
        (provide val-b)
        
        (define val-b : Integer 30))

    (provide val-c)
    (require/typed 'mod-a [val-a Integer])
    (require 'mod-b)
    
    (define val-c : Integer (+ val-a val-b)))

(require 'mod-c)

(display val-c)

; Contracts

(require "contracts.rkt")