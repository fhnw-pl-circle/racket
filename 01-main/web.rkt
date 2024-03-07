#lang racket
(require web-server/servlet
         web-server/servlet-env)

(define (start req)
  (response/xexpr
   `(html (head (title "Hello world!"))
          (body (p "Hey out there!")))))
 
(serve/servlet start
    #:port 2005
    #:launch-browser? #f)

; http://localhost:2005/servlets/standalone.rkt

; #lang web-server/insta
; 
; (define (start req)
;   (response/xexpr
;    `(html (head (title "Hello world!"))
;           (body (p "Hey out there!")))))
