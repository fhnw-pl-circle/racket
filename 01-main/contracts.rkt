#lang racket

(provide random-value)
(define (random-value) (random 200))

(provide (contract-out [snow-size positive?]))
(define snow-size 20)

(define/contract better-snow-size
    (and/c number? positive?)
    30)

(define (larger-than? threshold)
    (flat-named-contract (string->symbol (format "larger than ~v" threshold))
      (lambda (actual) (threshold . < . actual))))

(provide (contract-out [bigger-snow-size (larger-than? snow-size)]))
(define bigger-snow-size 30)

(provide safe-sqrt)
(define/contract (safe-sqrt n)
    (-> (and/c number? (not/c negative?))
        (and/c number? (not/c negative?))
    )
    (sqrt n))

(provide identity)
(define/contract (identity v)
    (-> any/c any/c)
    v)