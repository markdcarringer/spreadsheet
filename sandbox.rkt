#lang racket

(define ht (make-hash))
(hash-set! ht "a1" 2)
(hash-set! ht "a2" "apple")
(hash-set! ht "b1" 2)

(define ec (list "a1" "a2" "b1"))

(define output (for/list ([key ec])
                 (~a key " " (hash-ref ht key) "\n")))

;(display (for/list ([key ec])
;           (~a key " " (hash-ref ht key) "\n")))
;
;(for/list ([key ec])
;           (~a key " " (hash-ref ht key) "\n"))

(for-each (lambda (arg) (printf arg))
          output)