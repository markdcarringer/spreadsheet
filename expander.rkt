#lang br/quicklang

(define-macro (spreadsheet-mb PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))
(provide (rename-out [spreadsheet-mb #%module-begin]))

(define (fold-funcs tll ss-funcs)
  (for/fold ([current-tll tll])
            ([ss-func (in-list ss-funcs)])
    (apply ss-func current-tll)))

(define-macro (ss-program SS-ARG ...)
  #'(begin
      (define first-tll (list (make-hash) (list)))
      (define last-tll (fold-funcs first-tll (list SS-ARG ...)))
      (define ht (first last-tll))
      (define ec (second last-tll))
      (traverse-ht ht ec)))
(provide ss-program)

(define-macro (ss-set ARG)
  #'(make-setter ARG))
(provide ss-set)

(define (make-setter input)
  (define input-list (string-split input))
  (define cell (first input-list))
  (define value (second input-list))
  (lambda (ht ec)
    (hash-set! ht cell value)
    (set! ec (cons cell ec))
    (list ht ec)))

(define (traverse-ht ht ec)
  (define new-list (sort (remove-duplicates ec) string<?))
  (for ([key new-list])
    (display (~a key " " (hash-ref ht key) "\n"))))