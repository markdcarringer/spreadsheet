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
      (define first-tll (list (make-hash) (list) "dynamic"))
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
  (lambda (ht ec mode)
    ;; Check to see if the value specified is a cell reference, and if so, resolve it into an ordinary value.
    (define res-value
      (if (is-cell-ref? value)
          (resolve-cell-ref ht value)
          value))
    ;; If the cell is already envalued, and the mode is set to static, the cell value is set to CONFLICT!.
    (define new-value
      (if (hash-has-key? ht cell)
          (if (equal? mode "dynamic")
              res-value
              '\#CONFLICT!)
          res-value))
    (hash-set! ht cell new-value)
    (set! ec (cons cell ec))
    (list ht ec mode)))

(define (traverse-ht ht ec)
  (define new-list (sort (remove-duplicates ec) string<?))
  (for ([key new-list])
    (display (~a key " " (hash-ref ht key) "\n"))))

;; Returns true if the string cell is a valid cell reference, false otherwise. 
(define (is-cell-ref? cell)
  (regexp-match? #rx"[A-Za-z]+[0-9]+" cell))

;; This function takes a cell reference and returns either a string or a number, which is the
;; value of the referenced cell.
(define (resolve-cell-ref ht ref)
  (if (hash-has-key? ht ref)
      (hash-ref ht ref)
      '\#INVALID-CELL-REFERENCE!))

(define-macro (ss-mode ARG)
  #'(make-moder ARG))
(provide ss-mode)

(define (make-moder input)
  (define new-mode
    (cond
      [(equal? (string-downcase input) "static") "static"]
      [else "dynamic"]))
  (lambda (ht ec mode)
    (set! mode new-mode)
    (list ht ec mode)))