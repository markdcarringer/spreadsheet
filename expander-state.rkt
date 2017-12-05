#lang br/quicklang

(define-macro (spreadsheet-mb PARSE-TREE)
  #'(#%module-begin
     (void PARSE-TREE)
     (for-each (lambda (arg) (printf arg))
               (traverse-ht))))
(provide (rename-out [spreadsheet-mb #%module-begin]))

(define-macro (ss-program COMMAND-STRING ...)
  #'(void COMMAND-STRING ...))
(provide ss-program)

(define-macro (ss-set SET-STR)
  #'(set-func SET-STR))
(provide ss-set)

(define ht (make-hash))
(define envalued-cells (list))

(define (set-func input-str)
  (define args (string-split input-str))
  (define cell (first args))
  (define value-string (second args))
  (define value (get-cell-value value-string))
  
  (cond
    [(number? value)      (insert-kvp cell value)]
    [(is-cell-ref? value) (insert-kvp cell (resolve-cell-ref value))]
    [(string? value)      (insert-kvp cell value)]))

;; This function takes a string value and returns a number if possible, and otherwise a string.
(define (get-cell-value value)
  (if (string->number value)
      (string->number value)
      value))

;; Returns true if the string cell is a valid cell reference, false otherwise. 
(define (is-cell-ref? cell)
  (regexp-match? #rx"[A-Za-z]+[0-9]+" cell))

;; This function inserts a key-value pair into ht, where the key is the cell reference, and
;; value is either a number or a string.
(define (insert-kvp cell value)
  (set! envalued-cells (cons cell envalued-cells))
  (hash-set! ht cell value))

;; This function takes a cell reference and returns either a string or a number, which is the
;; value of the referenced cell.
(define (resolve-cell-ref ref)
  (hash-ref ht ref))

;; Traverses the hash table, resulting in a list of cell references and values. 
(define (traverse-ht)
  (define new-list (sort (remove-duplicates envalued-cells) string<?))
  (for ([key new-list])
    (display (~a key " " (hash-ref ht key) "\n"))))