#lang br/quicklang
(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define ss-lexer
      (lexer
       [(eof) eof]
       [(from/to "//" "\n") (next-token)]
       [(from/to "set" "\n") (token 'SET-TOKEN (trim-ends "set " lexeme "\n"))]
       [any-char (next-token)]))
    (ss-lexer port))
  next-token)
(provide make-tokenizer)