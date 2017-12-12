#lang br/quicklang
(require spreadsheet/parser spreadsheet/tokenizer brag/support)

;;(parse-to-datum (apply-tokenizer-maker make-tokenizer "set a1 2\nset b1 apple\n"))
(parse-to-datum (apply-tokenizer-maker make-tokenizer "mode static\n// comment\nset a1 2\nset b2 \"apple\"\n// another comment\nset a2 a1\nset a1 3.5\n"))