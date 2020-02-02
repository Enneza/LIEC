#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "helper.rkt")

(provide constants operators get-token)

(define-tokens constants
  (Lnum Lstr Lident Lbool)
)

(define-empty-tokens operators
  (Leof                         ;; end of file
   Leql Lneq Llt Lgt Llte Lgte  ;; == != < > <= >=
   Lassign                      ;; =
   Lplus Lsub Lmul Ldiv         ;; + - * /
   Lif Lelse Lcol Lthen         ;; if else : then
   Lopar Lcpar Lcomma Lsep      ;; ( ) , \n
   Ltrue Lfalse                 ;; True False
  )
)

(define-lex-abbrev identifier
  (:: alphabetic
      (:* (:or "_" alphabetic numeric))
  )
)


(define get-token
  (lexer-src-pos
   [(eof)        (token-Leof)]
   ["\n"         (token-Lsep)]
   [whitespace   (return-without-pos (get-token input-port))]
   
   ;; Booleans
   ["=="         (token-Leql)]
   ["!="         (token-Lneq)]
   ["<"          (token-Llt)]
   [">"          (token-Lgt)]
   ["<="         (token-Llte)]
   [">="         (token-Lgte)]

   ["True"       (token-Ltrue)]
   ["False"      (token-Lfalse)]

   ["0"          (token-Lbool #f)]
   ["1"          (token-Lbool #t)]

   ;; Assignement
   ["="          (token-Lassign)]
   
   ;; Operations
   ["+"          (token-Lplus)]
   ["-"          (token-Lsub)]
   ["/"          (token-Ldiv)]
   ["*"          (token-Lmul)]
   
   ;; Conditions
   ["if"         (token-Lif)]
   ["else"       (token-Lelse)]
   ["then"       (token-Lthen)]

   ;; Autres
   [":"          (token-Lcol)]
   ["("          (token-Lopar)]
   [")"          (token-Lcpar)]
   [","          (token-Lcomma)]

   [identifier   (token-Lident (string->symbol lexeme))]
   [(:+ numeric) (token-Lnum (string->number lexeme))]
   ["\""         (token-Lstr (read-str input-port))]
   [any-char (err (format "unrecognized character '~a'" lexeme)
                  start-pos)]
  )
)

(define read-str
  (lexer
   ["\\\""   (string-append "\"" (read-str input-port))]
   ["\""     ""]
   [any-char (string-append lexeme (read-str input-port))]
  )
)