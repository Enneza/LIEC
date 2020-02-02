#lang racket/base

(require parser-tools/yacc
         parser-tools/lex
         "lexer.rkt"
         "ast.rkt"
         "helper.rkt")

(provide parse)

(define parse-syntax
    (parser
        (src-pos)
        (tokens constants operators)
        (start prog)
        (end Leof)
        (grammar
    
            (prog
                [()                (list)]
                [(instr)           (list $1)]
                [(instr Lsep prog) (cons $1 $3)])
        
            (instr
                [(Lident Lassign expr)      (Passign $1 $3 $2-start-pos)]
                [(Lident Lcol Lident Lassign expr) (Passign $1 $5 $4-start-pos)]
                [(expr)                     (Pexpr $1 $1-start-pos)])

            (expr

                [(Lopar Lcpar)              (Pnil $1-start-pos)]
                [(Lnum)                     (Pnum $1 $1-start-pos)]
                [(Lstr)                     (Pstr $1 $1-start-pos)]
                [(Lident)                   (Pvar $1 $1-start-pos)]
                [(Lident Lopar args Lcpar)  (Pcall $1 $3 $1-start-pos)]


                [(Lbool)                    (Pbit $1 $1-start-pos)]
         

                [(expr Lplus expr)          (Pcall '%add (list $1 $3) $2-start-pos)]
                [(expr Lsub expr)           (Pcall '%sub (list $1 $3) $2-start-pos)]
                [(expr Lmul expr)           (Pcall '%mul (list $1 $3) $2-start-pos)]
                [(expr Ldiv expr)           (Pcall '%div (list $1 $3) $2-start-pos)]

                [(expr Leql expr)           (Pcall '%eql (list $1 $3) $2-start-pos)]
                [(expr Lneq expr)           (Pcall '%neq (list $1 $3) $2-start-pos)]
                [(expr Llt expr)            (Pcall '%lt  (list $1 $3) $2-start-pos)]
                [(expr Lgt expr)            (Pcall '%gt  (list $1 $3) $2-start-pos)]
                [(expr Llte expr)           (Pcall '%lte (list $1 $3) $2-start-pos)]
                [(expr Lgte expr)           (Pcall '%gte (list $1 $3) $2-start-pos)]

                [(Lopar expr Lcpar) $2])
       
            (args
                [()                 (list)]
                [(expr)             (list $1)]
                [(expr Lcomma args) (cons $1 $3)]))
   
        (precs
            (right Lelse)
            (left Lif Leql Lneq Llt Lgt Llte Lgte)
            (left Lplus Lsub)
            (left Lmul Ldiv))
   
        (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (err (format "syntax error near ~a~a"
                         (substring (symbol->string tok-name) 1)
                         (if tok-value
                             (format "(~a)" tok-value)
                             ""))
                 start-pos)))
    )
)

(define (parse src)
    (port-count-lines! src)
    (parse-syntax (lambda () (get-token src)))
)