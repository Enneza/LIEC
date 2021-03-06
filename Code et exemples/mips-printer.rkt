#lang racket/base

(require racket/match
         "ast.rkt")

(provide mips-print)

(define (format-loc location)
  (match location
    [(Lbl n) (format "~a" n)]
    [(Mem r o) (format "~a($~a)" o r)]
    [(? symbol? r) (format "$~a" r)]
  )
)

(define (print-instr instr)
  (match instr
    [(Asciiz n s) (printf "~a: .asciiz \"~a\"\n" n s)]
    [(Label n)    (printf "~a:\n" n)]
    [(Move d r)   (printf "  move $~a, $~a\n" d r)]
    [(Li d i)     (printf "  li $~a, ~a\n" d i)]
    [(La d l)     (printf "  la $~a, ~a\n" d (format-loc l))]
    [(Sw r l)     (printf "  sw $~a, ~a\n" r (format-loc l))]
    [(Lw r l)     (printf "  lw $~a, ~a\n" r (format-loc l))]
    [(Addi d r i) (printf "  addi $~a, $~a, ~a\n" d r i)]
    [(Add d r s)  (printf "  add $~a, $~a, $~a\n" d r s)]
    [(Sub d r s)  (printf "  sub $~a, $~a, $~a\n" d r s)]
    [(Mul d r s)  (printf "  mul $~a, $~a, $~a\n" d r s)]
    [(Div d r s)  (printf "  div $~a, $~a, $~a\n" d r s)]

    [(Eql d r s)  (printf "  eql $~a, $~a, $~a\n" d r s)]
    [(Neq d r s)  (printf "  neq $~a, $~a, $~a\n" d r s)]
    [(Lt d r s)   (printf "  lt $~a, $~a, $~a\n" d r s)]
    [(Gt d r s)   (printf "  gt $~a, $~a, $~a\n" d r s)]
    [(Lte d r s)  (printf "  lte $~a, $~a, $~a\n" d r s)]
    [(Gte d r s)  (printf "  gte $~a, $~a, $~a\n" d r s)]

    [(Syscall)    (printf "  syscall\n")]
    [(Jal l)      (printf "  jal ~a\n" (format-loc l))]
    [(Jr r)       (printf "  jr $~a\n" r)]
  )
)

(define (print-instructions instrs)
  (for-each print-instr instrs)
)

(define (mips-print asm)
  (printf "  .data\n")
  (print-instructions (Mips-data asm))
  (printf "\n  .text\n  .globl main\n")
  (print-instructions (Mips-text asm))
)