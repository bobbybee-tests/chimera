; chimera
; WINGS compiler backend for MIT Scratch

#lang racket

(require racket)

(define (chimera-entry program)
  (pretty-print program)
  (chimera-compile (first (first program))
                   '()
                   (hash 'distance 0
                         'lambdas (map first (third program)))))

(define (chimera-compile block emission ctx)
  (if (empty? block)
    (list emission ctx)
    (let ([line (chimera-line (first block) ctx)])
      (chimera-compile (rest block)
                       (append (first line) emission)
                       (second line)))))

(define (chimera-line line ctx)
  (if (equal? (first line) "=")
    (let* ([expression (chimera-expression (third line) ctx)])
      (chimera-target (second line) ctx expression))
    (display "Unknown command\n")))

(define (chimera-target target ctx value)
  (case (first target)
    [("stack") (list (cons (list "setLine:ofList:to:"
                                 (list "readVariable" "sp")
                                 "memory"
                                 (first value))
                           (second value))
                     (third value))]
    [(else) (display "Unknown target")]))

(define (chimera-expression value ctx)
  (case (first value)
    [("call") (chimera-call value ctx)]
    [(else) (display "Unknown value type")]))

; (identifier prefix ctx)

(define (chimera-call value ctx)
  (let ([args (map (lambda (x) (chimera-identifier x ctx))
                   (rest value))])
    (pretty-print args)
    (list 42 '() ctx)))

(define (chimera-identifier identifier ctx)
  (pretty-print ctx)
  (case (first identifier)
    [("imm") (second identifier)]
    [("global") "global"]))

(pretty-print (chimera-entry (first (rest (read)))))
