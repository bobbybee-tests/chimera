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
                   (cddr value))])
    (if (equal? (first (second value)) "lambda")
      (let* ([l (list-ref (hash-ref ctx 'lambdas) (second (second value)))]
             [formatted-args (chimera-arg-format l args ctx)]
             [nctx (third formatted-args)]
             [spec (chimera-lambda-spec (second (second value)) l)])
        (list (list "readVariable" "return")
              (cons (cons "call" (cons spec (first formatted-args)))
                    (second formatted-args))
              nctx))
      (display "Unknown other type of function call :P\n"))))

(define (chimera-arg-format l args ctx)
  (if (member '..... l)
    (list "TODO impartial")
    (if (member '... l)
      (list "TODO variadic")
      (list args '() ctx))))

(define (chimera-lambda-spec n l)
  (pretty-print n)
  (pretty-print l))

(define (chimera-identifier identifier ctx)
  (pretty-print ctx)
  (case (first identifier)
    [("imm") (second identifier)]
    [("global") "global"]))

(pretty-print (chimera-entry (first (rest (read)))))
