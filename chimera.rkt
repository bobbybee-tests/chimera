; chimera
; WINGS compiler backend for MIT Scratch

#lang racket

(require racket)

(define (chimera-entry program)
  (chimera-compile (first (first program)) '() (hash 'distance 0)))

(define (chimera-compile block emission ctx)
  (if (empty? block)
    (list emission ctx)
    (let ([line (chimera-line (first block) ctx)])
      (chimera-compile (rest block)
                       (append (first line) emission)
                       (second line)))))

(define (chimera-line line ctx)
  (if (equal? (first line) "=")
    (let ([expression (chimera-expression line ctx)])
      (list (cons (list "setLine:ofList:to:"
                        (list "readVariable" "sp") ; TODO: calculate differences
                        "memory"
                        (first expression))
                  (second expression))
            (third expression)))
    (display "Unknown command\n")))

(define (chimera-expression value ctx)
  (list 42 '() ctx))

(pretty-print (chimera-entry (first (rest (read)))))
