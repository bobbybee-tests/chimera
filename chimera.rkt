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
  (list 42 '() ctx))

(pretty-print (chimera-entry (first (rest (read)))))
