; chimera
; WINGS compiler backend for MIT Scratch

#lang racket

(require racket)

(define (chimera-entry program)
  (pretty-print program))

(chimera-entry (first (rest (read))))
