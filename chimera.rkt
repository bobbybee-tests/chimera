; chimera
; WINGS compiler backend for MIT Scratch

#lang racket

(require racket)

(define (chimera-entry program)
  (pretty-print (first (third program)))
  (let ([ctx (hash 'distance 0
                   'lambdas (map (lambda (x) (first (first x)))
                                 (third program))
                   'ssize (fourth program))])
    (cons (chimera-compile (first (first program)) ctx)
          (map (lambda (s) (chimera-compile (rest (first s))
                                            (hash-set ctx
                                                      'ssize
                                                      (first (second s)))))
               (third program)))))
  
(define (chimera-compile block ctx)
  (chimera-compile-internal block
                            (list (list "changeVariable"
                                        "sp"
                                        (- (hash-ref ctx 'ssize))))
                            ctx))

(define (chimera-compile-internal block emission ctx)
  (display "Block: \n")
  (pretty-print block)
  (if (empty? block)
    (list (reverse (cons (list "changeVariable" "sp" (hash-ref ctx 'ssize))
                         emission))
          ctx)
    (let ([line (chimera-line (first block) ctx)])
      (chimera-compile-internal (rest block)
                                (append (first line) emission)
                                (second line)))))

(define (chimera-line line ctx)
  (if (equal? (first line) "=")
    (let* ([expression (chimera-expression (third line) ctx)])
      (chimera-target (second line) ctx expression))
    (display "Unknown command\n")))

; this is broken; rewrite later to be better

(define (chimera-target target ctx value)
  (display "YO: ")
  (pretty-print target)
  (pretty-print ctx)
  (case (first target)
    [("stack") (list (cons (list "setLine:ofList:to:"
                                 (list "-"
                                       (list "readVariable" "sp") 
                                       (- (hash-ref ctx 'ssize)
                                          (second target)))
                                 "memory"
                                 (first value))
                           (second value))
                     (third value))]
    [("return") (list (cons (list "setVar:to:"
                                  "return"
                                  (first value))
                            (second value))
                      (third value))]
    [(else) (display "Unknown target\n")]))

(define (chimera-expression value ctx)
  (case (first value)
    [("call") (chimera-call value ctx)]
    [("stack") (chimera-access-stack value ctx)]
    [(else) (display "Unknown value type\n")]))

; (identifier prefix ctx)

(define (chimera-access-stack value ctx)
  (list (list "getLine:ofList:"
              (list "readVariable" "sp")
              "memory")
        '()
        ctx))

(define (chimera-call value ctx)
  (let ([args (map (lambda (x) (chimera-identifier x ctx))
                   (cddr value))])
    (if (equal? (first (second value)) "lambda")
      (let* ([l (list-ref (hash-ref ctx 'lambdas) (second (second value)))]
             [formatted-args (chimera-arg-format l args ctx)]
             [spec (chimera-lambda-spec (second (second value)) l)])
        (list (list "readVariable" "return")
              (cons (cons "call" (cons spec (first formatted-args)))
                    (second formatted-args))
              (third formatted-args)))
      (if (chimera-primitive (second (second value)))
        '()
        (let* ([l (chimera-identifier (second value) ctx)]
               [push (chimera-push-list args ctx '())])
          (list (list "readVariable" "return")
                (cons (list "call" "call" l (length args))
                      (first push))
                (second push)))))))

(define (chimera-arg-format l args ctx)
  (if (member '..... l)
    (list "TODO impartial")
    (if (member '... l)
      (list "TODO variadic")
      (list args '() ctx))))

(define (chimera-lambda-spec n l)
  (if (member '..... l)
   (list "TODO impartial")
   (if (member '... l)
     (list "TODO variadic")
     (string-join (cons "lambda"
                        (cons (number->string n)
                              (build-list (length l) (lambda (x) "%n"))))))))

(define (chimera-identifier identifier ctx)
  (case (first identifier)
    [("imm") (second identifier)]
    [("global") "global"]
    [("closure") (chimera-resolve-closure (second identifier) ctx)]))

(define (chimera-resolve-closure identifier ctx)
  (pretty-print ctx) ;TODO
  (list "getParam" identifier))

(define (chimera-primitive name)
  #f)

(define (chimera-push-list lst ctx emission)
  (if (empty? lst)
    (list emission ctx)
    (chimera-push-list (rest lst)
                       (hash-set ctx 'distance (+ (hash-ref ctx 'distance) 1))
                       (cons (list "changeVariable"
                                   "sp"
                                   1)
                             (cons (list "setLine:ofList:to:"
                                         (list "readVariable" "sp")
                                         "memory"
                                         (first lst))
                                   emission)))))

(pretty-print (chimera-entry (first (rest (read)))))
