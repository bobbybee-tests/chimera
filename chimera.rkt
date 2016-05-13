; chimera
; WINGS compiler backend for MIT Scratch

#lang racket

(require racket)

(define (chimera-entry program)
  (let ([ctx (hash 'distance 0
                   'lambdas (map (lambda (x) (first (first x)))
                                 (third program))
                   'ssize (fourth program))])
    (cons (chimera-dispatch program)
      (cons (chimera-compile (first (first program)) ctx)
            (chimera-lambdas (third program) ctx '() 0)))))

(define (chimera-lambdas lambdas ctx emission n)
  (if (empty? lambdas)
    emission
    (let ([l (first lambdas)])
      (chimera-lambdas (rest lambdas)
                       ctx
                       (cons (list 0 0 (cons (list "procDef"
                                         (chimera-lambda-spec n (first (first l)))
                                         (chimera-arg-names (first (first l)))
                                         (chimera-arg-defaults (first (first l)))
                                         #f)
                                   (chimera-compile (rest (first l))
                                                    (hash-set ctx
                                                              'ssize
                                                              (first (second l))))))
                             emission)
                       (+ n 1)))))

(define (chimera-compile block ctx)
  (first (chimera-compile-internal block
                                   (list (list "changeVar:by:"
                                               "sp"
                                               (- (hash-ref ctx 'ssize))))
                                   ctx)))
       
(define (chimera-compile-internal block emission ctx)
  (if (empty? block)
    (list (reverse (cons (list "changeVar:by:" "sp" (hash-ref ctx 'ssize))
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

(define (chimera-target target ctx value)
  (case (first target)
    [("stack") (list (cons (list "setLine:ofList:to:"
                                 (chimera-calculate-stack (second target) ctx)
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
    [("stack") (list (chimera-access-stack (second value) ctx) '() ctx)]
    [(else) (display "Unknown value type\n")]))

; (identifier prefix ctx)

(define (chimera-calculate-stack value ctx)
  (list "-" (list "readVariable" "sp") (- (hash-ref ctx 'ssize) value)))

(define (chimera-access-stack value ctx)
  (list "getLine:ofList:" (chimera-calculate-stack value ctx) "memory"))

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

(define (chimera-arg-names l)
  (if (member '..... l)
    (list "TODO impartial")
    (if (member '... l)
      (list "TODO variadic")
      l)))

(define (chimera-arg-defaults l)
  (map (lambda (x) 0) (chimera-arg-names l)))

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
    [("closure") (chimera-resolve-closure (second identifier) ctx)]
    [("stack") (chimera-access-stack (second identifier) ctx)]
    [else (pretty-print identifier)]))

(define (chimera-resolve-closure identifier ctx)
  (list "getParam" identifier))

(define (chimera-primitive name)
  #f)

(define (chimera-push-list lst ctx emission)
  (if (empty? lst)
    (list emission ctx)
    (chimera-push-list (rest lst)
                       (hash-set ctx 'distance (+ (hash-ref ctx 'distance) 1))
                       (cons (list "changeVar:by:"
                                   "sp"
                                   -1)
                             (cons (list "setLine:ofList:to:"
                                         (list "readVariable" "sp")
                                         "memory"
                                         (first lst))
                                   emission)))))

(define (chimera-dispatch program)
  (list '("procDef" "call %n" ("n") (0) #f)
        (chimera-dlog2 '("getParam" "n")
                       (map chimera-dispatch-lambda (third program))
                       0)))

(define (chimera-dlog2 test conditions start)
  (cond 
    [(empty? conditions)
     '()]
    [(= (length conditions) 1)
     (first conditions)]
    [(= (length conditions) 2)
     (append (list "doIfElse" (list "=" test start)) conditions)]
    [(even? (length conditions))
     (let ([hlen (/ (length conditions) 2)])
       (list "doIfElse"
             (list ">" test hlen)
             (chimera-dlog2 test (drop conditions hlen) (+ start hlen))
             (chimera-dlog2 test (take conditions hlen) start)))]
    [(odd? (length conditions))
     (chimera-dlog2 test (append conditions '()) start)]))

(define (chimera-dispatch-lambda l)
  (pretty-print l))

(pretty-print (chimera-entry (first (rest (read)))))
