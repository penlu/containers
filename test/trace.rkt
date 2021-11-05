#lang rosette

(require
  racket/cmdline
  rosette/lib/angelic
  rosette/lib/match
  rosette/lib/synthax
  "../struct.rkt"
  "../calls.rkt")

(struct trace-syscall (pid ts name args retval) #:transparent)
(struct trace-exit (pid ts exitcode) #:transparent)
(struct trace-signal (pid ts name) #:transparent)
(struct trace-killed (pid ts) #:transparent)

; stackoverflow told me to do this
; I am bad at racket -- WHY is this the language design?
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (execute call)
  (match (eval call ns)
    [(trace-syscall pid ts "clone" args retval)
      (let ([flags (cdr (assoc 'flags args))])
        (printf "CLONE: ~v ~v ~v ~v\n" pid ts flags retval))]
    [(trace-syscall pid ts "open" (list path flags) retval)
      (printf "OPEN: ~v ~v ~v ~v\n" pid ts flags retval)]
    [_ '()]
    ))

(define (read-lines input)
  (let ([c (read input)])
    (cond
      [(eof-object? c) '()]
      [else
        (execute c)
        (read-lines input)]
      )))

(read-lines (current-input-port))
