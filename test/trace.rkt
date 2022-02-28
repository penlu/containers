#lang rosette

(require
  racket/cmdline
  rosette/lib/angelic
  rosette/lib/match
  rosette/lib/synthax
  "../model/struct.rkt"
  "../model/calls.rkt")

(struct trace (pid ts) #:transparent)
(struct trace-syscall trace (name args retval) #:transparent)
(struct trace-exit trace (exitcode) #:transparent)
(struct trace-signal trace (name) #:transparent)
(struct trace-killed trace () #:transparent)

; stackoverflow told me to do this
; I am bad at racket -- WHY is this the language design?
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (execute sys call)
  (define tr (eval call ns))
  (define pid (trace-pid tr))
  (let ([proc (sys-get-proc sys pid)])
    (if (not proc)
      (let ([init (sys-get-proc sys 1)])
        (define new-proc
          (process
            pid
            pid
            ns
            (process-root init)
            (process-pwd init)
            (process-fds init)
            (process-may-chroot init)))
        (sys-add-proc! sys pid new-proc)
        (printf "NOW SYS PROCS: ~v\n" (system-procs sys))
        (printf "ADDED NEW PROC: ~v\n" pid)
        )
      '()))
  (define proc (sys-get-proc sys pid))
  (match tr
    [(trace-syscall pid ts "clone" args retval)
      (printf "CLONE: ~v ~v ~v = ~v\n" pid ts args retval)
      (let ([flags (cdr (assoc 'flags args))])
        (syscall-clone! sys proc flags retval)
        )]
    [(trace-syscall pid ts "setns" (list fd flags) retval)
      (printf "SETNS: ~v ~v ~v ~v = ~v\n" pid ts fd flags retval)
      (syscall-setns! sys proc fd flags)]
    ;[(trace-syscall pid ts "open" (list path flags) retval)
    ;  (printf "OPEN: ~v ~v ~v ~v = ~v\n" pid ts path flags retval)
    ;  (syscall-open! sys proc path flags retval)]
    ;[(trace-syscall pid ts "openat" (list dirfd path flags) retval)
    ;  (printf "OPENAT: ~v ~v ~v ~v ~v = ~v\n" pid ts dirfd path flags retval)]
    ;[(trace-syscall pid ts "openat" (list dirfd path flags mode) retval)
    ;  (printf "OPENAT: ~v ~v ~v ~v ~v ~v = ~v\n" pid ts dirfd path flags mode retval)]
    [_ '()]
    ))

(define (read-lines sys input)
  (let ([c (read input)])
    (cond
      [(eof-object? c) sys]
      [else
        (execute sys c)
        (read-lines sys input)]
      )))

(define sys (create-sys))
(read-lines sys (current-input-port))
