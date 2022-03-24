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

; convert string path to list
(define (convert-path path)
  (let* (
      [split (string-split path "/")]
      [absolute (string-prefix? path "/")])
    (if absolute (cons "/" split) split)))

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
            (fs-struct
              (process-root init)
              (process-pwd init))
            (process-mnt-ns init)
            (process-fds init)
            (process-may-chroot init)))
        (sys-add-proc! sys pid new-proc)
        ;(printf "NOW SYS PROCS: ~v\n" (system-procs sys))
        (printf "ADDED NEW PROC: ~v\n" pid)
        )
      '()))
  (define proc (sys-get-proc sys pid))
  (match tr
    [(trace-syscall pid ts "clone" args retval)
      (printf "CLONE: ~v ~v ~v = ~v\n" pid ts args retval)
      (let ([flags (cdr (assoc 'flags args))])
        (printf "~v\n" (syscall-clone! sys proc flags retval)))
      ]
    [(trace-syscall pid ts "close" fd retval)
      (printf "CLOSE: ~v ~v ~v = ~v\n" pid ts fd retval)
      (printf "~v\n" (syscall-close! sys proc (car fd)))
      ]
    [(trace-syscall pid ts "execve" (list path) retval)
      (printf "EXECVE: ~v ~v ~v = ~v\n" pid ts path retval)
      ]
    [(trace-syscall pid ts "fchdir" (list fd) retval)
      (printf "FCHDIR: ~v ~v ~v = ~v\n" pid ts fd retval)
      (printf "~v\n" (syscall-fchdir! sys proc fd))
      ]
    [(trace-syscall pid ts "mkdir" (list name) retval)
      (printf "MKDIR: ~v ~v ~v = ~v\n" pid ts name retval)
      (printf "~v\n" (syscall-mkdir! sys proc (convert-path name)))
      ]
    [(trace-syscall pid ts "open" (list path flags) retval)
      (printf "OPEN: ~v ~v ~v ~v = ~v\n" pid ts path flags retval)
      (printf "~v\n" (syscall-open! sys proc (convert-path path) flags retval))
      ]
    [(trace-syscall pid ts "openat" (list path flags) retval)
      (printf "OPENAT: ~v ~v ~v ~v = ~v\n" pid ts path flags retval)
      (when (not (equal? retval -1))
        (printf "~v\n" (syscall-open! sys proc (convert-path path) flags retval)))
      ]
    [(trace-syscall pid ts "openat" (list path flags mode) retval)
      (printf "OPENAT: ~v ~v ~v ~v ~v = ~v\n" pid ts path flags mode retval)
      (when (not (equal? retval -1))
        (printf "~v\n" (syscall-open! sys proc (convert-path path) flags retval)))
      ]
    [(trace-syscall pid ts "pivot_root" (list new-root put-old) retval)
      (printf "PIVOT_ROOT: ~v ~v ~v ~v = ~v\n" pid ts new-root put-old retval)
      (printf "~v\n" (syscall-pivot-root! sys proc (convert-path new-root) (convert-path put-old)))
      ]
    [(trace-syscall pid ts "setns" (list fd flags) retval)
      (printf "SETNS: ~v ~v ~v ~v = ~v\n" pid ts fd flags retval)
      (printf "~v\n" (syscall-setns! sys proc fd flags))
      ]
    [(trace-syscall pid ts "pseudo_mount" (list name target) retval)
      (printf "PSEUDO_MOUNT: ~v ~v ~v ~v = ~v\n" pid ts name target retval)
      (syscall-pseudo-mount! sys proc name (convert-path target))]
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
(printf "should be 'a: ~v\n" (device-name (mount-dev (car (process-root (sys-get-proc sys 8421))))))
