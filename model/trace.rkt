#lang rosette

(require
  rosette/lib/angelic rosette/lib/match rosette/lib/synthax
  "struct.rkt" "calls.rkt")
(provide (all-defined-out))

(struct call () #:transparent)
(struct call-open call (path) #:transparent)
(struct call-mkdir call (path) #:transparent)
(struct call-chdir call (path) #:transparent)
(struct call-chroot call (path) #:transparent)
(struct call-fchdir call (fd) #:transparent)

(struct trace (call retval) #:transparent)

(define (interpret-calls sys proc calls)
  (for/list ([c calls])
    (match c
      [(call-open path) (trace c (syscall-open! sys proc path '()))]
      [(call-mkdir path) (trace c (syscall-mkdir! sys proc path))]
      [(call-chdir path) (trace c (syscall-chdir! sys proc path))]
      [(call-chroot path) (trace c (syscall-chroot! sys proc path))]
      [(call-fchdir fd) (trace c (syscall-fchdir! sys proc fd))]
      )))
