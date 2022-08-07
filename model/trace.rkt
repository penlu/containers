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

(define (print-calls calls)
  (for ([c calls])
    (match c
      [(call-open p) (printf "open ")]
      [(call-mkdir p) (printf "mkdir ")]
      [(call-chdir p) (printf "chdir ")]
      [(call-chroot p) (printf "chroot ")]
      [(call-fchdir p) (printf "fchdir ")]
      ))
  (printf "\n")
  )

(define (interpret-call sys proc call)
  ;(print-calls (list call))
  (match call
    [(call-open path) (syscall-open! sys proc path '())]
    [(call-mkdir path) (syscall-mkdir! sys proc path)]
    [(call-chdir path) (syscall-chdir! sys proc path)]
    [(call-chroot path) (syscall-chroot! sys proc path)]
    [(call-fchdir fd) (syscall-fchdir! sys proc fd)]
    ))

(define (interpret-calls sys proc calls)
  ;(print-calls calls)
  (for/list ([c calls])
    (trace c (interpret-call sys proc c))))
