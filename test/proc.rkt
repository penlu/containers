#lang rosette

(require "../model/struct.rkt" "../model/calls.rkt")

(define (test-proc)
  (define sys (create-sys))
  (define proc (sys-get-proc sys 1))

  (printf "~v\n" (syscall-chdir! sys proc (list "proc")))
  (let ([cur-ino (dentry-ino (cdr (process-pwd proc)))])
    (printf "TEST: pwd ino (should be 0 'proc): ~v\n" cur-ino))

  ; TODO test mount namespace construction

  ; TODO test setns

  (printf "TEST COMPLETE\n\n")
  )

(test-proc)
