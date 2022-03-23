#lang rosette

(require "../model/struct.rkt" "../model/calls.rkt")

(define (test-proc)
  (define sys (create-sys))
  (define proc (sys-get-proc sys 1))

  (printf "~v\n" (syscall-chdir! sys proc (list "proc")))
  (let ([cur-ino (dentry-ino (cdr (process-pwd proc)))])
    (printf "TEST: pwd ino (should be 0 'proc): ~v\n" cur-ino))
  (printf "~v\n" (syscall-chdir! sys proc (list "..")))

  ; make a mount namespace, to contain:
  ; 1. a non-root mount
  ; 2. a proc fs
  (define container-dev (create-device! sys 'b))
  (printf "~v\n" (syscall-clone! sys proc (list 'CLONE_NEWNS) 2))
  (define child (sys-get-proc sys 2))
  (printf "~v\n" (syscall-mkdir! sys child (list "a")))
  (printf "~v\n" (syscall-mount! sys child container-dev (list "a") '()))

  ; use pivot-root to swap root mounts
  (printf "~v\n" (syscall-chdir! sys child (list "a")))
  (printf "~v\n" (syscall-mkdir! sys child (list "old")))
  (printf "~v\n" (syscall-pivot-root! sys child (list ".") (list "old")))
  (printf "~v\n" (syscall-umount! sys child (list "old")))

  ; check that /proc shows the mount namespace correctly
  (printf "~v\n" (syscall-clone! sys proc (list 'CLONE_NEWNS) 3))
  (define child-two (sys-get-proc sys 3))
  (printf "~v\n" (syscall-open! sys child-two (list "/" "proc" "2" "ns" "mnt") '() 3))

  ; use setns to get another process into the mount namespace
  (printf "~v\n" (syscall-setns! sys child-two 3 0))

  (printf "TEST COMPLETE\n\n")
  )

(test-proc)
