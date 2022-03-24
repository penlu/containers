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

(define (test-pivot-root)
  (define sys (create-sys))
  (define proc (sys-get-proc sys 1))

  (define new-dev (create-device! sys 'b))
  (printf "~v\n" (syscall-mkdir! sys proc (list "a")))
  (printf "~v\n" (syscall-mount! sys proc new-dev (list "a") '()))
  (printf "~v\n" (syscall-chdir! sys proc (list "a")))
  (printf "~v\n" (cdr (process-pwd proc)))
  (printf "~v\n" (syscall-pivot-root! sys proc (list ".") (list ".")))

  (define mnt-ns-root (mnt-namespace-root (process-mnt-ns proc)))
  (printf "dev should be 'b: ~v\n"
    (device-name (mount-dev mnt-ns-root)))
  (printf "parent dev should be 'b: ~v\n"
    (device-name (mount-dev (mount-parent mnt-ns-root))))

  (printf "should be on 'b: ~v\n" (cdr (process-root proc)))
  (printf "should be on 'b: ~v\n" (cdr (process-pwd proc)))
  (printf "\nNAMEI TEST\n")
  (printf "should be on 'a: ~v\n" (cdr (namei sys proc (list "."))))

  (printf "~v\n" (syscall-umount! sys proc (list ".")))
  (printf "should be on 'b: ~v\n" (cdr (namei sys proc (list "."))))

  (printf "TEST COMPLETE\n\n")
  )

(test-pivot-root)
