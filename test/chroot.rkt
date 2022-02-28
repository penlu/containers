#lang rosette

(require
  rosette/lib/angelic rosette/lib/match rosette/lib/synthax
  "../model/struct.rkt" "../model/calls.rkt")

; set up a system with a chroot jail and try to escape
(define (escape-setup)
  (define sys (create-sys))
  (define proc (sys-get-proc sys 1))

  (define mount-dev (create-device! sys 'b))

  ; make dir /a and mount an empty fs
  (syscall-mkdir! sys proc (list "a"))
  (syscall-mount! sys proc mount-dev (list "a") '())

  ; chroot to /a
  (syscall-chdir! sys proc (list "a"))
  (syscall-chroot! sys proc '())

  ;(printf "TEST: pwd inode: ~v\n" (dentry-ino (cdr (process-pwd proc))))
  ;(printf "TEST: root inode: ~v\n" (dentry-ino (cdr (process-root proc))))

  (values sys proc))

(struct call () #:transparent)
(struct call-open call (path) #:transparent)
(struct call-mkdir call (path) #:transparent)
(struct call-chdir call (path) #:transparent)
(struct call-chroot call (path) #:transparent)
(struct call-fchdir call (fd) #:transparent)

; determine whether an escape exists
(define (Relpath)
  (choose*
    ;'()
    (list "..")
    (list "a")))

(define (Path)
  (choose*
    ;(cons "/" (Relpath))
    (Relpath)))

(define (Call)
  (choose*
    ;(call-open (Path))
    (call-mkdir (Path))
    (call-chdir (Path))
    (call-chroot (Path))
    ;(call-fchdir 1)
    ))

(define (Calls n)
  (cond
    [(equal? n 0) '()]
    [else (choose* (cons (Call) (Calls (- n 1))))]))

(define (interpret-calls sys proc calls)
  (for-each (lambda (c)
    (match c
      [(call-open path) (syscall-open! sys proc path '())]
      [(call-mkdir path) (syscall-mkdir! sys proc path)]
      [(call-chdir path) (syscall-chdir! sys proc path)]
      [(call-chroot path) (syscall-chroot! sys proc path)]
      [(call-fchdir fd) (syscall-fchdir! sys proc fd)]
      )) calls))

(define (test-escape)
  (define-values (sys proc) (escape-setup))
  (syscall-open! sys proc '() '())
  (printf "TEST: opened: ~v\n" proc)
  (syscall-mkdir! sys proc (list "b"))
  (printf "TEST: mkdir: ~v\n" (dentry-ino (cdr (process-pwd proc))))
  (syscall-chroot! sys proc (list "b"))
  (printf "TEST: chroot: ~v\n" (dentry-ino (cdr (process-root proc))))
  (syscall-fchdir! sys proc 1)
  (printf "TEST: fchdir: ~v\n" (dentry-ino (cdr (process-pwd proc))))
  (syscall-chdir! sys proc (list ".."))
  (printf "TEST: chdir: ~v\n" (dentry-ino (cdr (process-pwd proc)))))

(test-escape)

(define (synthesize-escape)
  ; why does commenting this out affect literally anything
  (let*-values ([(calls) (Calls 3)] [(sys proc) (escape-setup)])
    (assert (equal? (car calls) (call-mkdir (list "a"))))
    (assert (equal? (cadr calls) (call-chroot (list "a"))))
    (assert (equal? (caddr calls) (call-chdir (list ".."))))
    (let ([model (solve (assert #t))])
      (interpret-calls sys proc (evaluate calls model))
      (if (and
            (sat? model)
            (equal? 'a (device-name (inode-dev (dentry-ino (cdr (process-pwd proc)))))))
        (printf "setup is correct :D\n")
        (printf "setup is incorrect :(\n"))))

  (printf "beginning test\n")
  (let*-values ([(calls) (Calls 3)] [(sys proc) (escape-setup)])
    ;(printf "proc: ~v\n" proc)
    ;(printf "calls: ~v\n" calls)

    ;(syscall-mkdir! sys proc (list "a"))
    ;(syscall-chroot! sys proc (list "a"))
    ;(syscall-chdir! sys proc (list ".."))
    (interpret-calls sys proc calls)
    ;(printf "~v\n" (process-pwd proc))
    ;(printf "~v\n" (car (process-pwd proc)))
    ;(printf "~v\n" (cdr (process-pwd proc)))
    ;(printf "~v\n" (dentry-ino (cdr (process-pwd proc))))

    ;(printf "~v\n" (device-name (inode-dev (dentry-ino (cdr (process-pwd proc))))))
    (assert (equal? 'a (device-name (inode-dev (dentry-ino (cdr (process-pwd proc)))))))
    (define model (solve (assert #t)))
    (if (sat? model)
      (printf "escaped chroot: ~v\n:D\n" (evaluate calls model))
      (printf "couldn't find escape :(\n")))
  )

(synthesize-escape)

; TODO fix, this is broken
(define (synthesize-no-escape)
  (define-values (sys proc) (escape-setup))
  (syscall-drop-cap-sys-chroot! sys proc)
  (define calls (Calls 6))

  (interpret-calls sys proc calls)
  (assert (equal? 'a (device-name (inode-dev (dentry-ino (cdr (process-pwd proc)))))))
  (define model (solve (assert #t)))
  (if (sat? model)
    (printf "escaped chroot?! ~v\n:(\n" (evaluate calls model))
    (printf "couldn't find escape :D\n"))
  )

(synthesize-no-escape)
