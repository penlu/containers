#lang rosette

(require rosette/lib/angelic rosette/lib/match rosette/lib/synthax "struct.rkt" "calls.rkt")

; === TESTS ===

; returns a system with a directory mounted on /foo which contains a directory bar
; so there is a /foo/bar
(define (test-sys)
  (define sys (create-sys))
  (define proc (car (system-procs sys)))
  (define ns (process-mnt-ns proc))
  (define root-mount (car (system-mounts sys)))
  (define root-dentry (namei sys proc (list "/")))
  (define root-dev (car (system-devs sys)))

  (printf "TEST: lookup-one-mnt for nonexistent dentry: ~v\n"
    (lookup-one-mnt sys (dentry (mount '() '() '() '()) (inode root-dev 1))))

  (printf "TEST: are identical dentries equal? ~v\n"
    (equal?
      root-dentry
      (dentry (dentry-mnt root-dentry) (dentry-ino root-dentry))))

  ; create new dir in root mount
  (define ino-foo (create-inode-d! root-dev))
  (add-inode-child! (dentry-ino root-dentry) "foo" ino-foo)

  ; create new fs with a directory /bar
  (define mount-dev (create-device 'b))
  (add-sys-devs! sys mount-dev)
  (define ino-mount-root (create-inode-d! mount-dev))
  (printf "TEST: ino-mount-root: ~v\n" ino-mount-root)
  (define ino-bar (create-inode-d! mount-dev))
  (add-inode-child! ino-mount-root "bar" ino-bar)

  ; mount new fs on /foo
  (define new-mount (mount ns mount-dev (namei sys proc (list "foo")) ino-mount-root))
  (add-sys-mounts! sys new-mount)
  (set-mnt-namespace-children! ns (list* new-mount (mnt-namespace-children ns)))

  (define dentry-foo (namei sys (car (system-procs sys)) (list "foo")))
  (printf "TEST: inode of namei with path /foo: ~v\n" (dentry-ino dentry-foo))
  (printf "TEST: lookup-one-mnt for dentry of /foo: ~v\n" (lookup-one-mnt sys dentry-foo))

  (define dentry-bar (namei sys (car (system-procs sys)) (list "foo" "bar")))
  (printf "TEST: inode of namei with path /foo/bar: ~v\n" (dentry-ino dentry-bar))
  )

(test-sys)

(define (test-sys2)
  (define sys (create-sys))
  (define proc (car (system-procs sys)))

  (define dev-b (create-device 'b))
  (define dev-c (create-device 'c))
  (define dev-d (create-device 'd))

  (syscall-mkdir! sys proc (list "a"))
  (syscall-mount! sys proc dev-b (list "a") '())
  (syscall-chdir! sys proc (list "a"))
  (syscall-mkdir! sys proc (list "b"))
  (syscall-mount! sys proc dev-c (list "b") '())
  (syscall-mount! sys proc dev-d '() '())
  (syscall-chdir! sys proc (list "b"))
  (syscall-chdir! sys proc (list ".."))
  (printf "TEST: pwd inode: ~v\n" (dentry-ino (process-pwd proc)))
  )

(test-sys2)

; set up a system with a chroot jail and try to escape
(define (escape-setup)
  (define sys (create-sys))
  (define proc (car (system-procs sys)))

  (define mount-dev (create-device 'b))

  ; make dir /a and mount an empty fs
  (syscall-mkdir! sys proc (list "a"))
  (syscall-mount! sys proc mount-dev (list "a") '())

  ; chroot to /a
  (syscall-chdir! sys proc (list "a"))
  (syscall-chroot! sys proc '())

  (printf "TEST: pwd inode: ~v\n" (dentry-ino (process-pwd proc)))
  (printf "TEST: root inode: ~v\n" (dentry-ino (process-root proc)))

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
    '()
    (list "..")
    (list "a")))

(define (Path)
  (choose*
    (Relpath)
    (cons "/" (Relpath))))

(define (Call)
  (choose*
    (call-open (Path))
    (call-mkdir (Path))
    (call-chdir (Path))
    (call-chroot (Path))
    (call-fchdir 1)
    ))

(define (Calls n)
  (cond
    [(equal? n 0) '()]
    [else (choose* (cons (Call) (Calls (- n 1))))]))

(define (interpret-calls sys proc calls)
  (for-each (lambda (c)
    (match c
      [(call-open path) (syscall-open! sys proc path)]
      [(call-mkdir path) (syscall-mkdir! sys proc path)]
      [(call-chdir path) (syscall-chdir! sys proc path)]
      [(call-chroot path) (syscall-chroot! sys proc path)]
      [(call-fchdir fd) (syscall-fchdir! sys proc fd)])) calls))

(define (test-escape)
  (define-values (sys proc) (escape-setup))
  (syscall-open! sys proc '())
  (printf "TEST: opened: ~v\n" proc)
  (syscall-mkdir! sys proc (list "b"))
  (printf "TEST: mkdir: ~v\n" (dentry-ino (process-pwd proc)))
  (syscall-chroot! sys proc (list "b"))
  (printf "TEST: chroot: ~v\n" (dentry-ino (process-root proc)))
  (syscall-fchdir! sys proc 1)
  (printf "TEST: fchdir: ~v\n" (dentry-ino (process-pwd proc)))
  (syscall-chdir! sys proc (list ".."))
  (printf "TEST: chdir: ~v\n" (dentry-ino (process-pwd proc))))

(test-escape)

(define (synthesize-escape)
  (define-values (sys proc) (escape-setup))
  (define calls (Calls 3))

  (interpret-calls sys proc calls)
  (assert (equal? 'a (device-name (inode-dev (dentry-ino (process-pwd proc))))))
  (define model (solve (assert #t)))
  (if (sat? model)
    (printf "escaped chroot: ~v\n:D" (evaluate calls model))
    (printf "couldn't find escape :(\n"))
  )

(synthesize-escape)

(define (synthesize-no-escape)
  (define-values (sys proc) (escape-setup))
  (syscall-drop-cap-sys-chroot! sys proc)
  (define calls (Calls 6))

  (interpret-calls sys proc calls)
  (assert (equal? 'a (device-name (inode-dev (dentry-ino (process-pwd proc))))))
  (define model (solve (assert #t)))
  (if (sat? model)
    (printf "escaped chroot?! ~v\n:(" (evaluate calls model))
    (printf "couldn't find escape :D\n"))
  )

(synthesize-no-escape)
