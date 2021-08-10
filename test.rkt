#lang rosette

(require rosette/lib/synthax "struct.rkt" "calls.rkt")

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

; determine whether an escape exists
(define-grammar (Call sys proc)
  [call
    (choose
      syscall-open! sys proc (path)
      syscall-mkdir! sys proc (path)
      syscall-chdir! sys proc (path)
      syscall-fchdir! sys proc 1
      )]
  [path (choose (relpath) (list* "/" (relpath)))]
  [relpath (choose '() (list* "a" (relpath)))])

(define (escape)
  (define-values (sys proc) (escape-setup))
  (Call sys proc #:depth 1))
