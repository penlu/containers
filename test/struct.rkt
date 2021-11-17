#lang rosette

(require "../model/struct.rkt" "../model/calls.rkt")

; === TESTS ===

; returns a system with a directory mounted on /foo which contains a directory bar
; so there is a /foo/bar
(define (test-sys)
  (define sys (create-sys))
  (define proc (sys-get-proc sys 1))
  (define ns (process-mnt-ns proc))
  (define root-mount (car (system-mounts sys)))
  (define root-path (namei sys proc (list "/")))
  (define root-dent (cdr root-path))
  (define root-dev (car (system-devs sys)))

  (define empty-mount (mount '() '() '() '() '()))

  (define fake-inode (inode 1 root-dev))
  (printf "TEST: lookup-mnt for nonexistent path: ~v\n"
    (lookup-mnt sys (cons empty-mount (dentry fake-inode root-dent))))

  (printf "TEST: are identical paths equal? ~v\n"
    (equal?
      root-path (cons (car root-path) (cdr root-path))))

  (printf "TEST: are identical dentries equal? ~v\n"
    (equal?
      root-dent
      (dentry (dentry-ino root-dent) (dentry-parent root-dent))))

  ; create new dir in root mount
  (define ino-foo (create-inode/dir! root-dev))
  (add-inode-child! (dentry-ino root-dent) "foo" ino-foo)

  ; create new fs with a directory /bar
  (define new-dev (create-device! sys 'b))
  (define new-root-dent (device-root new-dev))
  (printf "TEST: new-root-dent: ~v\n" new-root-dent)
  (define ino-bar (create-inode/dir! new-dev))
  (add-inode-child! (dentry-ino new-root-dent) "bar" ino-bar)

  ; mount new fs on /foo
  (define path-foo (namei sys proc (list "foo")))
  (define new-mount (mount ns new-dev (car path-foo) (cdr path-foo) new-root-dent))
  (add-sys-mounts! sys new-mount)
  (set-mnt-namespace-children! ns (list* new-mount (mnt-namespace-children ns)))

  (define path-foo2 (namei sys proc (list "foo")))
  (printf "TEST: device name of namei with path /foo: ~v\n" (device-name (mount-dev (car path-foo2))))
  (printf "TEST: inode of namei with path /foo: ~v\n" (dentry-ino (cdr path-foo2)))
  (printf "TEST: lookup-mnt for path of /foo: ~v\n" (lookup-mnt sys path-foo2))

  (define path-bar (namei sys proc (list "foo" "bar")))
  (printf "TEST: inode of namei with path /foo/bar: ~v\n" (dentry-ino (cdr path-bar)))
  )

(test-sys)

(define (test-sys2)
  (define sys (create-sys))
  (define proc (sys-get-proc sys 1))

  (define dev-b (create-device! sys 'b))
  (define dev-c (create-device! sys 'c))
  (define dev-d (create-device! sys 'd))

  ; we make /a, mount 'b on /a, and cd a
  (syscall-mkdir! sys proc (list "a"))
  (syscall-mount! sys proc dev-b (list "a") '())
  (syscall-chdir! sys proc (list "a"))
  (let* (
      [cur-ino (dentry-ino (cdr (process-pwd proc)))]
      [cur-dev (inode-dev cur-ino)])
    (printf "TEST: pwd dev (should be 'b): ~v\n" (device-name cur-dev)))

  ; we make /a/b, mount 'c on /a/b, mount d on /, and cd b
  (syscall-mkdir! sys proc (list "b"))
  (syscall-mount! sys proc dev-c (list "b") '())
  (syscall-mount! sys proc dev-d '() '())
  (syscall-chdir! sys proc (list "b"))
  (let* (
      [cur-ino (dentry-ino (cdr (process-pwd proc)))]
      [cur-dev (inode-dev cur-ino)])
    (printf "TEST: pwd dev (should be 'c): ~v\n" (device-name cur-dev)))

  ; we cd .. so we should be in /a (device 'b)
  (syscall-chdir! sys proc (list ".."))
  (let* (
      [cur-ino (dentry-ino (cdr (process-pwd proc)))]
      [cur-dev (inode-dev cur-ino)])
    (printf "TEST: pwd dev (should be 'b): ~v\n" (device-name cur-dev)))

  ; we cd .. so we should be in / (therefore dropping into device 'd)
  (syscall-chdir! sys proc (list ".."))
  (let* (
      [cur-ino (dentry-ino (cdr (process-pwd proc)))]
      [cur-dev (inode-dev cur-ino)])
    (printf "TEST: pwd dev (should be 'd): ~v\n" (device-name cur-dev)))
  )

(test-sys2)
