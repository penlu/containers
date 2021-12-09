#lang rosette

(require "../model/struct.rkt" "../model/calls.rkt")

; === TESTS ===

(define (test-sys)
  ; create a system with a directory mounted on /foo which contains a directory bar
  ; so there is a /foo/bar
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

  (printf "TEST COMPLETE\n\n")
  )

(test-sys)

(define (test-calls)
  (define sys (create-sys))
  (define proc (sys-get-proc sys 1))

  (define dev-b (create-device! sys 'b))
  (define dev-c (create-device! sys 'c))
  (define dev-d (create-device! sys 'd))

  ; we mkdir a, mount 'b on a, and cd a
  ; we should end up at root of device 'b
  (syscall-mkdir! sys proc (list "a"))
  (syscall-mount! sys proc dev-b (list "a") '())
  (syscall-chdir! sys proc (list "a"))
  (let ([cur-ino (dentry-ino (cdr (process-pwd proc)))])
    (printf "TEST: pwd ino (should be 0 'b): ~v\n" cur-ino))

  ; we make b, mount 'c on b, mount d on . (i.e. /a)
  (syscall-mkdir! sys proc (list "b"))
  (syscall-mount! sys proc dev-c (list "b") '())
  (syscall-mount! sys proc dev-d (list ".") '())

  ; we now cd b
  ; we should end up at root of device 'c
  (syscall-chdir! sys proc (list "b"))
  (let ([cur-ino (dentry-ino (cdr (process-pwd proc)))])
    (printf "TEST: pwd ino (should be 0 'c): ~v\n" cur-ino))

  ; we cd .. so we should end up at root of device 'd (atop /a)
  (syscall-chdir! sys proc (list ".."))
  (let ([cur-ino (dentry-ino (cdr (process-pwd proc)))])
    (printf "TEST: pwd ino (should be 0 'd): ~v\n" cur-ino))

  ; we cd .. so we should be in / (therefore dropping into device 'a)
  (syscall-chdir! sys proc (list ".."))
  (let ([cur-ino (dentry-ino (cdr (process-pwd proc)))])
    (printf "TEST: pwd ino (should be 0 'a): ~v\n" cur-ino))

  ; we unmount 'd from a and cd a
  ; we should again end up at root of device 'b
  (printf "TEST: umount returned ~v\n" (syscall-umount! sys proc (list "a")))
  (syscall-chdir! sys proc (list "a"))
  (let ([cur-ino (dentry-ino (cdr (process-pwd proc)))])
    (printf "TEST: pwd ino (should be 0 'b): ~v\n" cur-ino))

  ; again mount d on . (i.e. /a) and cd .
  ; now we should end up in 'd
  (syscall-mount! sys proc dev-d (list ".") '())
  (syscall-chdir! sys proc (list "."))
  (let ([cur-ino (dentry-ino (cdr (process-pwd proc)))])
    (printf "TEST: pwd ino (should be 0 'd): ~v\n" cur-ino))

  ; back to 'a and umount a
  (syscall-chdir! sys proc (list ".."))
  (let ([cur-ino (dentry-ino (cdr (process-pwd proc)))])
    (printf "TEST: pwd ino (should be 0 'a): ~v\n" cur-ino))
  (printf "TEST: umount returned ~v\n" (syscall-umount! sys proc (list "a")))

  ; TODO outstanding bug: this will fail
  ; cd a, then mount 'd on . and cd ./b (to be distinguished from cd b)
  (syscall-chdir! sys proc (list "a"))
  (let ([cur-ino (dentry-ino (cdr (process-pwd proc)))])
    (printf "TEST: pwd ino (should be 0 'b): ~v\n" cur-ino))
  (syscall-mount! sys proc dev-d (list ".") '())
  (printf "KNOWN BUG:\n")
  (printf "TEST: chdir returned ~v\n" (syscall-chdir! sys proc (list "." "b")))
  ; testing on my linux shows that this just works, as does cd b
  ; so somehow cd . and cd "" enter 'd, but cd ./b does not
  ; kernel code seems to suggest step_into (hence eventually traverse_mounts)
  ; WILL be called in the case of cd ./b, but empirically not...?
  (let ([cur-ino (dentry-ino (cdr (process-pwd proc)))])
    (printf "TEST: pwd ino (should be 0 'c): ~v\n" cur-ino))

  (printf "TEST COMPLETE\n\n")
  )

(test-calls)
