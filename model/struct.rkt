#lang rosette

(require "base.rkt" "proc.rkt")
(provide
  (all-defined-out)
  (all-from-out "base.rkt")
  (all-from-out "proc.rkt"))

; file inode
; data is some arbitrary thing
(struct inode/file inode (data) #:transparent #:mutable
  #:methods gen:inode-file
  [
    (define (inode-read ino) (inode/file-data ino))])

; directory inode
; children is a list of (string . inode) pairs
(struct inode/dir inode (children) #:transparent #:mutable
  #:methods gen:inode-dir
  [
    (define (inode-lookup ino name)
      (let ([ino (assoc name (inode/dir-children ino))])
        (if ino (cdr ino) #f)))])

; create a dentry w/ given inode and parent as itself
(define (create-root-dentry ino)
  (let ([dent (dentry ino #f)])
    (set-dentry-parent! dent dent) ; XXX please don't mutate dentries elsewhere
    dent))

; returns a new system containing:
; - a single mount
; - an empty root directory
; - a single process with a default namespace
(define (create-sys)
  (define sys (system 1 '() '() '()))

  ; add a root device and a proc device
  (define root-dev (create-device! sys 'a))
  (define root-dent (device-root root-dev))
  (define procfs-dev (create-proc-device! sys))
  (define procfs-dent (device-root procfs-dev))
  (set-system-devs! sys (list root-dev procfs-dev))

  ; add /proc dir
  (define proc-dir (create-inode/dir! root-dev))
  (add-inode-child! (dentry-ino root-dent) "proc" proc-dir)

  ; mount root and proc devices under a base mount namespace
  ; TODO these should be helper fxns instead of manual manipulation
  (define procfs-mp (dentry proc-dir root-dent))
  (define-values (mnt-ns root-mount procfs-mount)
    (shared ([ns (mnt-namespace 0 root-mount (list root-mount procfs-mount))]
             [root-mount (mount ns root-dev root-mount root-dent root-dent)]
             [procfs-mount (mount ns procfs-dev root-mount procfs-mp procfs-dent)])
      (values ns root-mount procfs-mount)))
  (set-system-mounts! sys (list
    (cons root-dent root-mount)
    (cons procfs-mp procfs-mount)))

  ; create a process with pwd /
  (define root-path (cons root-mount root-dent))
  (define proc (process 1 1 mnt-ns root-path root-path '() #t))
  (set-system-procs! sys (list (cons 1 proc)))
  sys)

; create empty device
(define (create-device! sys name)
  (define dev (device sys name 1 '() '()))
  (define ino (inode/dir 0 dev '()))
  (set-device-inodes! dev (list ino))
  (set-device-root! dev (create-root-dentry ino))
  (add-sys-devs! sys dev)
  dev)

(define (create-proc-device! sys)
  (define dev (create-device! sys 'proc))
  (define ino (inode/proc-root 0 dev))
  (set-device-inodes! dev (list ino))
  (set-device-root! dev (create-root-dentry ino))
  dev)

; TODO don't do this; just expose an add method
; create empty file under device
(define (create-inode/file! dev)
  (define ino (inode/file (device-num dev) dev 0))
  (set-device-num! dev (+ 1 (device-num dev)))
  (set-device-inodes! dev (list* ino (device-inodes dev)))
  ino)

; create empty directory under device
(define (create-inode/dir! dev)
  (define ino (inode/dir (device-num dev) dev '()))
  (set-device-num! dev (+ 1 (device-num dev)))
  (set-device-inodes! dev (list* ino (device-inodes dev)))
  ino)

; create inode representing a namespace
(define (create-inode/ns! dev ns)
  (define ino (inode/ns (device-num dev) dev ns))
  (set-device-num! dev (+ 1 (device-num dev)))
  (set-device-inodes! dev (list* ino (device-inodes dev)))
  ino)

; add a child to an inode/dir
(define (add-inode-child! parent name child)
  (set-inode/dir-children! parent
    (list* (cons name child) (inode/dir-children parent))))

; add device or list of devices to sys
(define (add-sys-devs! sys dev)
  (define devs (if (list? dev) dev (list dev)))
  (set-system-devs! sys (append devs (system-devs sys))))

; add mount or list of mounts to sys
(define (add-sys-mounts! sys mnt)
  (define mnts (if (list? mnt) mnt (list mnt)))
  (set-system-mounts! sys
    (append
      ; associate each mount with its mountpoint
      (map (lambda (mnt) (cons (mount-mountpoint mnt) mnt)) mnts)
      (system-mounts sys))))

; add file to process fd list
; return the fd
(define (add-proc-fd! proc fd f)
  ; TODO fail on duplicate, probably
  (define fds (process-fds proc))
  ;(define fd (+ 1 (length fds)))
  (set-process-fds! proc
    (list*
      (cons fd f)
      (process-fds proc)))
  fd)

(define (sys-get-proc sys pid)
  (let ([proc (assoc pid (system-procs sys))])
    (if proc (cdr proc) #f)))

(define (sys-add-proc! sys pid proc)
  ; TODO fail on duplicate, probably
  (set-system-procs! sys
    (list*
      (cons pid proc)
      (system-procs sys)))
  pid)
