#lang rosette

(require racket/generic)

(provide (all-defined-out))

; === STRUCTURES ===
; TODO enforcing field types would be great

; error codes
(define (err? e)
  (cond
    [(equal? e 'EBADF) #t]
    [(equal? e 'EEXIST) #t]
    [(equal? e 'ENOENT) #t]
    [(equal? e 'ENOTDIR) #t]
    [(equal? e 'EPERM) #t]
    [else #f]))

; simple immutable dentry
; ino: inode
; parent: parent dentry
(struct dentry (ino parent) #:transparent)

; a path is just a list of strings
; an ugly hack: (list "/") is root, '() is .
; e.g.: (list "/" "a") is /a, (list "a") is ./a
; slash corresponds to root; each component corresponds to a filename

; the system contains:
; - process map
; - mount list: list of pairs (dentry . mount)
; - inode list
(struct system (procs mounts devs) #:transparent #:mutable)

; a process contains:
; - tgid
; - pid
; - mnt-ns: mount namespace
; - root: root dir, (mnt . dentry)
; - pwd: working dir, (mnt . dentry)
; - fds: list of (fd . (mnt . dentry))
; - may-chroot: SYS_CAP_CHROOT stand-in
; LATER: users, capabilities
; LATER: cgroups
(struct process (tgid pid mnt-ns root pwd fds may-chroot) #:transparent #:mutable)

; a mount namespace contains:
; - root mount
; - list of children
(struct mnt-namespace (root children) #:transparent #:mutable)

; roughly equivalent to struct mount and struct vfsmount
; a mount contains:
; - mnt-ns: containing mount namespace
; - dev: device, equivalent to mnt_sb
; - parent: parent mount
; - mountpoint: a dentry (in the parent mount)
; - root: a dentry (typically device root)
; this corresponds roughly to the vfsmount struct in linux
; LATER: propagation, i.e. mount groups
(struct mount (mnt-ns dev parent mountpoint root) #:transparent #:mutable)

; roughly equivalent to struct super_block
; a device contains:
; - sys: the containing system
; - name: any identifying name
; - num: counter used to assign numbers to inodes
; - root: the root dentry
; - inodes: a list of inodes
(struct device (sys name num root inodes) #:transparent #:mutable
  ;#:methods gen:custom-write
  ;[(define (write-proc self port mode)
  ;  (fprintf port "(device ~v ~v)" (device-name self) (device-num self)))]
  )

(struct proc-device device () #:transparent #:mutable)

; an inode is either:
; - a file, containing nothing
; - a directory, containing a list of (name, child inode) pairs

; an inode contains:
; - device: corresponds to i_sb (super_block)
; - number: unique identifier within a device; corresponds to i_ino
; - operations are some mash of inode_operations and file_operations
; LATER: symlinks
; LATER: access permissions
(struct inode (dev num) #:transparent)

(define-generics inode-file
  [inode-read inode-file] ; get data
  )

(define-generics inode-dir
  [inode-lookup inode-dir name] ; look up a child
  )

(define-generics inode-ns
  [inode-ns inode-ns] ; get namespace
  )

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

(struct inode/ns inode (ns) #:transparent #:mutable
  #:methods gen:inode-ns
  [
    (define (inode-ns ino) (inode/ns-ns ino))])

(struct inode/proc-root inode () #:transparent #:mutable
  #:methods gen:inode-dir
  [
    (define (inode-lookup ino name)
      (let* (
          [dev (inode-dev ino)]
          [procs (system-procs (device-sys dev))]
          [pid (string->number name)]
          [proc (assoc name procs)])
        (if proc
          ; create proc toplevel dir inode
          (begin
            (define ino (inode/proc-dir dev (device-num dev) proc))
            (set-device-num! dev (+ 1 (device-num dev)))
            (set-device-inodes! dev (list* ino (device-inodes dev)))
            ino)
          'ENOENT)))])

; TODO this thing is supposed to die when the proc goes away
(struct inode/proc-dir inode (proc) #:transparent #:mutable
  #:methods gen:inode-dir
  [
    (define (inode-lookup ino name)
      (let* (
          [dev (inode-dev ino)]
          [procs (system-procs (device-sys dev))]
          [pid (string->number name)]
          [proc (assoc name procs)])
        (if (equal? name "ns")
          (begin
            (define ino (inode/proc-ns-dir dev (device-num dev) proc))
            (set-device-num! dev (+ 1 (device-num dev)))
            (set-device-inodes! dev (list* ino (device-inodes dev)))
            ino)
          'ENOENT)))])

(struct inode/proc-ns-dir inode (proc) #:transparent #:mutable
  #:methods gen:inode-dir
  [
    (define (inode-lookup ino name)
      (let* (
          [dev (inode-dev ino)]
          [procs (system-procs (device-sys dev))]
          [pid (string->number name)]
          [proc (assoc name procs)])
        (if (equal? name "mnt")
          (begin
            (define ino (inode/ns dev (device-num dev) (process-mnt-ns proc)))
            (set-device-num! dev (+ 1 (device-num dev)))
            (set-device-inodes! dev (list* ino (device-inodes dev)))
            ino)
          'ENOENT)))])

; returns a new system containing:
; - a single mount
; - an empty root directory
; - a single process with a default namespace
; TODO a mounted /proc
(define (create-sys)
  (define sys (system '() '() '()))
  (define root-dev (create-device! sys 'a))
  (define root-dent (device-root root-dev))
  (define-values (mnt-ns root-mount)
    (shared ([ns (mnt-namespace root-mount (list root-mount))]
             [root-mount (mount ns root-dev root-mount root-dent root-dent)])
      (values ns root-mount)))
  (define root-path (cons root-mount root-dent))
  (define proc (process 1 1 mnt-ns root-path root-path '() #t))
  (set-system-procs! sys (list (cons 1 proc)))
  (set-system-mounts! sys (list (cons root-dent root-mount)))
  sys)

; create empty device
(define (create-device! sys name)
  (define dev (device sys name 1 '() '()))
  (define ino (inode/dir dev 0 '()))
  (set-device-inodes! dev (list ino))
  (set-device-root! dev (dentry ino #f)) ; TODO how to make this cyclic?
  (add-sys-devs! sys dev)
  dev)

(define (create-proc-device! sys)
  (define dev (create-device! sys 'proc))
  (define ino (inode/proc-root dev 0))
  (set-device-inodes! dev (list ino))
  (set-device-root! dev (dentry ino #f))
  dev)

; create empty file under device
(define (create-inode/file! dev)
  (define ino (inode/file dev (device-num dev) 0))
  (set-device-num! dev (+ 1 (device-num dev)))
  (set-device-inodes! dev (list* ino (device-inodes dev)))
  ino)

; create empty directory under device
(define (create-inode/dir! dev)
  (define ino (inode/dir dev (device-num dev) '()))
  (set-device-num! dev (+ 1 (device-num dev)))
  (set-device-inodes! dev (list* ino (device-inodes dev)))
  ino)

; create inode representing a namespace
(define (create-inode/ns! dev ns)
  (define ino (inode/ns dev (device-num dev) ns))
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
