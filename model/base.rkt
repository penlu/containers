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
    [(equal? e 'EINVAL) #t]
    [(equal? e 'ENOENT) #t]
    [(equal? e 'ENOTDIR) #t]
    [(equal? e 'EPERM) #t]
    [else #f]))

; a dentry contains:
; - ino: inode
; - parent: parent dentry
; XXX this should be immutable but I literally do not know how else to make
; the cyclic root dentry
(struct dentry (ino parent) #:transparent #:mutable
  #:methods gen:custom-write
  [
    (define (write-proc self port mode)
      (fprintf port
        "(dentry ~v ~v)"
        (dentry-ino self)
        (dentry-parent self)))]
  ;#:guard (lambda (ino parent type-name)
  ;  (for*/all ([ino ino] [parent parent])
  ;    (cond
  ;      [(and (not (inode? ino)) ino)
  ;        (printf "ino is ~v\n" ino)
  ;        (error "dentry-ino not inode?")]
  ;      ; if only...
  ;      ;[(not (dentry? parent)) (error "dentry-parent not dentry?")]
  ;      [else (values ino parent)]
  ;      )))
  )

; a path is just a list of strings
; an ugly hack: (list "/") is root, '() is .
; e.g.: (list "/" "a") is /a, (list "a") is ./a
; slash corresponds to root; each component corresponds to a filename

; the system contains:
; - inum: namespace numbering index
; - procs: process table; list of (pid . process)
; - mounts: mount list; list of (dentry . mount)
; - devs: device list; list of device
(struct system (inum procs mounts devs) #:transparent #:mutable)

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
; - inum: some unique ID number
; - root mount
; - list of children
(struct mnt-namespace (inum root children) #:transparent #:mutable)

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
(struct device (sys name num root inodes) #:transparent #:mutable)
  ;#:methods gen:custom-write
  ;[(define (write-proc self port mode)
  ;  (fprintf port "(device ~v ~v)" (device-name self) (device-num self)))]

; an inode contains:
; - device: corresponds to i_sb (super_block)
; - number: unique identifier within a device; corresponds to i_ino
; - operations are some mash of inode_operations and file_operations
; LATER: symlinks
; LATER: access permissions
(struct inode (num dev) #:transparent #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode)
    (let ([num (inode-num self)]
          [dev (device-name (inode-dev self))])
      (fprintf port "(inode ~v ~v)" num dev)))]
  )

; an inode is either:
; - a file, containing nothing
; - a directory, containing a list of (name, child inode) pairs
(define-generics inode-file
  [inode-read inode-file] ; get data
  )

(define-generics inode-dir
  [inode-lookup inode-dir name] ; look up a child
  )

(define-generics inode-ns
  [inode-ns inode-ns] ; get namespace
  )

(struct/c dentry inode? dentry?)
