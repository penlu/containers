#lang rosette

(provide (all-defined-out))

; === STRUCTURES ===

; error codes
(define (err? e)
  (cond
    [(equal? e 'EBADF) #t]
    [(equal? e 'EEXIST) #t]
    [(equal? e 'ENOENT) #t]
    [(equal? e 'ENOTDIR) #t]
    [else #f]))

; instead of dentries, we are using (mount . inode) pairs
(struct dentry (mnt ino) #:transparent)

; a path is just a list of strings
; an ugly hack: (list "/") is root, '() is .
; e.g.: (list "/" "a") is /a, (list "a") is ./a
; slash corresponds to root; each component corresponds to a filename

; the system contains:
; - process list
; - mount list: list of pairs (dentry . mount)
; - inode list
(struct system (procs mounts devs) #:transparent #:mutable)

; a process contains:
; - namespace
; - root dir (dentry)
; - working dir (dentry)
; - fds (list of (fd, dentry)) pairs
; TODO users, capabilities
; TODO cgroups eventually
(struct process (mnt-ns root pwd fds) #:transparent #:mutable)

; a mount namespace contains:
; - root mount
; - list of children
(struct mnt-namespace (root children) #:transparent #:mutable)

; a mount contains:
; - mount namespace
; - mountpoint: a dentry
; - mount root: an inode
; this corresponds roughly to the vfsmount struct in linux
; TODO propagation, i.e. mount groups
(struct mount (ns dev mountpoint root) #:transparent #:mutable)

; a device contains:
; - name
; - num, used to assign numbers to inodes
; - a list of inodes
(struct device (name num root inodes) #:transparent #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode)
    (fprintf port "(device ~v ~v)" (device-name self) (device-num self)))])

; an inode is either:
; - a file, containing nothing
; - a directory, containing a list of (name, child inode) pairs

; an inode contains:
; - device
; - number: some identifier, unique within the scope of a device
; TODO access permissions
; TODO symlinks
(struct inode (dev num) #:transparent)

; file inode
; data is some arbitrary thing
(struct inode-f inode (data) #:transparent #:mutable)

; directory inode
; parent is the parent inode (perhaps incorrect, but our dentries are stupid for now)
; children is a list of (string . inode) pairs
(struct inode-d inode (parent children) #:transparent #:mutable)

; returns a new system containing:
; - a single mount
; - an empty root directory
; - a single process with a default namespace
(define (create-sys)
  (define root-dev (create-device 'a))
  (define root-inode (device-root root-dev))
  (define-values (ns root-mount root-dentry)
    (shared ([ns (mnt-namespace root-mount (list root-mount))]
             [root-mount (mount root-dev ns root-dentry root-inode)]
             [root-dentry (dentry root-mount root-inode)])
      (values ns root-mount root-dentry)))
  (define proc (process ns root-dentry root-dentry '()))
  (system (list proc) (list (cons root-dentry root-mount)) (list root-dev)))

(define (dentry-parent dent)
  (dentry (dentry-mnt dent) (inode-d-parent (dentry-ino dent))))

(define (mount-parent mnt)
  (dentry-mnt (mount-mountpoint mnt)))

(define (create-device name)
  (shared ([dev (device name 1 ino (list ino))]
           [ino (inode-d dev 0 '() '())])
    dev))

; create empty file under device
(define (create-inode-f! dev)
  (define ino (inode-f dev (device-num dev) 0))
  (set-device-num! dev (+ 1 (device-num dev)))
  (set-device-inodes! dev (list* ino (device-inodes dev)))
  ino)

; create empty directory under device
(define (create-inode-d! dev)
  (define ino (inode-d dev (device-num dev) '() '()))
  (set-device-num! dev (+ 1 (device-num dev)))
  (set-device-inodes! dev (list* ino (device-inodes dev)))
  ino)

; add a child to an inode-d
(define (add-inode-child! parent name child)
  (set-inode-d-parent! child parent)
  (set-inode-d-children! parent (list* (cons name child) (inode-d-children parent))))

; add device or list of devices to sys
(define (add-sys-devs! sys dev)
  (define devls (if (list? dev) dev (list dev)))
  (set-system-devs! sys (append devls (system-devs sys))))

; add mount or list of mounts to sys
(define (add-sys-mounts! sys mount)
  (define mntls (if (list? mount) mount (list mount)))
  (set-system-mounts! sys
    (append
      ; associate each mount with its mountpoint
      (map (lambda (mnt) (cons (mount-mountpoint mnt) mnt)) mntls)
      (system-mounts sys))))

; add file to process fd list
; return the fd
(define (add-proc-fd! proc f)
  (define fds (process-fds proc))
  (define fd (+ 1 (length fds)))
  (set-process-fds! (list* (cons fd f) fds))
  fd)
