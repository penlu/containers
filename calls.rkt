#lang rosette

(require "struct.rkt")

(provide (all-defined-out))

; === METHODS ===

; a path will be a list of strings

; see if something is mounted at the given dentry
; returns a mount if one is found; otherwise returns #f
(define (lookup-one-mnt sys dent)
  ; search the mount list
  (let ([found (assoc dent (system-mounts sys))])
    (if found (cdr found) #f)))

; find the bottommost mount at a dentry
; returns a mount if one is found; otherwise returns the starting mount
(define (lookup-mnt sys dent)
  (define mnt (lookup-one-mnt sys dent))
  (cond
    ; found no mount; return current mount
    [(not mnt) (dentry-mnt dent)]
    ; parent mount is same; we are at root
    [(equal? mnt (dentry-mnt dent)) mnt]
    ; go up one more
    [else (lookup-mnt sys (dentry mnt (mount-root mnt)))]))

; return the dentry we're at after mount traversal
(define (traverse-mounts sys dent)
  (define new-mnt (lookup-mnt sys dent))
  (if (equal? new-mnt (dentry-mnt dent))
    ; nothing is mounted here
    dent
    ; something is mounted here
    (dentry new-mnt (mount-root new-mnt))))

; return the dentry at which the topmost mount is mounted
(define (choose-mountpoint dent)
  (let ascend ([cur dent])
    (define cur-mnt (dentry-mnt cur))
    (define cur-mntpt (mount-mountpoint cur-mnt))
    (define parent-mnt (dentry-mnt cur-mntpt))
    (cond
      ; parent mount is same; we are at root
      [(equal? cur-mnt parent-mnt) cur-mntpt]
      ; this is the root of the parent mount; we go up again
      [(equal? (dentry-ino cur-mntpt) (mount-root parent-mnt))
        (ascend cur-mntpt)]
      ; we stop here
      [else cur-mntpt])))

; performs path traversal, returning a found dentry or an error
; namei starts from the process's root
(define (namei sys proc path)
  ; start from the process root
  ; iterate over path; for each entry:
  ; - look for the child with the appropriate name
  ; - is there something mounted here? go into mount; iterate until bottom mount
  (define is-rel (or (null? path) (not (equal? (car path) "/"))))
  ; would've used define-values but this upsets rosette
  (define start-dent (if is-rel (process-pwd proc) (process-root proc)))
  (define start-path (if is-rel path (cdr path)))
  (let walk-component ([cur start-dent] [next start-path])
    (define cur-mnt (dentry-mnt cur))
    (define cur-ino (dentry-ino cur))
    (cond
      ; done walking; return current dentry
      [(null? next) cur]
      ; otherwise we must be a directory
      [(not (inode-d? cur-ino)) 'ENOTDIR]
      ; . does nothing (?)
      [(equal? (car next) ".") (walk-component cur (cdr next))]
      ; ..
      [(equal? (car next) "..")
        (cond
          ; at our root; don't ascend
          [(equal? cur (process-root proc)) (walk-component (process-root proc) (cdr next))]
          ; at root of current mount
          [(equal? (dentry-ino cur) (mount-root cur-mnt))
            (if (equal? (mount-parent cur-mnt) cur-mnt)
              ; we are at the root mount
              (walk-component cur (cdr next))
              (begin
                ; go to topmost mountpoint
                (define mntpt (choose-mountpoint cur))
                (walk-component (traverse-mounts sys (dentry-parent mntpt)) (cdr next))))]
          ; just go to parent
          [else (walk-component (traverse-mounts sys (dentry-parent cur)) (cdr next))])]
      ; normal path; look for child
      [else
        (let ([found (assoc (car next) (inode-d-children cur-ino))])
          (if (not found)
            ; no child with appropriate name; error
            'ENOENT
            (begin
              (define next-dentry (dentry cur-mnt (cdr found)))
              (walk-component (traverse-mounts sys next-dentry) (cdr next)))))]
        )))

; copy each mount in a mount namespace
; returns new mnt-namespace
; modifies sys to include newly created mounts
(define (copy-mnt-namespace! sys ns)
  (define new-ns (mnt-namespace '() '()))
  ; iterate over all child mounts of namespace
  ; make a copy of each child mount under the new namespace
  ; associate new mounts with corresponding old mounts
  (define mount-copy-pairs
    (for/list ([mnt (mnt-namespace-children ns)])
      (cons mnt (mount new-ns '() (mount-root mnt)))))
  (define mount-copies (map cadr mount-copy-pairs))
  ; fix mountpoints to use corresponding new mount
  (for-each
    (lambda (pair)
      (define old-mnt (car pair))
      (define new-mnt (cadr pair))
      (define mntpt (mount-mountpoint old-mnt))
      (let ([found (assoc (dentry-mnt mntpt) mount-copy-pairs)])
        (if found
          (set-mount-mountpoint! (dentry (cadr found) (dentry-ino mntpt)))
          (error "copy of mount parent not found!"))))
    mount-copy-pairs)
  ; set new mount namespace root
  (set-mnt-namespace-root! new-ns
    (let ([found (assoc (mnt-namespace-root ns) mount-copy-pairs)])
      (if found (cadr found) (error "copy of root mount not found!"))))
  ; set new mount namespace children
  (set-mnt-namespace-children! new-ns mount-copies)
  ; add new mounts to system
  (add-sys-mounts! sys mount-copies)
  new-ns)

; === OPERATIONS ===

; all ops take the following two arguments:
; - sys: system state, which includes:
;   - the mount hashtable (a list of `mount`s)
;   - file system state (a list of `inode`s)
; - proc: the calling process
; TODO eventually: how valid is the serializability assumption anyway?

; returns an error or nothing
(define (syscall-chroot! sys proc path)
  (define new-root (namei sys proc path))
  (cond
    ; some path resolution error
    [(err? new-root) new-root]
    [(not (process-may-chroot proc)) 'EPERM]
    [else (set-process-root! proc new-root)]))

; accepts:
; - source: a device
; - target: a path
; - flags: TODO eventually: bind mounts, propagation properties
(define (syscall-mount! sys proc source target flags)
  (define ns (process-mnt-ns proc))
  ; find the parent mount and inode
  (define dent (namei sys proc target))
  (if (err? dent)
    dent
    (begin
      ; create a new mount w/ the indicated root directory and mountpoint
      (define new-mnt (mount (process-mnt-ns proc) source dent (device-root source)))
      ; add new mount and new inodes to system
      (add-sys-mounts! sys new-mnt)
      ; update mount namespace
      (set-mnt-namespace-children! ns (list* new-mnt (mnt-namespace-children ns))))))

(define (syscall-umount! sys proc target)
  (define ns (process-mnt-ns proc))
  ; look up mount object
  (define dent (namei sys proc target))
  (if (err? dent)
    dent
    (begin
      ; remove mount from mount list
      (define pair (assoc dent (system-mounts sys)))
      (when (not pair) (error "mount not found!"))
      (set-system-mounts! sys (remove pair (system-mounts sys)))
      ; remove mount from namespace
      (set-mnt-namespace-children! ns (remove (dentry-mnt dent) (mnt-namespace-children ns))))))

; return new proc
; as we model only mount namespaces at present, CLONE_NEWNS is the only interesting flag
(define (syscall-clone! sys proc flags pid)
  (define ns
    (if (member 'CLONE_NEWNS flags)
      (copy-mnt-namespace! sys (process-mnt-ns proc))
      (process-mnt-ns proc)))
  (define tgid
    (if (member 'CLONE_THREAD flags)
      (process-tgid proc)
      pid))
  (define new-proc
    (process
      tgid
      pid
      ns
      (process-root proc)
      (process-pwd proc)
      (process-fds proc)
      (process-may-chroot proc)))
  (sys-add-proc! sys pid new-proc)
  pid)

; as we model only mount namespaces at present, CLONE_NEWNS is the only interesting flag
(define (syscall-unshare! sys proc flags)
  (when (member 'CLONE_NEWNS flags)
    (define new-ns (copy-mnt-namespace! sys (process-mnt-ns proc)))
    (set-process-mnt-ns! new-ns)))

(define (syscall-mkdir! sys proc path)
  (define basename (car (reverse path)))
  (define dirname (reverse (cdr (reverse path))))
  (define parent (namei sys proc dirname))
  (define parent-ino (dentry-ino parent))
  (cond
    ; we must resolve parent dir
    [(err? parent) parent]
    [(not (inode-d? parent-ino)) 'ENOTDIR]
    [(not (err? (namei sys proc path))) 'EEXIST]
    [else
      (define new-ino (create-inode-d! (inode-dev parent-ino)))
      (add-inode-child! parent-ino basename new-ino)]))

(define (syscall-chdir! sys proc path)
  (define d (namei sys proc path))
  (cond
    [(err? d) d]
    [(not (inode-d? (dentry-ino d))) 'ENOTDIR]
    [else (set-process-pwd! proc d)]))

(define (syscall-fchdir! sys proc fd)
  (define d (assoc fd (process-fds proc)))
  (cond
    [(not d) 'EBADF]
    [(not (inode-d? (dentry-ino (cdr d)))) 'ENOTDIR]
    [else (set-process-pwd! proc (cdr d))]))

(define (syscall-open!
    sys proc path flags
    [fd (+ 1 (length (process-fds proc)))])
  (define f (namei sys proc path))
  (cond
    [(err? f) f]
    [else (add-proc-fd! proc fd f)]))

; TODO just a stand-in
(define (syscall-drop-cap-sys-chroot! sys proc)
  (set-process-may-chroot! proc #f))
