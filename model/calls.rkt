#lang rosette

(require "struct.rkt")
(provide (all-defined-out))

; === METHODS ===

; a name is a list of strings
; struct path is (mount . dentry)

; see if something is mounted at the given dentry
; returns a mount if one is found; otherwise returns #f
(define (lookup-mnt sys path)
  ; search the mount list for a mount such that:
  ; - its parent is mnt
  ; - its mountpoint is dent
  (define mnt (car path))
  (define dent (cdr path))
  (define (pred entry)
    (let ([entry-dent (car entry)]
          [entry-mnt (cdr entry)])
      (and (equal? dent entry-dent)
      (equal? mnt (mount-parent entry-mnt)))))
  (let ([found (findf pred (system-mounts sys))])
    (if found (cdr found) #f)))

; find the bottommost mount at a path
; returns starting path if none is found
; otherwise, returns path corresponding to the root of the found mount
(define (traverse-mounts sys path)
  (define mnt (lookup-mnt sys path))
  (cond
    ; found no mount; return current mount
    [(not mnt) path]
    ; arrived at same mount; we are at FS root
    [(equal? mnt (car path)) path]
    ; go down one more
    [else
      (let ([next (cons mnt (mount-root mnt))])
        (traverse-mounts sys next))]))

; try to traverse upward given a path that is the root of a mount
; return the path of the topmost mount's mountpoint
(define (choose-mountpoint path)
  (let ascend ([cur path])
    (define mnt (car cur))
    (define parent-mnt (mount-parent mnt))
    (define mountpoint (mount-mountpoint mnt))
    (cond
      ; we are at root of this mount and parent mount is the same
      ; this means we are at the root mount and should stop
      [(and
        (equal? (cdr path) mountpoint)
        (equal? mnt parent-mnt)) (cons mnt mountpoint)]
      ; mountpoint is root of the parent mount; we go up again
      [(equal? mountpoint (mount-root parent-mnt))
        (ascend (cons parent-mnt mountpoint))]
      ; we stop here
      [else (cons parent-mnt mountpoint)])))

; performs path traversal, returning a found path or an error
; namei starts from the process's root
; path is a pair (mount . dentry)
; XXX weird point of interpretation: '() is not (list ".")
; XXX linux is weird about this too; further investigation needed
(define (namei sys proc name)
  ; start from the process root
  ; iterate over name; for each entry:
  ; - look for the child with the appropriate name
  ; - is there something mounted here? go into mount; iterate until bottom mount
  (define is-rel (or (null? name) (not (equal? (car name) "/"))))
  ; would've used define-values but this upsets rosette
  (define start-path (if is-rel (process-pwd proc) (process-root proc)))
  (define start-name (if is-rel name (cdr name)))
  (let walk-component ([cur start-path] [next start-name])
    (for*/all (
        [cur cur]
        [cur-mnt (car cur)]
        [cur-dent (cdr cur)]
        [cur-ino (dentry-ino cur-dent)])
      (cond
        ; done walking; return current path
        [(null? next) cur]
        ; otherwise we had better be a directory
        [(not (inode-dir? cur-ino)) 'ENOTDIR]
        ; . goes into mount when . was mounted upon
        [(equal? (car next) ".")
          (walk-component (traverse-mounts sys cur) (cdr next))]
        ; .. goes up
        [(equal? (car next) "..")
          (cond
            ; at process root; don't ascend
            [(equal? cur (process-root proc))
              (walk-component (process-root proc) (cdr next))]
            ; at root of current mount; ascend to topmost mountpoint
            [(equal? cur-dent (mount-root cur-mnt))
              (let* (
                  [mountpath (choose-mountpoint cur)]
                  [parent-dent (dentry-parent (cdr mountpath))]
                  [parent-mnt (car mountpath)]
                  [parent-path (cons parent-mnt parent-dent)])
                (walk-component (traverse-mounts sys parent-path) (cdr next)))]
            ; no root encounters: just go to parent
            [else
              (let* (
                  [parent-dent (dentry-parent cur-dent)]
                  [parent-path (cons cur-mnt parent-dent)])
                (walk-component (traverse-mounts sys parent-path) (cdr next)))])]
        ; normal path; look for child
        [else
          (let ([found (inode-lookup cur-ino (car next))])
            (cond
              ; no child with appropriate name; return error
              [(not found) 'ENOENT]
              ; lookup error; return
              [(err? found) found]
              ; drop into found child
              [else
                (let* (
                    [dent (dentry found (cdr cur))]
                    [next-path (cons cur-mnt dent)])
                  (walk-component (traverse-mounts sys next-path) (cdr next)))]))]
        ))))

; replace mnt-ns of proc with a copy
; - also copies each mount in the mount namespace
; - modifies sys to include newly created mounts
; - modifies proc root & pwd to use the new mounts
(define (copy-mnt-namespace! sys proc)
  (define ns (process-mnt-ns proc))

  ; create new ns struct
  (define inum (system-inum sys))
  (set-system-inum! sys (+ 1 (system-inum sys)))
  (define new-ns (mnt-namespace inum '() '()))

  ; iterate over all child mounts of namespace
  ; make a copy of each child mount under the new namespace
  ; associate old mount -> new mount
  (define mount-copy-pairs
    (for/list ([mnt (mnt-namespace-children ns)])
      (cons mnt (struct-copy mount mnt
        [mnt-ns new-ns]))))
  (define mount-copies (map cdr mount-copy-pairs))

  ; helper to look up new mount corresponding to old mount
  (define (lookup-copy mnt)
    (let ([found (assoc mnt mount-copy-pairs)])
      (if found
        (cdr found)
        (error "copy of mount not found!"))))

  ; update copied mounts' parents to use copied mounts
  (for-each
    (lambda (mnt)
      (set-mount-parent! mnt (lookup-copy (mount-parent mnt))))
    mount-copies)

  ; add new mounts to system
  (add-sys-mounts! sys mount-copies)

  ; set new mount namespace root
  (set-mnt-namespace-root! new-ns (lookup-copy (mnt-namespace-root ns)))
  ; set new mount namespace children
  (set-mnt-namespace-children! new-ns mount-copies)

  ; update proc mnt-ns, root, pwd
  (set-process-mnt-ns! proc new-ns)
  (set-process-root! proc
    (let ([cur-root (process-root proc)])
      (cons (lookup-copy (car cur-root)) (cdr cur-root))))
  (set-process-pwd! proc
    (let ([cur-pwd (process-pwd proc)])
      (cons (lookup-copy (car cur-pwd)) (cdr cur-pwd))))
  )

; === SYSCALLS ===

; all syscalls take the following two arguments:
; - sys: system state, which includes:
;   - the mount hashtable (a list of `mount`s)
;   - file system state (a list of `inode`s)
; - proc: the calling process
; LATER: (much later) how valid is the serializability assumption anyway?

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
; - flags: LATER: bind mounts, propagation properties
(define (syscall-mount! sys proc source target flags)
  (define ns (process-mnt-ns proc))
  ; find the parent mount and inode
  (define path (namei sys proc target))
  (if (err? path)
    path
    ; create a new mount w/ the indicated root directory and mountpoint
    (let ([new-mnt (mount
        (process-mnt-ns proc)
        source
        (car path)
        (cdr path)
        (device-root source))])
      ; add new mount and new inodes to system
      (add-sys-mounts! sys new-mnt)
      ; update mount namespace
      (set-mnt-namespace-children! ns (list* new-mnt (mnt-namespace-children ns))))))

(define (syscall-umount! sys proc target)
  (define ns (process-mnt-ns proc))
  ; look up mount object
  (define path (namei sys proc target))
  (if (err? path)
    path
    (cond
      [(not (equal? (cdr path) (mount-root (car path)))) 'EINVAL]
      ; TODO refuse to remove root mount
      [else (let* (
          [mnt (car path)]
          [mnt-entry (cons (mount-mountpoint mnt) mnt)])
        ; ensure umount target is in system mount list
        (when (not (member mnt-entry (system-mounts sys)))
          (error "mount missing from system-mounts!"))
        ; ensure umount target is in mount namespace children
        (when (not (member mnt (mnt-namespace-children ns)))
          (error "mount missing from mnt-namespace-children!"))
        ; remove mount from mount list
        (set-system-mounts! sys (remove mnt-entry (system-mounts sys)))
        ; remove mount from namespace
        (set-mnt-namespace-children! ns (remove mnt (mnt-namespace-children ns))))]
      )))

; return new proc
; we support CLONE_NEWNS, CLONE_THREAD, and CLONE_FS
(define (syscall-clone! sys proc flags pid)
  (cond
    [(and (member 'CLONE_FS flags) (member 'CLONE_NEWNS flags))
      'EINVAL]
    [else
      (define tgid
        (if (member 'CLONE_THREAD flags)
          (process-tgid proc)
          pid))
      (define new-proc
        (struct-copy process proc
          [tgid tgid]
          [pid pid]))
      ; copy fs info
      (when (not (member 'CLONE_FS flags))
        (set-process-fs! new-proc (struct-copy fs-struct (process-fs proc))))
      ; copy mount namespace
      (when (member 'CLONE_NEWNS flags)
        (copy-mnt-namespace! sys new-proc))
      (sys-add-proc! sys pid new-proc)
      pid]
    ))

; as we model only mount namespaces at present, CLONE_NEWNS is the only interesting flag
(define (syscall-unshare! sys proc flags)
  (when (member 'CLONE_NEWNS flags)
    (copy-mnt-namespace! sys proc)))

(define (syscall-mkdir! sys proc name)
  (define basename (car (reverse name)))
  (define dirname (reverse (cdr (reverse name))))
  (define parent (namei sys proc dirname))
  (for/all ([parent parent])
    (if (err? parent)
      parent
      (for/all ([parent-dent (cdr parent)])
        (define parent-ino (dentry-ino parent-dent))
        (when (union? parent-ino) (error "mkdir: parent-ino is union, needs handling"))
        (cond
          ; we must resolve parent dir
          [(err? parent) parent]
          [(not (inode-dir? parent-ino)) 'ENOTDIR]
          [(not (err? (namei sys proc name))) 'EEXIST]
          [else
            (define new-ino (create-inode/dir! (inode-dev parent-ino)))
            (add-inode-child! parent-ino basename new-ino)])))))

(define (syscall-chdir! sys proc name)
  (define path (namei sys proc name))
  (for/all ([path path])
    (if (err? path)
      path
      (for/all ([dent (cdr path)])
        (if (not (inode-dir? (dentry-ino dent)))
          'ENOTDIR
          (set-process-pwd! proc (cons (car path) dent)))))))

(define (syscall-fchdir! sys proc fd)
  (define d (proc-get-fd proc fd))
  (cond
    [(not d) 'EBADF]
    [(not (inode-dir? (dentry-ino (cdr d)))) 'ENOTDIR]
    [else (set-process-pwd! proc d)]))

(define (syscall-open!
    sys proc name flags
    [fd (+ 1 (length (process-fds proc)))])
  (define f (namei sys proc name))
  (cond
    [(err? f) f]
    [else (proc-add-fd! proc fd f)]))

(define (syscall-close! sys proc fd)
  (cond
    [(not (proc-get-fd proc fd)) 'EBADF]
    [else
      (proc-rm-fd! proc fd)]))

; LATER just a stand-in
(define (syscall-drop-cap-sys-chroot! sys proc)
  (set-process-may-chroot! proc #f))

; LATER capabilities
; TODO should fail when proc fs is shared
(define (syscall-setns! sys proc fd nstype)
  (define f (proc-get-fd proc fd))
  (cond
    [(not f) 'EBADF]
    [(not (inode-ns? (dentry-ino (cdr f)))) 'EINVAL]
    [else
      (define ns (inode-ns (dentry-ino (cdr f))))
      (cond
        [(and (or (equal? (car nstype) 'CLONE_NEWNS)
                  (equal? (car nstype) 0))
              (mnt-namespace? ns))
          (set-process-mnt-ns! proc ns)
          ; update process root and pwd dentry for new mounts
          (define root (mnt-namespace-root ns))
          (define root-path (cons root (mount-root root)))
          (set-process-root! proc root-path)
          (set-process-pwd! proc root-path)
          ]
        ; more ns types go here
        [else 'EINVAL])]
    ))

(define (path-is-mountpoint path)
  (define mnt (car path))
  (define dentry (cdr path))
  (equal? dentry (mount-root mnt)))

; TODO I don't think pivot-root "." "." works
(define (syscall-pivot-root! sys proc new-root put-old)
  (define new-root-path (namei sys proc new-root))
  (define put-old-path (namei sys proc put-old))
  (define cur-root (process-root proc))
  (define cur-root-mnt (mnt-namespace-root (process-mnt-ns proc)))
  (for*/all ([new-root-path new-root-path] [put-old-path put-old-path])
    (cond
      [(err? new-root-path) new-root-path]
      [(err? put-old-path) put-old-path]
      ; new-root is not a dir
      [(not (inode-dir? (dentry-ino (cdr new-root-path))))
        'ENOTDIR]
      ; put-old is not a dir
      [(not (inode-dir? (dentry-ino (cdr put-old-path))))
        'ENOTDIR]
      ; current root is not a mountpoint
      [(not (path-is-mountpoint cur-root))
        'EINVAL]
      ; LATER how to determine if put-old is underneath new-root?
      ; new-root is not a mountpoint
      [(not (path-is-mountpoint new-root-path))
        'EINVAL]
      [(equal? (car new-root-path) cur-root-mnt)
        'EBUSY]
      [(equal? (car put-old-path) cur-root-mnt)
        'EBUSY]
      [else
        (define new-root-mnt (car new-root-path))
        ; remove mounts from system mount list
        (define new-root-entry (cons (mount-mountpoint new-root-mnt) new-root-mnt))
        (define cur-root-entry (cons (mount-mountpoint cur-root-mnt) cur-root-mnt))
        (set-system-mounts! sys (remove new-root-entry (system-mounts sys)))
        (set-system-mounts! sys (remove cur-root-entry (system-mounts sys)))
        ; change parent/child relationship on each mount
        (set-mount-parent! new-root-mnt new-root-mnt)
        (set-mount-parent! cur-root-mnt new-root-mnt)
        ; change mountpoint on each mount
        (set-mount-mountpoint! new-root-mnt (mount-root new-root-mnt))
        (set-mount-mountpoint! cur-root-mnt (cdr put-old-path))
        ; add mounts back to system mount list
        (add-sys-mounts! sys (list cur-root-mnt new-root-mnt))
        ; change mnt namespace root
        (set-mnt-namespace-root! (process-mnt-ns proc) new-root-mnt)

        ; TODO move all procs with a root or pwd on old root to new root
        ; for now just moving calling proc
        (when (and
            (equal? (car (process-root proc)) cur-root-mnt)
            (equal? (cdr (process-root proc)) (mount-root cur-root-mnt)))
          (set-process-root! proc (cons new-root-mnt (mount-root new-root-mnt))))
        ]
    ))
  )

(define (syscall-pseudo-mount! sys proc name target)
  (define ns (process-mnt-ns proc))
  ; find the parent mount and inode
  (define path (namei sys proc target))
  (if (err? path)
    path
    ; create a new mount w/ the indicated root directory and mountpoint
    (let* (
        [new-dev (create-device! sys name)]
        [new-mnt (mount
          (process-mnt-ns proc)
          new-dev
          (car path)
          (cdr path)
          (device-root new-dev))])
      ; add new mount and new inodes to system
      (add-sys-mounts! sys new-mnt)
      ; update mount namespace
      (set-mnt-namespace-children! ns (list* new-mnt (mnt-namespace-children ns))))))
