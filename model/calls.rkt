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
      ; parent mount is same; we are at root
      [(equal? mnt parent-mnt) (cons mnt mountpoint)]
      ; mountpoint is root of the parent mount; we go up again
      [(equal? mountpoint (mount-root parent-mnt))
        (ascend (cons parent-mnt mountpoint))]
      ; we stop here
      [else (cons mnt mountpoint)])))

; performs path traversal, returning a found dentry or an error
; namei starts from the process's root
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
    (define cur-mnt (car cur))
    (define cur-dent (cdr cur))
    (define cur-ino (dentry-ino cur-dent))
    (cond
      ; done walking; return current path
      [(null? next) cur]
      ; otherwise we had better be a directory
      [(not (inode-dir? cur-ino)) 'ENOTDIR]
      ; . does nothing (?)
      [(equal? (car next) ".") (walk-component cur (cdr next))]
      ; .. goes up
      [(equal? (car next) "..")
        (cond
          ; at process root; don't ascend
          [(equal? cur (process-root proc))
            (walk-component (process-root proc) (cdr next))]
          ; at root of current mount
          [(equal? cur-dent (mount-root cur-mnt))
            (if (equal? (mount-parent cur-mnt) cur-mnt)
              ; at root of actual root mount
              (walk-component cur (cdr next))
              ; otherwise, ascend mounts to topmost mountpoint
              (let* (
                  [mountpath (choose-mountpoint cur)]
                  [parent-dent (dentry-parent (cdr mountpath))]
                  [parent-mnt (car mountpath)]
                  [parent-path (cons parent-mnt parent-dent)])
                (walk-component (traverse-mounts sys parent-path) (cdr next))))]
          ; just go to parent
          [else
            (let* (
                [parent-dent (dentry-parent cur-dent)]
                [parent-path (cons cur-mnt parent-dent)])
              (walk-component (traverse-mounts sys parent-path) (cdr next)))])]
      ; normal path; look for child
      [else
        (let ([found (inode-lookup cur-ino (car next))])
          (if (not found)
            ; no child with appropriate name; error
            'ENOENT
            ; drop into found child
            (let* (
                [dent (dentry found (cdr cur))]
                [next-path (cons cur-mnt dent)])
              (walk-component (traverse-mounts sys next-path) (cdr next)))))])))

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
      (cons mnt (struct-copy mount mnt
        [mnt-ns new-ns]
        [parent '()]))))
  (define mount-copies (map cdr mount-copy-pairs))
  ; fix parent to use corresponding new mount
  (for-each
    (lambda (pair)
      (define old-mnt (car pair))
      (define new-mnt (cdr pair))
      (define parent (mount-parent old-mnt))
      (let ([found (assoc parent mount-copy-pairs)])
        (if found
          (set-mount-parent! (cdr found))
          (error "copy of mount parent not found!"))))
    mount-copy-pairs)
  ; set new mount namespace root
  (set-mnt-namespace-root! new-ns
    (let ([found (assoc (mnt-namespace-root ns) mount-copy-pairs)])
      (if found (cdr found) (error "copy of root mount not found!"))))
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
; LATER: how valid is the serializability assumption anyway?

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
    (begin
      ; remove mount from mount list
      (define pair (assoc path (system-mounts sys)))
      (when (not pair) (error "mount not found!"))
      (set-system-mounts! sys (remove pair (system-mounts sys)))
      ; remove mount from namespace
      (set-mnt-namespace-children! ns (remove (car path) (mnt-namespace-children ns))))))

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
    (struct-copy process proc
      [tgid tgid]
      [pid pid]
      [mnt-ns ns]))
  (sys-add-proc! sys pid new-proc)
  pid)

; as we model only mount namespaces at present, CLONE_NEWNS is the only interesting flag
(define (syscall-unshare! sys proc flags)
  (when (member 'CLONE_NEWNS flags)
    (define new-ns (copy-mnt-namespace! sys (process-mnt-ns proc)))
    (set-process-mnt-ns! new-ns)))

(define (syscall-mkdir! sys proc name)
  (define basename (car (reverse name)))
  (define dirname (reverse (cdr (reverse name))))
  (define parent (namei sys proc dirname))
  (define parent-ino (dentry-ino (cdr parent)))
  (cond
    ; we must resolve parent dir
    [(err? parent) parent]
    [(not (inode/dir? parent-ino)) 'ENOTDIR]
    [(not (err? (namei sys proc name))) 'EEXIST]
    [else
      (define new-ino (create-inode/dir! (inode-dev parent-ino)))
      (add-inode-child! parent-ino basename new-ino)]))

(define (syscall-chdir! sys proc name)
  (define path (namei sys proc name))
  (cond
    [(err? path) path]
    [(not (inode/dir? (dentry-ino (cdr path)))) 'ENOTDIR]
    [else (set-process-pwd! proc path)]))

(define (syscall-fchdir! sys proc fd)
  (define d (assoc fd (process-fds proc)))
  (cond
    [(not d) 'EBADF]
    [(not (inode/dir? (dentry-ino (cddr d)))) 'ENOTDIR]
    [else (set-process-pwd! proc (cddr d))]))

(define (syscall-open!
    sys proc name flags
    [fd (+ 1 (length (process-fds proc)))])
  (define f (namei sys proc name))
  (cond
    [(err? f) f]
    [else (add-proc-fd! proc fd f)]))

; LATER just a stand-in
(define (syscall-drop-cap-sys-chroot! sys proc)
  (set-process-may-chroot! proc #f))
