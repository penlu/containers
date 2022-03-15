#lang rosette

(require "base.rkt")
(provide (all-defined-out))

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
            (define ino (inode/proc-dir (device-num dev) dev proc))
            (set-device-num! dev (+ 1 (device-num dev)))
            (set-device-inodes! dev (list* ino (device-inodes dev)))
            ino)
          'ENOENT)))])

; TODO this thing is supposed to die when the proc goes away
; TODO this generates eexist upon attempting to create folder, fix that
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
            (define ino (inode/proc-ns-dir (device-num dev) dev proc))
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
            (define ino (inode/ns (device-num dev) dev (process-mnt-ns proc)))
            (set-device-num! dev (+ 1 (device-num dev)))
            (set-device-inodes! dev (list* ino (device-inodes dev)))
            ino)
          'ENOENT)))])
