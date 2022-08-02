#lang rosette

(require
  rosette/lib/angelic rosette/lib/match rosette/lib/synthax
  "../model/struct.rkt" "../model/calls.rkt" "../model/trace.rkt")

; enumerate syscall traces
; ideally we bucket these by point of return

(define (enumerate-paths pathlen [root #t])
  (cond
    [root
      (let ([paths (enumerate-paths pathlen #f)])
        (append paths
          (map (lambda (path) (cons "/" path)) paths)))]
    [(equal? pathlen 0) (list '())]
    [else
      (for*/list (
          [path (enumerate-paths (- pathlen 1) #f)]
          [component (list "." ".." "a" "b")])
        (cons component path))]
    ))

(define (enumerate-call pathlen)
  (let ([paths (enumerate-paths pathlen)])
    (append
      (for/list ([path paths]) (call-open path))
      (for/list ([path paths]) (call-mkdir path))
      (for/list ([path paths]) (call-chdir path))))
  )

; enumerate traces for one process
; num - number of system calls
; pathlen - length of paths
(define (enumerate-calls num pathlen)
  (cond
    [(equal? num 0) (list '())]
    [else
      (for*/list (
          [trace (enumerate-calls (- num 1) pathlen)]
          [call (enumerate-call pathlen)])
        (cons call trace))])
  )

; enumerate systems
; inum - number of inodes
;(define (enumerate-systems inum)
;  '()
;  )

; inum - number of inodes
; num - number of system calls
; pathlen - length of paths
; return: list of tuples (initial system, system calls, final system, return values)
(define (enumerate inum num pathlen)
  (for/list ([trace (enumerate-calls num pathlen)])
    (begin
      (printf "current trace: ~v\n" trace)
      (define sys (create-sys))
      (define proc (sys-get-proc sys 1))
      (syscall-umount! sys proc (list "/" "proc"))
      (list trace sys (interpret-calls sys proc trace))
      )))

(define (path-to-string path)
  (if (equal? (car path) "/")
    (string-append "/" (string-join (cdr path) "/"))
    (string-join path "/")))

(define (print-test f test)
  (for-each
    (lambda (t)
      (let ([call (trace-call t)]
            [retval (trace-retval t)])
        (match call
          [(call-open path) (fprintf f "open ~v = ~v ; " (path-to-string path) retval)]
          [(call-mkdir path) (fprintf f "mkdir ~v = ~v ; " (path-to-string path) retval)]
          [(call-chdir path) (fprintf f "chdir ~v = ~v ; " (path-to-string path) retval)]
          )))
    (caddr test))
  (fprintf f "\n"))

;(printf "path test: ~v\n" (enumerate-paths 2))
;(let ([traces (enumerate-calls 2 2)])
;  (printf "call test: ~v\nlength: ~v\n" traces (length traces)))
(let (
    [tests (enumerate 0 2 2)]
    [f (open-output-file "traces.txt" #:exists 'replace)])
  (for-each (lambda (test) (print-test f test)) tests))
