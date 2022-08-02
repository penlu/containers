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

; generate symbolic path
(define (sym-paths pathlen [root #t])
  (cond
    [root
      (define-symbolic* is-root boolean?)
      (let ([paths (sym-paths pathlen #f)])
        (if is-root
          (cons "/" paths)
          paths))]
    [(equal? pathlen 0) '()]
    [else
      (define-symbolic* is-dir boolean?)
      (let ([paths (sym-paths (- pathlen 1) #f)])
        (if is-dir
          (cons (choose* "." ".." "a" "b") paths)
          '()))]
    ))

; enumerate calls with all paths
(define (enumerate-call pathlen)
  (let ([paths (enumerate-paths pathlen)])
    (append
      (for/list ([path paths]) (call-open path))
      (for/list ([path paths]) (call-mkdir path))
      (for/list ([path paths]) (call-chdir path))))
  )

; generate one symbolic call
(define (sym-call pathlen)
  (let ([paths (sym-paths pathlen)])
    (choose*
      (call-open paths)
      (call-mkdir paths)
      (call-chdir paths))))

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

; enumerate traces with symbolic paths
(define (enumerate-calls-sym-path num pathlen)
  (cond
    [(equal? num 0) (list '())]
    [else
      (let ([paths (sym-paths pathlen)])
        (for*/list (
            [trace (enumerate-calls-sym-path (- num 1) pathlen)]
            [call (list (call-open paths) (call-mkdir paths) (call-chdir paths))])
          (cons call trace)))]
  ))

; symbolic trace for one process
; num - number of system calls in trace
; pathlen - length of paths
(define (sym-calls num pathlen)
  (cond
    [(equal? num 0) '()]
    [else
      (let ([trace (sym-calls (- num 1) pathlen)]
            [call (sym-call pathlen)])
        (cons call trace))]
    ))

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

;(let ([paths (enumerate-paths 2)])
;  (printf "path test: ~v\nlength: ~v\n" paths (length paths)))
;(let ([traces (enumerate-calls 2 2)])
;  (printf "call test: ~v\nlength: ~v\n" traces (length traces)))

(define (print-all-tests)
  (let (
      [tests (enumerate 0 2 2)]
      [f (open-output-file "traces.txt" #:exists 'replace)])
    (for-each (lambda (test) (print-test f test)) tests))
  )

(define (print-all-sym-tests)
  (let ([tests (enumerate-calls-sym-path 2 2)])
    (for-each (lambda (test) (printf "~v\n" test)) tests)
    (printf "length: ~v\n" (length tests))
    ;(printf "~v\n" tests)
    ))

; given trace, return lists of possible retvals for the calls in the trace
; returns every feasible trace
(define (enumerate-retvals trace)
  (cond
    [(equal? trace '()) (list '())]
    [else
      (for*/list (
          [retval (match (car trace)
            [(call-open p) (list 'ENOTDIR 'ENOENT (void))]
            [(call-mkdir p) (list 'ENOTDIR 'ENOENT 'EEXIST (void))]
            [(call-chdir p) (list 'ENOTDIR 'ENOENT 'EEXIST (void))])]
          [retvals (enumerate-retvals (cdr trace))])
        (cons retval retvals))]
    ))

; return: list of tuples (system calls, return values)
; symbolic paths on the calls
; for each possible distinct return value
(define (sym-enumerate inum num pathlen)
  (for* (
      [trace (enumerate-calls-sym-path num pathlen)]
      [retvals (enumerate-retvals trace)])
    (when (not (equal? (length trace) (length retvals)))
      (error "trace, retvals not same length!"))
    (let* (
        [sys (create-sys)]
        [proc (sys-get-proc sys 1)])
      (syscall-umount! sys proc (list "/" "proc"))
      (let ([model (solve
        (for ([call trace] [ret retvals])
          (assert (equal? (interpret-call sys proc call) ret))))])
        (if (sat? model)
          (printf "found example: ~v\n"
            (evaluate trace
              (complete-solution model (symbolics trace))))
          (printf "no examples found!\n")))
      ;(let ([cur-vc (vc)])
      ;  (printf "assumes: ~v\n" (vc-assumes cur-vc))
      ;  (printf "asserts: ~v\n" (vc-asserts cur-vc)))
      )))

(sym-enumerate 0 2 2)
