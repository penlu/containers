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
            ;[call (list (call-open paths) (call-mkdir paths) (call-chdir paths))]
            [call (list (call-open paths))]
            )
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
  (for/list ([calls (enumerate-calls num pathlen)])
    (begin
      (printf "current trace: ~v\n" calls)
      (define sys (create-sys))
      (define proc (sys-get-proc sys 1))
      (syscall-umount! sys proc (list "/" "proc"))
      (list calls sys (interpret-calls sys proc calls))
      )))

(define (path-to-string path)
  (cond
    [(equal? path '()) "."]
    [(equal? (car path) "/") (string-append "/" (string-join (cdr path) "/"))]
    [else (string-join path "/")]))

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

;(print-all-tests)

(define (print-all-sym-tests)
  (let ([tests (enumerate-calls-sym-path 2 2)])
    (for-each (lambda (test) (printf "~v\n" test)) tests)
    (printf "length: ~v\n" (length tests))
    ;(printf "~v\n" tests)
    ))

; given trace, return lists of possible retvals for the calls in the trace
; returns every feasible trace
(define (enumerate-retvals t)
  (cond
    [(equal? t '()) (list '())]
    [else
      (for*/list (
          [retval (match (car t)
            [(call-open p) (list (void) 'ENOTDIR 'ENOENT)]
            ;[(call-open p) (list 'ENOTDIR 'ENOENT (void))]
            [(call-mkdir p) (list 'ENOTDIR 'ENOENT 'EEXIST (void))]
            [(call-chdir p) (list 'ENOTDIR 'ENOENT 'EEXIST (void))])]
          [retvals (enumerate-retvals (cdr t))])
        (cons retval retvals))]
    ))

; return: list of tuples (system calls, return values)
; return one example of a concrete trace
; for each possible distinct series of return values
(define (enumerate-representatives inum num pathlen)
  (apply append (for*/list (
      [t (enumerate-calls-sym-path num pathlen)]
      [retvals (enumerate-retvals t)])
    (printf "current retvals: ~v\n" retvals)
    (when (not (equal? (length t) (length retvals)))
      (error "trace, retvals not same length!"))
    (let* (
        [sys (create-sys)]
        [proc (sys-get-proc sys 1)])
      (syscall-umount! sys proc (list "/" "proc"))
      (let ([model (solve
        (for ([call t] [ret retvals])
          (assert (equal? (interpret-call sys proc call) ret))))])
        (if (sat? model)
          (let ([ctrace (evaluate t (complete-solution model (symbolics t)))])
            (list (for/list (
                [call ctrace]
                [ret retvals])
              (trace call ret))))
          '()
          ))
      ))))

(define (print-representative-sym-tests)
  (let (
      [tests (enumerate-representatives 0 3 2)]
      [f (open-output-file "representatives.txt" #:exists 'replace)])
    (for-each (lambda (test) (print-test f (list '() '() test))) tests)
    ))

;(print-representative-sym-tests)

(define (check-crashfree inum num pathlen)
  (for ([t (enumerate-calls-sym-path num pathlen)])
    ; check for whether model was crash-free on this trace
    (let* (
        [sys (create-sys)]
        [proc (sys-get-proc sys 1)])
      (syscall-umount! sys proc (list "/" "proc"))
      (clear-vc!)
      (let ([m (verify (interpret-calls sys proc t))])
        (if (sat? m)
          ; we can crash model with a concrete input?
          (let ([bad (evaluate t (complete-solution m (symbolics t)))])
            ;(printf "model: ~v\n" m)
            (printf "bug input: ~v\n" bad)
            (let* (
                [sys (create-sys)]
                [proc (sys-get-proc sys 1)])
              (printf "~v\n" (interpret-calls sys proc bad)))
            )
          (printf "ok\n"))
        ))
    ))

;(check-crashfree 0 2 2)

; return: list of tuples (system calls, return values)
; return traces with symbolic paths & a constraint
; for each possible distinct series of return values
(define (enumerate-distinct inum num pathlen)
  (apply append (for*/list (
      [t (enumerate-calls-sym-path num pathlen)]
      [retvals (enumerate-retvals t)]
      )
    (when (not (equal? (length t) (length retvals)))
      (error "trace, retvals not same length!"))
    (let* (
        [sys (create-sys)]
        [proc (sys-get-proc sys 1)])
      (syscall-umount! sys proc (list "/" "proc"))
      (clear-vc!)
      (let (
          [a (for/fold ([conj #t]) ([call t] [ret retvals])
            (and (equal? (interpret-call sys proc call) ret) conj))])
        ;(printf "assumes: ~v\nasserts:~v\n" (vc-assumes (vc)) (vc-asserts (vc)))
        (let ([x (vc-asserts (vc))])
          (with-vc
            (when (sat? (verify (assert x)))
              (printf "oh no\n"))))
        (if (sat? (solve (assert a)))
          (list (list a t retvals))
          '())
        )))))

(define (bool-to-string b)
  (match b
    ;[(term content type) (printf "term with content ~v type ~v\n" content type)]
    [(constant id type)
      ;(printf "constant with id ~v ~v type ~v\n" (syntax->datum (car id)) (cadr id) type)
      (string-append (symbol->string (syntax->datum (car id))) (number->string (cadr id)))
      ]
    [(expression op child ...)
      ;(printf "expression with op ~v child ~v\n" op child)
      (let* (
          [op-str
            (cond
              [(equal? op &&) "#and"]
              [(equal? op ||) "#or"]
              [(equal? op !) "#not"])]
          [op-strs (make-list (- (length child) 1) op-str)]
          [child-strs
            (for/list ([c child])
              (bool-to-string c))]
          )
          (string-join (append op-strs child-strs) " ")
        )
      ]))

(define (symstr-to-string s)
  (cond
    [(concrete? s)
      (printf "concrete! ~v\n" s)
      (if (equal? s '())
        "\".\""
        (string-append "\"" s "\""))
      ]
    [(union? s)
      (printf "union! ~v\n" s)
      (let* (
          [union-strs (for/list ([u (union-contents s)])
            (let ([b (car u)] [x (cdr u)])
              (printf "union term: ~v ~v\n" b x)
              (cons (bool-to-string b) (symstr-to-string x))
              ))])
        ;(printf "~v\n" (cdar (reverse union-strs)))
        (if (equal? (length union-strs) 1)
          (cdar union-strs)
          (string-append
            (string-join
              (map
                (lambda (x) (string-append (car x) " " (cdr x)))
                  (reverse (cdr (reverse union-strs))))
              " #ite " #:before-first "#ite ")
            " "
            (cdar (reverse union-strs))))
        )
      ]
    [(list? s)
      (printf "list! ~v\n" s)
      (string-append
        "#concat #concat "
        (symstr-to-string (car s))
        " \"/\" "
        (symstr-to-string (cdr s)))
      ]
    [else
      (match s
        [(constant id type)
          (printf "constant with id ~v ~v type ~v\n" (syntax->datum (car id)) (cadr id) type)
          (string-append (symbol->string (syntax->datum (car id))) (number->string (cadr id)))
          ]
        [(expression op child ...)
          (printf "expression with op ~v child ~v\n" op child)
          (let* (
              [op-str
                (cond
                  [(equal? op &&) "#and"]
                  [(equal? op ||) "#or"]
                  [(equal? op !) "#not"]
                  [else "#???"])]
              [op-strs (make-list (- (length child) 1) op-str)]
              [child-strs
                (for/list ([c child])
                  (bool-to-string c))]
              )
              (string-join (append op-strs child-strs) " ")
            )
          ]
        )]
    )
  ;(exit 0)
  )

(define (print-sym-test f test)
  (let ([a (car test)]
        [t (cadr test)]
        [r (caddr test)])
    ;(display (string-append "#cond " (bool-to-string a) " ; ") f)
    (fprintf f "#cond ~a ; " (bool-to-string a))
    (for (
        [c t]
        [retval r])
      (match c
        [(call-open path) (fprintf f "open ~a = ~v ; " (symstr-to-string path) retval)]
        [(call-mkdir path) (fprintf f "mkdir ~a = ~v ; " (symstr-to-string path) retval)]
        [(call-chdir path) (fprintf f "chdir ~a = ~v ; " (symstr-to-string path) retval)]
        ))
    ))

(define (print-sym-tests)
  (let (
      [tests (enumerate-distinct 0 1 2)]
      [f (open-output-file "distinct.txt" #:exists 'replace)])
    (for-each (lambda (test) (print-sym-test f test)) tests)
    ))

(print-sym-tests)
