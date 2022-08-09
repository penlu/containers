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
            [call (list (call-open paths) (call-mkdir paths) (call-chdir paths))]
            )
          (cons call trace)))]
  ))

; generate one symbolic call
(define (sym-call pathlen)
  (let ([paths (sym-paths pathlen)])
    (choose*
      (call-open paths)
      (call-mkdir paths)
      (call-chdir paths))))

; fully symbolic trace for one process
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

; one test with final sys, syscall trace, and return values
(struct test (asn sys calls retvals))

(define (prepare-system)
  (let* (
      [sys (create-sys)]
      [proc (sys-get-proc sys 1)])
    (syscall-umount! sys proc (list "/" "proc"))
    (values sys proc)))

; inum - number of inodes
; num - number of system calls
; pathlen - length of paths
; return: list of tuples (initial system, system calls, final system, return values)
(define (enumerate-concrete inum num pathlen)
  (for/list ([calls (enumerate-calls num pathlen)])
    (printf "current calls: ~v\n" calls)
    (let-values ([(sys proc) (prepare-system)])
      (test '() sys calls (map (lambda (t) (trace-retval t) (interpret-calls sys proc calls))))
      )))

; given: callseq
; check whether any execution produces a crash
(define (check-crashfree calls)
  ; check for whether model was crash-free on this callseq
  (let-values ([(sys proc) (prepare-system)])
    (let* ([m (verify (interpret-calls sys proc calls))])
      (if (sat? m)
        ; we can crash model with a concrete input?
        (let ([bad (evaluate calls (complete-solution m (symbolics calls)))])
          (printf "model: ~v\n" m)
          (printf "bug input: ~v\n" bad)
          (let-values ([(sys proc) (prepare-system)])
            (printf "~v\n" (interpret-calls sys proc bad)))
          #f
          )
        (begin
          (printf "ok\n")
          #t
          ))
      ))
  )

; given trace, return lists of possible retvals for the calls in the trace
; returns every feasible trace
(define (enumerate-retvals calls)
  (cond
    [(equal? calls '()) (list '())]
    [else
      (for*/list (
          [retval (match (car calls)
            [(call-open p) (list 'ENOTDIR 'ENOENT (void))]
            [(call-mkdir p) (list 'ENOTDIR 'ENOENT 'EEXIST (void))]
            [(call-chdir p) (list 'ENOTDIR 'ENOENT 'EEXIST (void))])]
          [retvals (enumerate-retvals (cdr calls))])
        (cons retval retvals))]
    ))

; return: list of tests
; for each distinct series of return values
; return one example of a concrete trace
(define (enumerate-representatives calls)
  ; check crash-freedom
  (printf "enumerating representatives\n")
  (print-calls calls)
  (if (check-crashfree calls)
    (apply append (for*/list ([retvals (enumerate-retvals calls)])
      (printf "current retvals: ~v\n" retvals)
      (when (not (equal? (length calls) (length retvals)))
        (error "callseq, retvals not same length!"))
      (let-values ([(sys proc) (prepare-system)])
        ; generate assertion for requested retvals
        (let ([model (solve
          (for ([c calls] [ret retvals])
            (assert (equal? (interpret-call sys proc c) ret))))])
          (if (sat? model)
            ; retvals feasible: generate concrete trace
            (let ([ctrace (evaluate calls (complete-solution model (symbolics calls)))])
              (list (test '() sys calls retvals)))
            ; retvals infeasible: empty list
            '())
          )
        )))
    (begin
      (printf "calls not crash-free!\n")
      '()
      ))
  )

; return: list of tuples (system calls, return values)
; return traces with symbolic paths & a constraint
; for each possible distinct series of return values
(define (enumerate-distinct calls)
  (printf "enumerating distinct\n")
  (print-calls calls)
  (if (check-crashfree calls)
    (apply append (for*/list ([retvals (enumerate-retvals calls)])
      (printf "current retvals: ~v\n")
      (when (not (equal? (length calls) (length retvals)))
        (error "trace, retvals not same length!"))
      (let-values ([(sys proc) (prepare-system)])
        (clear-vc!)
        ; generate assertion for requested retvals
        (let (
            [a (for/fold ([conj #t]) ([c calls] [ret retvals])
              (and (equal? (interpret-call sys proc c) ret) conj))])
          (if (sat? (solve (assert a)))
            ; retvals feasible: output trace
            (list (test a sys calls retvals))
            ; retvals infeasible: empty list
            '())
          ))))
    (begin
      (printf "calls not crash-free!\n")
      '()
      ))
    )

; TODO circuit output
; TODO simplify output (how???)
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

(define (path-to-string s)
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
  )

(define (print-test f test)
  (when (boolean? (test-asn test))
    (fprintf f "#cond ~a ; " (bool-to-string (test-asn test))))
  (for (
      [c (test-calls test)]
      [retval (test-retvals test)])
    (match c
      [(call-open path) (fprintf f "open ~a = ~v ; " (path-to-string path) retval)]
      [(call-mkdir path) (fprintf f "mkdir ~a = ~v ; " (path-to-string path) retval)]
      [(call-chdir path) (fprintf f "chdir ~a = ~v ; " (path-to-string path) retval)]
      ))
  )

(define (print-tests fname tests)
  (let ([f (open-output-file fname #:exists 'replace)])
    (for-each (lambda (test) (print-test f test)) tests))
  )

;(let ([paths (enumerate-paths 2)])
;  (printf "path test: ~v\nlength: ~v\n" paths (length paths)))
;(let ([traces (enumerate-calls 2 2)])
;  (printf "call test: ~v\nlength: ~v\n" traces (length traces)))


