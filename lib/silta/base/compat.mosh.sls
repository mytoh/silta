(library (silta base compat)
    (export
      ;; draft 9
      *
      +
      -
      ...
      /
      <
      <=
      =
      =>
      >
      >=
      abs
      append
      apply
      assoc
      assq
      assv
      begin
      binary-port?
      boolean=?
      boolean?
      bytevector
      bytevector-append
      bytevector-copy
      bytevector-copy!
      bytevector-length
      bytevector-u8-ref
      bytevector-u8-set!
      bytevector?
      caar
      cadr
      call-with-current-continuation
      call-with-port
      call-with-values
      call/cc
      car
      cdar
      cddr
      cdr
      ceiling
      char->integer
      char-ready?
      char<=?
      char<?
      char=?
      char>=?
      char>?
      char?
      close-input-port
      close-output-port
      close-port
      complex?
      cond-expand
      cons
      current-error-port
      current-input-port
      current-output-port
      define
      define-record-type
      define-syntax
      denominator
      dynamic-wind

      else
      eof-object?
      equal?
      error
      error-object-message
      even?
      exact-integer-sqrt
      exact?
      features
      floor
      floor-remainder
      flush-output-port
      gcd
      get-output-string
      if
      include-ci
      inexact?
      input-port?
      integer?
      lcm

      list
      list->vector
      list-ref
      list-tail
      make-bytevector
      make-vector
      max
      memq
      min
      negative?
      not
      number->string
      numerator
      open-input-bytevector
      open-output-bytevector
      output-port?
      peek-u8
      positive?
      quasiquote
      quotient
      raise-continuable
      rationalize
      read-bytevector!
      read-error?
      read-string
      real?
      reverse
      set!
      set-cdr!
      string
      string->number
      string->utf8
      string-append

      eof-object
      eq?
      eqv?
      error-object-irritants
      error-object?
      exact
      exact-integer?
      expt
      file-error?
      floor-quotient
      floor/
      for-each
      get-output-bytevector
      include
      inexact
      input-port-open?
      integer->char
      lambda
      length
      let-syntax
      letrec-syntax
      list->string
      list-copy
      list-set!
      list?
      make-list
      make-string
      map
      member
      memv
      modulo
      newline
      null?
      number?
      odd?
      open-input-string
      open-output-string
      output-port-open?
      pair?
      peek-char
      port?
      procedure?
      quote
      raise
      rational?
      read-bytevector
      read-char
      read-line
      read-u8
      remainder
      round
      set-car!
      square
      string->list
      string->symbol
      string->vector
      string-copy

      string-copy!
      string-for-each
      string-map
      string-set!
      string<?
      string>=?
      string?
      symbol->string
      symbol?
      syntax-rules
      truncate
      truncate-remainder
      u8-ready?
      unquote
      utf8->string
      vector
      vector->string
      vector-copy
      vector-fill!
      vector-length
      vector-ref
      vector?
      with-exception-handler
      write-char
      write-u8

      string-fill!
      string-length
      string-ref
      string<=?
      string=?
      string>?
      substring
      symbol=?
      syntax-error
      textual-port?
      truncate-quotient
      truncate/
      unquote-splicing
      values
      vector->list
      vector-append
      vector-copy!
      vector-for-each
      vector-map
      vector-set!
      write-bytevector
      write-string
      zero?

      ;;;;
      ;; define in base.sls

      ;; cond
      ;; let*
      ;; let
      ;; let*-values
      ;; let-values
      ;; letrec
      ;; letrec*
      ;; and
      ;; when
      ;; or
      ;; unless
      ;; case
      ;; define-values
      ;; do
      make-parameter
      parameterize
      ;; guard

      )
  (import
    (rnrs r5rs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (except (rnrs)
            case
            syntax-rules
            error
            define-record-type
            ;; SRFI-1
            map for-each member assoc

            vector-map
            )
    (srfi :0)
    (only (srfi :1)
          map for-each member assoc)
    (only (srfi :6)
          get-output-string
          open-output-string
          open-input-string)
    (srfi :23)
    (srfi :39)
    (srfi :9)

    (silta util bytevector-buffer)
    (silta util char-ready)
    (for (silta util syntax-rules) run expand)
    )





  ;;;;;;;;;;;;;;;;;;
  ;; from mosh r7b


  (define-syntax include
    (lambda (x)
      (syntax-case x ()
        ((_ ...) (assertion-violation 'include
                                      "Not allowed here..")))))

  (define-syntax include-ci
    (lambda (x)
      (syntax-case x ()
        ((_ ...) (assertion-violation 'include-ci
                                      "Not allowed here..")))))

  (define (features) ;; FIXME: ???
    '(r7rs ratios exact-complex full-unicode))


  (define-syntax syntax-error
    (lambda (x)
      (syntax-case x ()
        ((_ message args ...) (syntax-violation 'syntax-error
                                                #'message
                                                (quote  #'(args ...)))))))

  ;; R7RS error object will be mapped to R6RS condition object
  (define error-object? condition?)

  (define (error-object-irritants obj)
    (and (irritants-condition? obj)
      (condition-irritants obj)))

  (define (error-object-message obj)
    (and (message-condition? obj)
      (condition-message obj)))


  (define make-list
    (case-lambda
     ((k fil) (vector->list (make-vector k fil)))
     ((k) (make-list k 'unspecified))))


  ;; From division library
  (define (floor-quotient x y)
    (exact (floor (/ x y))))

  (define (floor-remainder0 x y)
    (- x (* (floor-quotient x y) y)))

  (define (floor/ x y)
    (let ((q (floor-quotient x y)))
      (values q
        (- x (* q y)))))

  (define floor-remainder modulo)

  (define write-string
    (case-lambda
     ((str) (write-string str (current-output-port)))
     ((str port) (put-string port str))
     ((str port start) (write-string str port start
                                     (- (string-length str) start)))
     ((str port start end)
      (write-string (substring str start end) port))))

  (define write-bytevector
    (case-lambda
     ((bv port)
      (put-bytevector port bv))
     ((bv) (write-bytevector bv (current-output-port)))))

  (define vector-copy!
    (case-lambda
     ((to at from)
      (vector-copy! to at from 0 (vector-length from)))
     ((to at from start)
      (vector-copy! to at from start (- (vector-length from) start)))
     ((to at from start end)
      (if (= start end)
        to
        (begin
          (vector-set! to at (vector-ref from start))
          (vector-copy! to (+ at 1) from (+ start 1) end))))))


  (define (bytevector . lis)
    (u8-list->bytevector lis))
  (define (bytevector-append . bvs)
    (u8-list->bytevector (apply append (map bytevector->u8-list bvs))))
  (define (vector-append . lis)
    (list->vector (apply append (map vector->list lis))))

  (define (open-input-bytevector bv) (open-bytevector-input-port bv))

  (define truncate-quotient quotient)
  (define truncate-remainder remainder)
  (define (truncate/ x y)
    (values (truncate-quotient x y)
      (truncate-remainder x y)))

  (define (vector-map proc . args)
    (list->vector (apply map proc (map vector->list args))))

  (define write-u8
    (case-lambda
     ((obj) (write-u8 obj (current-output-port)))
     ((obj port) (put-u8 port obj))))


  (define (string->vector str) (list->vector (string->list str)))
  (define (vector->string vec) (list->string (vector->list vec)))
  (define (vector-copy vec) (list->vector (vector->list vec)))


  (define (string-map proc . strs)
    (list->string (apply map proc (map string->list strs))))

  (define string-copy!
    (case-lambda
     ((to at from)
      (string-copy! to at from 0 (string-length from)))
     ((to at from start)
      (string-copy! to at from start (- (string-length from) start)))
     ((to at from start end)
      (let ((s (substring from start end)))
        (%string-charlist-paste to at (string->list s))))))

  (define (%string-charlist-paste to at l)
    (if (pair? l)
      (begin
        (string-set! to at (car l))
        (%string-charlist-paste to (+ at 1) (cdr l)))
      to))

  (define (square x) (* x x))

  (define read-u8
    (case-lambda
     (() (read-u8 (current-input-port)))
     ((port) (get-u8 port))))


  (define read-line
    (case-lambda
     (() (read-line (current-input-port)))
     ((port) (get-line port))))


  (define read-bytevector
    (case-lambda
     ((len) (read-bytevector len (current-input-port)))
     ((len port) (get-bytevector-n port len))))


  (define (list-set! l k obj)
    (define (itr cur count)
      (if (= count k)
        (set-car! cur obj)
        (itr (cdr cur) (+ count 1))))
    (itr l 0))

  (define (list-copy l) (map (lambda (e) e) l))

  (define file-error? i/o-error?)
  (define read-error? lexical-violation?)

  (define (exact-integer? i) (and (integer? i) (exact? i)))

  (define read-string
    (case-lambda
     ((len) (read-string len (current-input-port)))
     ((len port) (get-string-n port len))))

  (define read-bytevector!
    (case-lambda
     ((bv start end)
      (read-bytevector! bv start end (current-input-port)))
     ((bv start end port)
      (get-bytevector-n! port bv start (- end start)))))

  (define peek-u8
    (case-lambda
     (() (peek-u8 (current-input-port)))
     ((port)
      (lookahead-u8 port))))


  ;;; r7b-util.port-open
  (define (input-port-open? p) #t)
  (define (output-port-open? p) #t)



  ;;; r7b-util.u8-ready
  ;; FIXME:
  (define (u8-ready? p) #f)





























  )
