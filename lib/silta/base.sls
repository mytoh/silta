(library (silta base)
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
      and
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
      case
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
      cond
      cond-expand
      cons
      current-error-port
      current-input-port
      current-output-port
      define
      define-record-type
      define-syntax
      define-values
      denominator
      do
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
      let
      let*-values
      let-values
      letrec*
      list
      list->vector
      list-ref
      list-tail
      make-bytevector
      make-parameter
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
      or
      output-port?
      parameterize
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
      guard
      include
      inexact
      input-port-open?
      integer->char
      lambda
      length
      let*
      let-syntax
      letrec
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
      unless
      unquote-splicing
      values
      vector->list
      vector-append
      vector-copy!
      vector-for-each
      vector-map
      vector-set!
      when
      write-bytevector
      write-string
      zero?

      )
  (import
    (silta base compat))

  ;;;;;;;;;;;;;;;;;;
  ;; r7rs draft 9

  (define-syntax cond
    (syntax-rules (else =>)
      ((_ (else result1 result2 ...))
       (begin result1 result2 ...))
      ((_ (test => result))
       (let ((temp test))
         (if temp (result temp))))
      ((_ (test => result) clause1 clause2 ...)
       (let ((temp test))
         (if temp
           (result temp)
           (cond clause1 clause2 ...))))
      ((_ (test)) test)
      ((_ (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
           temp
           (cond clause1 clause2 ...))))
      ((_ (test result1 result2 ...))
       (if test (begin result1 result2 ...)))
      ((_ (test result1 result2 ...)
          clause1 clause2 ...)
       (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

  (define-syntax case
    (syntax-rules (else =>)
      ((_ (key ...)
          clauses ...)
       (let ((atom-key (key ...)))
         (case atom-key clauses ...)))
      ((_ key
          (else => result))
       (result key))
      ((_ key
          (else result1 result2 ...))
       (begin result1 result2 ...))
      ((_ key
          ((atoms ...) => result))
       (if (memv key '(atoms ...))
         (result key)))
      ((_ key
          ((atoms ...) => result)
          clause clauses ...)
       (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
      ((_ key
          ((atoms ...) result1 result2 ...))
       (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
      ((_ key
          ((atoms ...) result1 result2 ...)
          clause clauses ...)
       (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))


  (define-syntax and
    (syntax-rules ()
      ((_) #t)
      ((_ test) test)
      ((_ test1 test2 ...)
       (if test1 (and test2 ...) #f))))

  (define-syntax or
    (syntax-rules ()
      ((_) #f)
      ((_ test) test)
      ((_ test1 test2 ...)
       (let ((x test1))
         (if x x (or test2 ...))))))

  (define-syntax when
    (syntax-rules ()
      ((_ test result1 result2 ...)
       (if test
         (begin result1 result2 ...)))))

  (define-syntax unless
    (syntax-rules ()
      ((_ test result1 result2 ...)
       (if (not test)
         (begin result1 result2 ...)))))


  (define-syntax let
    (syntax-rules ()
      ((_ ((name val) ...) body1 body2 ...)
       ((lambda (name ...) body1 body2 ...)
        val ...))
      ((_ tag ((name val) ...) body1 body2 ...)
       ((letrec ((tag (lambda (name ...)
                        body1 body2 ...)))
          tag)
        val ...))))


  (define-syntax let*
    (syntax-rules ()
      ((_ () body1 body2 ...)
       (let () body1 body2 ...))
      ((_ ((name1 val1) (name2 val2) ...)
          body1 body2 ...)
       (let ((name1 val1))
         (let* ((name2 val2) ...)
           body1 body2 ...)))))


  (define-syntax letrec
    (syntax-rules ()
      ((_ ((var1 init1) ...) body ...)
       (letrec "generate temp names"
         (var1 ...)
         ()
         ((var1 init1) ...)
         body ...))
      ((_ "generate temp names"
          ()
          (temp1 ...)
          ((var1 init1) ...)
          body ...)
       (let ((var1 '<undefined>) ...)
         (let ((temp1 init1) ...)
           (set! var1 temp1)
           ...
           body ...)))
      ((_ "generate temp names"
          (x y ...)
          (temp ...)
          ((var1 init1) ...)
          body ...)
       (letrec "generate temp names"
         (y ...)
         (newtemp temp ...)
         ((var1 init1) ...)
         body ...))))


  (define-syntax letrec*
    (syntax-rules ()
      ((_ ((var1 init1) ...) body1 body2 ...)
       (let ((var1 '<undefined>) ...)
         (set! var1 init1)
         ...
         (let () body1 body2 ...)))))

  (define-syntax let-values
    (syntax-rules ()
      ((_ (binding ...) body0 body1 ...)
       (let-values "bind"
         (binding ...) () (begin body0 body1 ...)))
      ((_ "bind" () tmps body)
       (let tmps body))
      ((_ "bind" ((b0 e0)
                  binding ...)
          tmps body)
       (let-values "mktmp" b0 e0 ()
                   (binding ...) tmps body))
      ((_ "mktmp" () e0 args
          bindings tmps body)
       (call-with-values
           (lambda () e0)
         (lambda args
           (let-values "bind"
             bindings tmps body))))
      ((_ "mktmp" (a . b) e0 (arg ...)
          bindings (tmp ...) body)
       (let-values "mktmp" b e0 (arg ... x)
                   bindings (tmp ... (a x)) body))
      ((_ "mktmp" a e0 (arg ...)
          bindings (tmp ...) body)
       (call-with-values
           (lambda () e0)
         (lambda (arg ... . x)
           (let-values "bind"
             bindings (tmp ... (a x)) body))))))


  (define-syntax let*-values
    (syntax-rules ()
      ((_ () body0 body1 ...)
       (let () body0 body1 ...))
      ((_ (binding0 binding1 ...)
          body0 body1 ...)
       (let-values (binding0)
         (let*-values (binding1 ...)
           body0 body1 ...)))))


  (define-syntax define-values
    (syntax-rules ()
      ((_ () expr)
       (define dummy
         (call-with-values (lambda () expr)
           (lambda args #f))))
      ((_ (var) expr)
       (define var expr))
      ((_ (var0 var1 ... varn) expr)
       (begin
         (define var0
           (call-with-values (lambda () expr)
             list))
         (define var1
           (let ((v (cadr var0)))
             (set-cdr! var0 (cddr var0))
             v))
         ...
         (define varn
           (let ((v (cadr var0)))
             (set! var0 (car var0))
             v))))
      ((_ (var0 var1 ... . varn) expr)
       (begin
         (define var0
           (call-with-values (lambda () expr)
             list))
         (define var1
           (let ((v (cadr var0)))
             (set-cdr! var0 (cddr var0))
             v))
         ...
         (define varn
           (let ((v (cdr var0)))
             (set! var0 (car var0))
             v))))
      ((_ var expr)
       (define var
         (call-with-values (lambda () expr)
           list)))))


  (define-syntax do
    (syntax-rules ()
      ((_ ((var init step ...) ...)
          (test expr ...)
          command ...)
       (letrec
           ((loop
             (lambda (var ...)
               (if test
                 (begin
                   (if #f #f)
                   expr ...)
                 (begin
                   command
                   ...
                   (loop (do "step" var step ...)
                         ...))))))
         (loop init ...)))
      ((_ "step" x)
       x)
      ((_ "step" x y)
       y)))



  (define-syntax guard
    (syntax-rules ()
      ((_ (var clause ...) e1 e2 ...)
       ((call/cc
            (lambda (guard-k)
              (with-exception-handler
                  (lambda (condition)
                    ((call/cc
                         (lambda (handler-k)
                           (guard-k
                            (lambda ()
                              (let ((var condition))
                                (guard-aux
                                 (handler-k
                                  (lambda ()
                                    (raise-continuable condition)))
                                 clause ...))))))))
                (lambda ()
                  (call-with-values
                      (lambda () e1 e2 ...)
                    (lambda args
                      (guard-k
                       (lambda ()
                         (apply values args)))))))))))))


  (define-syntax guard-aux
    (syntax-rules (else =>)
      ((_ reraise (else result1 result2 ...))
       (begin result1 result2 ...))
      ((_ reraise (test => result))
       (let ((temp test))
         (if temp
           (result temp)
           reraise)))
      ((_ reraise (test => result)
          clause1 clause2 ...)
       (let ((temp test))
         (if temp
           (result temp)
           (guard-aux reraise clause1 clause2 ...))))
      ((_ reraise (test))
       (or test reraise))
      ((_ reraise (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
           temp
           (guard-aux reraise clause1 clause2 ...))))
      ((_ reraise (test result1 result2 ...))
       (if test
         (begin result1 result2 ...)
         reraise))
      ((_ reraise
          (test result1 result2 ...)
          clause1 clause2 ...)
       (if test
         (begin result1 result2 ...)
         (guard-aux reraise clause1 clause2 ...)))))




  )
