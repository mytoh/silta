(library (silta lazy compat)
    (export
      promise?
      force
      delay
      make-promise
      delay-force
      )
  (import
    (rnrs)
    (rnrs mutable-pairs)
    )

  (begin
    ;;;;;;;;;;;;;;;;;
    ;; r7rs draft 9

    (define-syntax delay-force
      (syntax-rules ()
        ((_ expression)
         (make-promise #f (lambda () expression)))))

    (define-syntax delay
      (syntax-rules ()
        ((_ expression)
         (delay-force (make-promise #t expression)))))

    (define (make-promise done? proc)
      (list (cons done? proc)))

    (define (force promise)
      (if (promise-done? promise)
        (promise-value promise)
        (let ((promise* ((promise-value promise))))
          (unless (promise-done? promise)
            (promise-update! promise* promise))
          (force promise))))

    (define (promise-done? x)
      (car (car x)))

    (define (promise-value x)
      (cdr (car x)))

    (define (promise-update! new old)
      (set-car! (car old) (promise-done? new))
      (set-cdr! (car old) (promise-value new))
      (set-car! new (car old)))

    (define promise? list?)

    ))
