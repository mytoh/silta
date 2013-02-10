
(library (silta case-lambda)
    (export
      case-lambda)
  (import
    (silta base))



  (define-syntax case-lambda
    (syntax-rules ()
      ((_ (params body0 ...) ...)
       (lambda args
         (let ((len (length args)))
           (let-syntax
               ((cl (syntax-rules ::: ()
                                  ((_)
                                   (error "no matching clause"))
                                  ((_ ((p :::) . body) . rest)
                                   (if (= len (length '(p :::)))
                                       (apply (lambda (p :::)
                                                . body)
                                              args)
                                       (cl . rest)))
                                  ((_ ((p ::: . tail) . body)
                                      . rest)
                                   (if (>= len (length '(p :::)))
                                       (apply
                                        (lambda (p ::: . tail)
                                          . body)
                                        args)
                                       (cl . rest))))))
             (cl (params body0 ...) ...)))))))


  )
