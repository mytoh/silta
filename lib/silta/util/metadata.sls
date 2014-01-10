
(library (silta util metadata)
    (export metadata-set!
            metadata-ref)
  (import (rnrs)
          (silta util weak-eq-hashtable))
  (define store (make-weak-eq-hashtable))
  (define (metadata-set! obj datum)
    (weak-hashtable-set! store obj datum))
  (define (metadata-ref obj)
    (weak-hashtable-ref store obj))
  )
