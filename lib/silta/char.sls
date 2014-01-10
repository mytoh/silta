
(library (silta char)
    (export
      ;; draft 9
      char-alphabetic?
      char-ci<?
      char-ci>=?
      char-downcase
      char-lower-case?
      char-upcase
      char-whitespace?
      string-ci<=?
      string-ci=?
      string-ci>?
      string-foldcase

      char-ci<=?
      char-ci=?
      char-ci>?
      char-foldcase
      char-numeric?
      char-upper-case?
      digit-value
      string-ci<?
      string-ci>=?
      string-downcase
      string-upcase
      )
  (import
    (rnrs))

  (define (digit-value char)
    (and (char-numeric? char)
         (case char
           ((#\0) 0)
           ((#\1) 1)
           ((#\2) 2)
           ((#\3) 3)
           ((#\4) 4)
           ((#\5) 5)
           ((#\6) 6)
           ((#\7) 7)
           ((#\8) 8)
           ((#\9) 9)
           (else #f))))
  )
