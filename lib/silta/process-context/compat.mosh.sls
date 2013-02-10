
(library (silta process-context compat)
    (export get-environment-variable get-environment-variables
            command-line exit emergency-exit)
  (import (rnrs)
          (srfi :98)
          (silta util emergency-exit))
  )
