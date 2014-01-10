
(library (silta util time)
    (export current-jiffy current-second jiffies-per-second)
  (import (rnrs)
          (srfi :19))

  (define scale 1000000000.0)

  (define (jiffies-per-second) (exact scale))
  (define (current-jiffy) (exact (return-sec time-monotonic)))
  (define (current-second) (return-sec time-tai))

  (define (return-sec sym)
    (let ((t (current-time sym)))
      (+ (* scale (time-second t))
         (time-nanosecond t))))

  )
