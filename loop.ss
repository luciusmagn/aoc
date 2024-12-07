(import (chezscheme))

(let* ((start (time-nanosecond (current-time)))
       (_ (do ((i 0 (+ i 1)))
              ((= i 1000000000))
            (void)))
       (end (time-nanosecond (current-time))))
  (display (/ (- end start) 1e9))
  (display " seconds\n"))
