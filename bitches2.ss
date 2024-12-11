(library-directories (quote (".")))
(import (chezscheme) (utils))

(define (main input)
  (let ((lines (read-lines input)))
    (display lines)))
