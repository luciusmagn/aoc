(library-directories '("."))
(import (chezscheme)
        (utils))

(define dx '(0 1 0 -1))
(define dy '(-1 0 1 0))

(define (find-start matrix)
  (car (matrix-find (lambda (c) (char=? c #\^)) matrix)))

(define (is-valid? matrix pos)
  (let ((row (car pos))
        (col (cdr pos)))
    ;; already returns #f on out of bounds!
    (matrix-ref matrix row col)))

(define (next-pos pos dir)
  (cons (+ (car pos) (list-ref dy dir))
        (+ (cdr pos) (list-ref dx dir))))

(define (main input)
  (let* ((lines    (read-lines input))
         (matrix   (map string->list lines))
         (start    (find-start matrix)))
    (let loop ((pos     start)
               (dir     0)
               (visited (list start)))
      (let ((next (next-pos pos dir)))
        (cond
         ((not (is-valid? matrix next))
          (printf "Positions visited: ~a\n" (length (unique visited))))
         ((char=? #\# (matrix-ref matrix (car next) (cdr next)))
          (loop pos (modulo (+ dir 1) 4) visited))
         (else
          (loop next dir (cons next visited))))))))

(main "guard-gallivant.input")
