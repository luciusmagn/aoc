(library-directories '("."))
(import (chezscheme)
        (utils))

(define dx '(0 1 0 -1))  ; up, right, down, left
(define dy '(-1 0 1 0))

(define (find-start matrix)
  (car (matrix-find (lambda (c) (char=? c #\^)) matrix)))

(define (is-valid? matrix pos)
  (matrix-ref matrix (car pos) (cdr pos)))

(define (next-pos pos dir)
  (cons (+ (car pos) (list-ref dy dir))
        (+ (cdr pos) (list-ref dx dir))))

(define (would-hit? matrix pos obstacle)
  (and (is-valid? matrix pos)
       (or (char=? #\# (matrix-ref matrix (car pos) (cdr pos)))
           (equal? pos obstacle))))

(define (simulate-loop matrix start obstacle)
  (let loop ((pos start) (dir 0) (seen (make-hashtable equal-hash equal?)))
    (let ((state (vector (car pos) (cdr pos) dir)))
      (cond
       ((not (is-valid? matrix pos)) #f)        ; Left grid
       ((hashtable-ref seen state #f) #t)       ; Found loop
       (else
        (hashtable-set! seen state #t)
        (let ((ahead (next-pos pos dir)))
          (if (would-hit? matrix ahead obstacle)
              (loop pos (modulo (+ dir 1) 4) seen)  ; Turn right
              (loop ahead dir seen))))))))          ; Move forward

(define (find-loop-positions matrix start)
  (let ((rows (length matrix))
        (cols (length (car matrix))))
    (let scan ((row 0) (col 0) (count 0))
      (cond
       ((>= row rows) count)
       ((>= col cols) (scan (+ row 1) 0 count))
       ((char=? #\. (matrix-ref matrix row col))
        (let ((new-count (if (simulate-loop matrix start (cons row col))
                             (+ count 1)
                             count)))
          (scan row (+ col 1) new-count)))
       (else
        (scan row (+ col 1) count))))))

(define (main input)
  (let* ((lines    (read-lines input))
         (matrix   (map string->list lines))
         (start    (find-start matrix))
         (loops    (find-loop-positions matrix start)))
    (printf "Positions that create loops: ~a\n" loops)))

(main "guard-gallivant.input")
