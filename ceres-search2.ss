(library-directories '("."))
(import (chezscheme)
        (utils))

(define (is-char? matrix char pos)
  (and (matrix-ref matrix (car pos) (cdr pos))
       (char=? (matrix-ref matrix (car pos) (cdr pos)) char)))

(define (check-diagonal matrix a-pos direction)
  (let* ((first-pos (cons (- (car a-pos) (car direction))
                          (- (cdr a-pos) (cdr direction))))
         (last-pos (cons (+ (car a-pos) (car direction))
                         (+ (cdr a-pos) (cdr direction)))))
    (or (and (is-char? matrix #\M first-pos)  ; MAS
             (is-char? matrix #\S last-pos))
        (and (is-char? matrix #\S first-pos)   ; SAM
             (is-char? matrix #\M last-pos)))))

(define (check-x-pattern matrix a-pos)
  (and (check-diagonal matrix a-pos '(1 . 1))
       (check-diagonal matrix a-pos '(1 . -1))))

(define (main input)
  (let* ((lines      (read-lines input))
         (matrix     (map string->list lines))
         (enumerated (matrix-enumerate matrix))
         (a-letters  (filter (lambda (e) (char=? (cdr e) #\A)) enumerated))
         (a-positions (map car a-letters))
         (x-patterns (filter (lambda (pos)
                               (check-x-pattern matrix pos))
                             a-positions)))
    (display (length x-patterns))))

(main "ceres-search.input")
