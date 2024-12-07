(import (chezscheme)
        (utils))

(define (search-direction matrix str dx dy pos)
  (let ((row (car pos))
        (col (cdr pos)))
    (cond
     ((string=? str "") #t)                    ; found whole string
     ((not (matrix-ref matrix row col)) #f)    ; out of bounds
     ((not (char=? (matrix-ref matrix row col)
                   (string-ref str 0))) #f)    ; char doesn't match
     (else
      (search-direction
       matrix
       (substring str 1 (string-length str))
       dx
       dy
       (cons (+ row dy) (+ col dx)))))))

(define (is-x? element)
  (char=? (cdr element) #\X))

(define (main input)
  (let* ((lines      (read-lines input))
         (matrix     (map string->list lines))
         (enumerated (matrix-enumerate matrix))
         (xletters   (filter is-x? enumerated))
         (searcher   (lambda (dx dy pos) (search-direction matrix "XMAS" dx dy pos)))
         (xpositions (map car xletters))
         (top-left   (map (lambda (pos) (searcher -1 -1 pos)) xpositions))
         (top-top    (map (lambda (pos) (searcher -1  0 pos)) xpositions))
         (top-right  (map (lambda (pos) (searcher -1  1 pos)) xpositions))
         (mid-left   (map (lambda (pos) (searcher  0 -1 pos)) xpositions))
         (mid-right  (map (lambda (pos) (searcher  0  1 pos)) xpositions))
         (bot-left   (map (lambda (pos) (searcher  1 -1 pos)) xpositions))
         (bot-bot    (map (lambda (pos) (searcher  1  0 pos)) xpositions))
         (bot-right  (map (lambda (pos) (searcher  1  1 pos)) xpositions))
         (all        (flatten (list top-left
                                    top-top
                                    top-right
                                    mid-left
                                    mid-right
                                    bot-left
                                    bot-bot
                                    bot-right)))
         (total      (length (filter (lambda (x) x) all))))
    (display total)))

(main "ceres-search.input")
