(library-directories '("."))
(import (chezscheme)
        (utils))

(define (reachable-nines matrix-enumerated zero-pos)
  (define (height-at pos)
    (let ((entry (assoc pos matrix-enumerated)))
      (if entry
          (- (char->integer (cdr entry)) (char->integer #\0))
          #f)))

  (define (neighbors pos)
    (let ((row (car pos))
          (col (cdr pos)))
      (list
       (cons (- row 1) col)
       (cons (+ row 1) col)
       (cons row (- col 1))
       (cons row (+ col 1)))))

  (define (find-nines pos visited current-height)
    (cond
     ((member pos visited) '())
     ((not (height-at pos)) '())
     ((not (= (height-at pos) current-height)) '())
     ((= current-height 9) (list pos))
     (else
      (unique
       (apply append
              (map (lambda (n) (find-nines n (cons pos visited) (+ current-height 1)))
                   (neighbors pos)))))))

  (length (find-nines zero-pos '() 0)))

(define (main input)
  (let* ((lines             (read-lines input))
         (matrix            (map string->list lines))
         (matrix-enumerated (matrix-enumerate matrix))
         (zero-filter       (lambda (p) (char=? #\0 (cdr p))))
         (zeroes            (filter zero-filter matrix-enumerated))
         (zero-positions    (map car zeroes))
         (reachable-lambda  (lambda (n) (reachable-nines matrix-enumerated n)))
         (reachable-nines   (map reachable-lambda zero-positions))
         (total             (sum reachable-nines)))
    (display total)))

(main "hoof-it.input")
