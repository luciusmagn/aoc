(library-directories '("."))
(import (chezscheme)
        (utils))

(define (split-number n)
  (let* ((s (number->string n))
         (len (string-length s))
         (mid (quotient len 2))
         (left (string->number (substring s 0 mid)))
         (right (string->number (substring s mid len))))
    (list left right)))

(define (transform-stone stone)
  (cond
   ((= stone 0) (list 1))
   ((even? (string-length (number->string stone)))
    (split-number stone))
   (else (list (* stone 2024)))))

(define (transform-stones stones)
  (apply append (map transform-stone stones)))

(define (main input)
  (let* ((content (read-to-string input))
         (numbers (strlist->numberlist (split-whitespace content)))
         (final-stones (let loop ((stones numbers)
                                  (times 75))
                         (display times)
                         (newline)
                         (if (= times 0)
                             stones
                             (loop (transform-stones stones)
                                   (- times 1))))))
    (display (length final-stones))))

(main "plutonian-pebbles.input")
