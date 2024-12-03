(library-directories '("."))
(import (chezscheme)
        (utils))

(define (max-adj-diff lst)
  (let loop ((current lst)
             (max-diff 0))
    (if (null? (cdr current))
        max-diff
        (let* ((this-diff (- (car current) (cadr current)))
               (abs-diff  (abs this-diff)))
          (loop (cdr current)
                (max max-diff abs-diff))))))

(define (validate-report report-list)
  (let ((ascending  (unique (sort < report-list)))
        (descending (unique (sort > report-list))))
    (if (or
         (and (equal? ascending report-list) (>= 3 (max-adj-diff ascending)))
         (and (equal? descending report-list) (>= 3 (max-adj-diff descending))))
        1
        0)))

(define (main input)
  (let* ((lines         (read-lines input))
         (split-numbers (map split-whitespace lines))
         (number-lists  (map strlist->numberlist split-numbers))
         (results       (map validate-report number-lists))
         (total         (sum results)))
    (display total)))

(main "red-nosed-reports.input")
