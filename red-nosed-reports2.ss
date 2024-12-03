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

(define (valid? varant)
  (let ((ascending  (unique (sort < variant)))
        (descending (unique (sort > variant))))
    (or (and
         (equal? ascending variant)
         (>= 3 (max-adj-diff ascending)))
        (and
         (equal? descending variant)
         (>= 3 (max-adj-diff descending))))
    ))

(define (validate-report report-list)
  (let ((all-variants (remove-each report-list)))
    (ormap valid? all-variants)))

(define (main input)
  (let* ((lines         (read-lines input))
         (split-numbers (map split-whitespace lines))
         (number-lists  (map strlist->numberlist split-numbers))
         (results       (map validate-report number-lists))
         (results-nums  (map bool->number results))
         (total         (sum results-nums)))
    (display total)))

(main "red-nosed-reports.input")
