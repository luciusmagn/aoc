(library-directories '("."))
(import (chezscheme)
        (utils))

(define (make-counts keys values)
  (map cons keys (map (lambda (k)
                        (count k values))
                      keys)))

(define (multiply-alist pair)
  (* (car pair) (cdr pair)))

(define (main input-file)
  (let* ((lines        (read-lines input-file))
         (lines-words  (map split-whitespace lines))
         (words1       (carve 0 lines-words))
         (words2       (carve 1 lines-words))
         (numbers1     (map string->number words1))
         (numbers2     (map string->number words2))
         (sorted1      (sort < numbers1))
         (sorted2      (sort < numbers2))
         ;; get ready for turbo-slowness of O(fuckyou)
         ;; complexity
         (counts       (make-counts sorted1 sorted2))
         (values       (map multiply-alist counts))
         (total        (fold-left + 0 values)))
    (display total)))

(main "historian-histeria.input")
