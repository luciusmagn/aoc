(library (utils)
  (export read-lines
          split-whitespace
          carve
          zip
          map-apply
          count
          unique
          strlist->numberlist
          bool->number
          sum
          find-between
          flatten
          string-slice
          string-split-first
          string-every
          string-snip
          remove-each)
  (import (chezscheme))

  ;; read-lines : String -> (list-of String)
  (define (read-lines filename)
    (call-with-input-file filename
      (lambda (port)
        (let loop ((lines '())
                   (current (get-line port)))
          (if (eof-object? current)
              (reverse lines)
              (loop (cons current lines)
                    (get-line port)))))))

  ;; split-whitespace : String -> (list-of String)
  (define (split-whitespace str)
    (let loop ((chars (string->list str))
               (words '())
               (current '()))
      (cond
       ((null? chars)
        (if (null? current)
            (reverse words)
            (reverse (cons (list->string (reverse current)) words))))
       ((char-whitespace? (car chars))
        (if (null? current)
            (loop (cdr chars) words '())
            (loop (cdr chars)
                  (cons (list->string (reverse current)) words)
                  '())))
       (else
        (loop (cdr chars) words (cons (car chars) current))))))

  ;; carve : Integer (list-of (list-of Any)) -> (list-of Any)
  (define (carve n lists)
    (map (lambda (lst) (list-ref lst n)) lists))

  ;; zip : (list-of A) (list-of B) -> (list-of (list-of A B))
  (define (zip list1 list2)
    (map list list1 list2))

  ;; map-apply : (A ... -> B) (list-of (list-of A)) -> (list-of B)
  (define (map-apply f lst)
    (map (lambda (l) (apply f l)) lst))

  ;; count: Any (list-of Any) -> Integer
  (define (count x lst)
    (length (filter (lambda (y) (= x y)) lst)))

  ;; strlist->numberlist: (list-of String) -> (list-of Number)
  (define (strlist->numberlist lst)
    (map string->number lst))

  ;; bool->number: Bool -> Integer
  (define (bool->number b)
    (if b 1 0))

  ;; unique: (list-of Any) -> (list-of Any)
  (define (unique lst)
    (fold-left (lambda (acc x)
                 (if (member x acc)
                     acc
                     (cons x acc)))
               '()
               (reverse lst)))

  ;; sum: (list-of Number) -> Number
  (define (sum nums)
    (fold-left + 0 nums))

  ;; find-between: String String String -> (list-of String)
  (define (find-between start-str end-str text)
    (let* ((len (string-length text))
           (start-len (string-length start-str))
           (end-len (string-length end-str)))
      (let loop ((i 0) (results '()))
        (if (> (+ i start-len) len)
            (reverse results)
            (let ((start-pos (and (<= (+ i start-len) len)
                                  (string=? (substring text i (+ i start-len)) start-str)
                                  i)))
              (if start-pos
                  (let inner ((j (+ start-pos start-len)))
                    (cond
                     ((> (+ j end-len) len) (loop (+ i 1) results))
                     ((string=? (substring text j (+ j end-len)) end-str)
                      (loop (+ i 1)
                            (cons (substring text start-pos (+ j end-len))
                                  results)))
                     (else (inner (+ j 1)))))
                  (loop (+ i 1) results)))))))

  ;; flatten: (list-of (list-of Any)) -> (list-of Any)
  (define (flatten lst)
    (apply append lst))

  ;; string-slice: String Integer Integer -> String
  (define (string-slice str start end)
    (let* ((len (string-length str))
           (real-end (if (negative? end)
                         (+ len end)
                         end)))
      (substring str start real-end)))

  ;; string-split-first: String char -> (String String)
  (define (string-split-first str char)
    (let loop ((i 0))
      (cond
       ((= i (string-length str)) (cons str ""))
       ((char=? (string-ref str i) char)
        (cons (substring str 0 i)
              (substring str (+ i 1) (string-length str))))
       (else (loop (+ i 1))))))

  ;; string-every: pred? String -> Bool
  (define (string-every pred str)
    (let loop ((i 0))
      (cond
       ((= i (string-length str)) #t)
       ((not (pred (string-ref str i))) #f)
       (else (loop (+ i 1))))))

  ;; string-snip: String String String -> String
  (define (string-snip start-str end-str str)
    (let loop ((result "")
               (rest str))
      (let* ((start-idx (let find ((i 0))
                          (cond
                           ((> (+ i (string-length start-str)) (string-length rest)) #f)
                           ((string=? (substring rest i (+ i (string-length start-str))) start-str) i)
                           (else (find (+ i 1))))))
             (end-idx (and start-idx
                           (let find ((i (+ start-idx (string-length start-str))))
                             (cond
                              ((> (+ i (string-length end-str)) (string-length rest)) #f)
                              ((string=? (substring rest i (+ i (string-length end-str))) end-str)
                               (+ i (string-length end-str)))
                              (else (find (+ i 1))))))))
        (cond
         ((not start-idx) (string-append result rest))
         ((not end-idx) (string-append result rest))
         (else (loop (string-append result (substring rest 0 start-idx))
                     (substring rest end-idx (string-length rest))))))))

  ;; remove-each: (list-of Any) -> (list-of (list-of Any))
  (define (remove-each lst)
    (let loop ((before '())
               (after lst)
               (acc '()))
      (if (null? after)
          acc
          (loop (append before (list (car after)))
                (cdr after)
                (cons (append before (cdr after))
                      acc))))))
