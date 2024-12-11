(library (utils)
  (export read-lines
          read-to-string
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
          remove-each
          matrix-ref
          matrix-enumerate
          matrix-find
          list-middle
          list-index
          string-split-first-string
          any
          fn
          trace-block
          flat-map
          matrix-in-bounds?
          point
          point-x
          point-y
          point-map
          point-
          point+
          point-scale
          grid-points
          group-by
          trace
          take
          drop
          fill-vector!
          vector-find
          vector-find-right
          string-split)
  (import (chezscheme))

  ;; =============== I/O Operations ===============

  ;; read-lines : String -> (list-of String)
  ;; ---
  ;; Reads a file line by line and returns its contents as a list of strings
  (define (read-lines filename)
    (call-with-input-file filename
      (lambda (port)
        (let loop ((lines '())
                   (current (get-line port)))
          (if (eof-object? current)
              (reverse lines)
              (loop (cons current lines)
                    (get-line port)))))))

  ;; read-file : String -> String
  ;; ---
  ;; Read a whole file into a single string
  (define (read-to-string filename)
    (fold-left (lambda (acc line)
                 (string-append acc line "\n"))
               ""
               (read-lines filename)))

  ;; =============== List Operations ===============

  ;; carve : Integer (list-of (list-of Any)) -> (list-of Any)
  ;; ---
  ;; Extracts nth element from each sublist in a list of lists
  (define (carve n lists)
    (map (lambda (lst) (list-ref lst n)) lists))

  ;; zip : (list-of A) (list-of B) -> (list-of (list-of A B))
  ;; ---
  ;; Combines two lists into a list of pairs
  (define (zip list1 list2)
    (map list list1 list2))

  ;; map-apply : (A ... -> B) (list-of (list-of A)) -> (list-of B)
  ;; ---
  ;; Applies function f to each sublist in lst
  (define (map-apply f lst)
    (map (lambda (l) (apply f l)) lst))

  ;; count: Any (list-of Any) -> Integer
  ;; ---
  ;; Counts occurrences of x in lst
  (define (count x lst)
    (length (filter (lambda (y) (= x y)) lst)))

  ;; unique: (list-of Any) -> (list-of Any)
  ;; ---
  ;; Returns list with duplicates removed, preserving order
  (define (unique lst)
    (fold-left (lambda (acc x)
                 (if (member x acc)
                     acc
                     (cons x acc)))
               '()
               (reverse lst)))

  ;; flatten: (list-of (list-of Any)) -> (list-of Any)
  ;; ---
  ;; Flattens one level of list nesting
  (define (flatten lst)
    (apply append lst))

  ;; remove-each: (list-of Any) -> (list-of (list-of Any))
  ;; ---
  ;; Returns list of lists where each sublist is original list with one element removed
  (define (remove-each lst)
    (let loop ((before '())
               (after lst)
               (acc '()))
      (if (null? after)
          acc
          (loop (append before (list (car after)))
                (cdr after)
                (cons (append before (cdr after))
                      acc)))))

  ;; list-middle: (list-of Any) -> Any
  ;; ---
  ;; Returns middle element. For even length lists, returns element at len/2
  (define (list-middle lst)
    (let* ((len (length lst))
           (mid (quotient len 2)))
      (if (odd? len)
          (list-ref lst mid)
          (list-ref lst mid))))

  ;; list-index: (Any -> Bool) (list-of Any) -> (or Integer #f)
  ;; ---
  ;; Returns index of first element matching predicate, or #f if none found
  (define (list-index pred lst)
    (let loop ((i 0) (l lst))
      (cond ((null? l) #f)
            ((pred (car l)) i)
            (else (loop (+ i 1) (cdr l))))))

  ;; any: (Any -> Bool) (list-of Any) -> Bool
  ;; ---
  ;; Returns #t if predicate is true for any element in list
  (define (any pred lst)
    (cond
     ((null? lst) #f)
     ((pred (car lst)) #t)
     (else (any pred (cdr lst)))))

  ;; =============== String Operations ===============

  ;; split-whitespace : String -> (list-of String)
  ;; ---
  ;; Splits string on whitespace into list of strings
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

  ;; string-slice: String Integer Integer -> String
  ;; ---
  ;; Returns substring from start to end index. Negative end counts from string end
  (define (string-slice str start end)
    (let* ((len (string-length str))
           (real-end (if (negative? end)
                         (+ len end)
                         end)))
      (substring str start real-end)))

  ;; string-split-first: String char -> (String String)
  ;; ---
  ;; Splits string on first occurrence of char into pair of strings
  (define (string-split-first str char)
    (let loop ((i 0))
      (cond
       ((= i (string-length str)) (cons str ""))
       ((char=? (string-ref str i) char)
        (cons (substring str 0 i)
              (substring str (+ i 1) (string-length str))))
       (else (loop (+ i 1))))))

  ;; string-every: pred? String -> Bool
  ;; ---
  ;; Tests if predicate is true for every character in string
  (define (string-every pred str)
    (let loop ((i 0))
      (cond
       ((= i (string-length str)) #t)
       ((not (pred (string-ref str i))) #f)
       (else (loop (+ i 1))))))

  ;; string-snip: String String String -> String
  ;; ---
  ;; Removes all substrings that start with start-str and end with end-str
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

  ;; find-between: String String String -> (list-of String)
  ;; ---
  ;; Finds all substrings that start with start-str and end with end-str
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

  ;; string-split-first-string: String String -> (cons String String)
  ;; ---
  ;; Splits string on first occurrence of split_str into pair of strings
  (define (string-split-first-string str split_str)
    (let* ((len (string-length str))
           (split-len (string-length split_str))
           (pos (let loop ((i 0))
                  (if (>= i (- len split-len))
                      str ; If split_str not found, return -1
                      (if (string=? (substring str i (+ i split-len)) split_str)
                          i
                          (loop (+ i 1))))))
           (first-part (if (>= pos 0)
                           (substring str 0 pos)
                           str))
           (second-part (if (>= pos 0)
                            (substring str (+ pos split-len) len)
                            "")))
      (cons first-part second-part)))

  ;; string-split: (Char -> Bool) String -> (list-of String)
  ;; ---
  ;; Splits string on characters matching delimiter predicate
  (define (string-split char-delimiter? string)
    (define (maybe-add a b parts)
      (if (= a b) parts (cons (substring string a b) parts)))
    (let ((n (string-length string)))
      (let loop ((a 0) (b 0) (parts '()))
        (if (< b n)
            (if (not (char-delimiter? (string-ref string b)))
                (loop a (+ b 1) parts)
                (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
            (reverse (maybe-add a b parts))))))

  ;; =============== Number Operations ===============

  ;; strlist->numberlist: (list-of String) -> (list-of Number)
  ;; ---
  ;; Converts list of strings to list of numbers
  (define (strlist->numberlist lst)
    (map string->number lst))

  ;; bool->number: Bool -> Integer
  ;; ---
  ;; Converts boolean to 1 (true) or 0 (false)
  (define (bool->number b)
    (if b 1 0))

  ;; sum: (list-of Number) -> Number
  ;; ---
  ;; Adds up all numbers in a list
  (define (sum nums)
    (fold-left + 0 nums))

  ;; =============== Matrix Operations ===============

  ;; matrix-ref: (list-of (list-of Any)) Integer Integer -> Any
  ;; ---
  ;; Safely access matrix element at row/col. Returns #f if out of bounds
  (define (matrix-ref matrix row col)
    (and (>= row 0)
         (>= col 0)
         (< row (length matrix))
         (< col (length (car matrix)))
         (list-ref (list-ref matrix row) col)))

  ;; matrix-enumerate: (list-of (list-of Any)) -> (list-of (cons (cons Integer Integer) Any))
  ;; ---
  ;; Returns list of ((row . col) . value) for all matrix elements
  (define (matrix-enumerate matrix)
    (let loop-row ((row 0) (rows matrix))
      (if (null? rows)
          '()
          (let loop-col ((col 0) (cols (car rows)))
            (if (null? cols)
                (loop-row (+ row 1) (cdr rows))
                (cons
                 (cons (cons row col) (car cols))
                 (loop-col (+ col 1) (cdr cols))))))))

  ;; matrix-find: (Any -> Bool) (list-of (list-of Any)) -> (cons (cons Integer Integer) Any)
  ;; ---
  ;; Finds first element matching predicate, returns ((row . col) . value) or #f
  (define (matrix-find pred matrix)
    (let ((enumerated (matrix-enumerate matrix)))
      (let loop ((items enumerated))
        (cond
         ((null? items) #f)
         ((pred (cdar items)) (car items))
         (else (loop (cdr items)))))))

  ;; =============== Macro Definitions ===============

  ;; fn: Macro for creating curryable functions
  ;; ---
  ;; Creates function that can be partially applied
  (define-syntax fn
    (syntax-rules ()
      [(_ (name args ...) body)
       (define name
         (letrec ([f (case-lambda
                       [(args ...) body]
                       [partial-args
                        (lambda more-args
                          (apply f (append partial-args more-args)))])])
           f))]))

  ;; trace-block: Macro for tracing expressions
  ;; ---
  ;; Displays and evaluates each expression, showing input and output
  (define-syntax trace-block
    (syntax-rules ()
      [(_ expr ...)
       (begin
         (let ([result (begin
                         (display ">> ")
                         (write 'expr)
                         (newline)
                         expr)])
           (display "<< ")
           (write result)
           (newline))
         ...)]))

  ;; =============== Macro Definitions ===============
  (define (flat-map f list)
    (flatten (map f list)))

  ;; Check if point is within matrix bounds
  (fn (matrix-in-bounds? rows cols point)
      (let ((x (car point))
            (y (cdr point)))
        (and (>= x 0) (>= y 0) (< x cols) (< y rows))))

  ;; Group values by key using provided key-fn
  (define (group-by key-fn lst)
    (fold-left (lambda (acc item)
                 (let* ((key (key-fn item))
                        (entry (assoc key acc)))
                   (if entry
                       (cons (cons key (cons item (cdr entry)))
                             (remove (lambda (p) (eq? (car p) key)) acc))
                       (cons (cons key (list item)) acc))))
               '()
               lst))

  ;; =============== Unsorted todo ===============
  ;; Pair operations on points
  (define (point x y) (cons x y))
  (define (point-x p) (car p))
  (define (point-y p) (cdr p))

  ;; Map operation on each coordinate of a point
  (define (point-map f p1 p2)
    (point (f (point-x p1) (point-x p2))
           (f (point-y p1) (point-y p2))))

  ;; Generate all points in a grid
  (define (grid-points rows cols)
    (flat-map (lambda (y)
                (map (lambda (x) (point x y))
                     (iota cols)))
              (iota rows)))

  (define (point- p1 p2)
    (point (- (point-x p1) (point-x p2))
           (- (point-y p1) (point-y p2))))

  (define (point+ p1 p2)
    (point (+ (point-x p1) (point-x p2))
           (+ (point-y p1) (point-y p2))))

  (define (point-scale p factor)
    (point (* (point-x p) factor)
           (* (point-y p) factor)))

  ;; take: Integer (list-of Any) -> (list-of Any)
  ;; ---
  ;; Returns first n elements of list
  (define (take lst n)
    (if (or (= n 0) (null? lst))
        '()
        (cons (car lst) (take (- n 1) (cdr lst)))))

  ;; drop: Integer (list-of Any) -> (list-of Any)
  ;; ---
  ;; Returns list without first n elements
  (define (drop lst n)
    (if (or (= n 0) (null? lst))
        lst
        (drop (- n 1) (cdr lst))))

  (define (fill-vector! vec value start count)
    (let loop ((i 0))
      (when (< i count)
        (vector-set! vec (+ start i) value)
        (loop (+ i 1)))))

  (define (vector-find pred vec start)
    (let ((len (vector-length vec)))
      (let loop ((i start))
        (cond
         ((>= i len) #f)
         ((pred (vector-ref vec i)) i)
         (else (loop (+ i 1)))))))

  (define (vector-find-right pred vec end)
    (let loop ((i (- end 1)))
      (cond
       ((< i 0) #f)
       ((pred (vector-ref vec i)) i)
       (else (loop (- i 1)))))))
