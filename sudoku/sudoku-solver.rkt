#lang racket

(require compatibility/defmacro)

(define-macro (my-assert-equals body1 body2)
  `(unless (equal? ,body1 ,body2)
     (error (format "Failed assertion '(assert-equals ~a ~a)'" ',body1 ',body2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Basic Matrix Functions

(define (mat-row mat r) ; return the matrix row
  (my-nth mat r))

(define (mat-col mat c) ; return the matrix column
  (my-map (lambda (row) (my-nth row c)) mat))

(define (mat-get mat r c) ; return mat[r][c]
  (my-nth (mat-row mat r) c))

(define (my-nth lst n)
  (cond
    [(null? lst) null]
    [(equal? n 0) (car lst)]
    [#t (my-nth (cdr lst) (- n 1))]))

(define (my-map foo lst)
  (my-foldr (lambda (acc x) (cons (foo x) acc)) null lst))

(define (my-foldr foo init lst)
  (if (null? lst)
      init
      (foo (my-foldr foo init (cdr lst)) (car lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Check if the sudoku matrix with the given number is valid

(define (isValid? m-size box-size num r c mat)
  (and (and (validate-col m-size 0 num r c mat)
            (validate-row m-size 0 num r c mat))
            (validate-box box-size num r c mat)))

; Checking the validity of the number by column

(define (validate-col m-size i num r c mat)
  (cond
    [(equal? i m-size) #t]
    [(and (equal? num (mat-get mat i c)) (not (equal? r i))) #f]
    [#t (validate-col m-size (+ i 1) num r c mat)]))

; Checking the validity of the number by row

(define (validate-row m-size i num r c mat)
  (cond
    [(equal? i m-size) #t]
    [(and (equal? num (mat-get mat r i)) (not (equal? c i))) #f]
    [#t (validate-row m-size (+ i 1) num r c mat)]))

; Checking the validity of the number by box

(define (box-row m-box-size r) ; return the row number of the matrix in which is the beginning of the box
  (* (floor (/ r m-box-size)) m-box-size))

(define (box-col m-box-size c) ; return the column number of the matrix in which is the beginning of the box
  (* (floor (/ c m-box-size)) m-box-size))

(define (validate-box box-size num r c mat)
  (let ([i (box-row box-size r)]
        [j (box-col box-size c)])
    (let ([boxRow (+ i box-size)]
          [boxCol (+ j box-size)])
      (my-validate-box i j boxRow boxCol num r c mat))))
(define (my-validate-box i j boxRow boxCol num r c mat)
  (cond
    [(equal? i boxRow) #t]
    [(equal? j boxCol) (my-validate-box (+ i 1) j boxRow boxCol num r c mat)]
    [(and (and (not (equal? i r)) (not (equal? j c))) (equal? num (mat-get mat i j))) #f]
    [#t (my-validate-box i (+ j 1) boxRow boxCol num r c mat)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Setters

(define (list-set m-size lst i num)
  (my-list-set 0 m-size lst i num))
(define (my-list-set k m-size lst i num)
  (cond
    [(equal? k (- m-size 1))
     (if (equal? k i)
         (cons num null)
         (cons (car lst) null))]
    [(equal? k i) (cons num (my-list-set (+ k 1) m-size (cdr lst) i num))]
    [#t (cons (car lst) (my-list-set (+ k 1) m-size (cdr lst) i num))]))

(define (mat-set m-size mat i j num)
  (my-mat-set 0 m-size mat i j num))
(define (my-mat-set k m-size mat i j num)
  (cond
    [(equal? k (- m-size 1))
     (if (equal? k i)
         (cons (list-set m-size (car mat) j num) null)
         (cons (car mat) null))]
    [(equal? k i) (cons (list-set m-size (car mat) j num)
                        (my-mat-set (+ k 1) m-size (cdr mat) i j num))]
    [#t (cons (car mat) (my-mat-set (+ k 1) m-size (cdr mat) i j num))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Function empty-pos finds empty positions in a sudoku matrix and returns a list of them

(define (empty-pos m-size mat)
  (my-empty-pos 0 0 m-size mat null))
(define (my-empty-pos i j s mat lst)
  (cond
    [(equal? i s) lst]
    [(equal? j s) (my-empty-pos (+ i 1) 0 s mat lst)]
    [(equal? 0 (mat-get mat i j)) (my-empty-pos i (+ j 1) s mat (my-append lst (cons i j)))]
    [#t (my-empty-pos i (+ j 1) s mat lst)]))


(define (my-append lst x)
  (if (null? lst)
      (cons x null)
      (cons (car lst) (my-append (cdr lst) x))))


(define (my-length lst)
  (cond
    [(null? lst) 0]
    [(atom? (car lst)) (+ 1 (my-length (cdr lst)))]
    [#t (+ (my-length (car lst)) (my-length (cdr lst)))]))

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Sudoku solving function

(define (solve mat)
  (let ([m-size (my-length (car mat))] [box-size (sqrt  (length (car mat)))])
  (let ([res (solve-sudoku m-size box-size (empty-pos m-size mat) mat 0 1)])
    (if (equal? res null)
        "solution does not exist"
        res))))

(define (solve-sudoku m-size box-size e-pos mtx i acc)
  (cond
    [(= i -1) null]
    [(null? (my-nth e-pos i)) mtx]
    [#t
     (let ([pos (my-nth e-pos i)])
          (let ( [x (car pos)] [y (cdr pos)])
            (let ([newval (+ (mat-get mtx x y) acc)])
              (cond
                [(> newval m-size)
                 (solve-sudoku m-size box-size e-pos (mat-set m-size mtx x y 0) (- i 1) 1)]
                [(isValid? m-size box-size newval x y mtx)
                 (solve-sudoku m-size box-size e-pos (mat-set m-size mtx x y newval) (+ i 1) 1)]
                [#t (solve-sudoku m-size box-size e-pos mtx i (+ acc 1))]))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define mat-1
  '((3 0 1 0)
    (0 2 0 0)
    (0 0 2 0)
    (0 1 4 0)))

(my-assert-equals (solve mat-1) '((3 4 1 2)
                                  (1 2 3 4)
                                  (4 3 2 1)
                                  (2 1 4 3)) )

(define mat-2
  '((0 0 0 0)
    (0 0 0 0)
    (0 0 0 0)
    (0 0 0 0)))

(my-assert-equals (solve mat-2) '((1 2 3 4)
                                  (3 4 1 2)
                                  (2 1 4 3)
                                  (4 3 2 1)) )

(define mat-3
  '((0 2 0 4)
    (0 4 0 0)
    (2 0 4 0)
    (0 0 2 3)))

(my-assert-equals (solve mat-3) '((1 2 3 4)
                                  (3 4 1 2)
                                  (2 3 4 1)
                                  (4 1 2 3)) )

(define mat-4
  '((0 3 1 4)
    (0 4 0 0)
    (2 0 4 0)
    (0 0 2 3)))

(my-assert-equals (solve mat-4) "solution does not exist" )

(define mat-5
  '((0 0 8 0 6 2 0 0 0)
    (0 3 0 8 4 0 9 0 2)
    (9 0 6 0 0 0 0 1 4)
    (0 1 2 0 0 0 6 0 0)
    (3 0 0 0 0 0 0 2 0)
    (0 6 0 0 0 0 0 3 7)
    (0 0 1 7 8 0 3 0 0)
    (6 8 5 2 0 0 7 4 0)
    (4 0 0 0 9 6 0 0 1)))

(my-assert-equals (solve mat-5) '((1 4 8 9 6 2 5 7 3)
                                  (5 3 7 8 4 1 9 6 2)
                                  (9 2 6 3 5 7 8 1 4)
                                  (7 1 2 4 3 8 6 9 5)
                                  (3 5 4 6 7 9 1 2 8)
                                  (8 6 9 1 2 5 4 3 7)
                                  (2 9 1 7 8 4 3 5 6)
                                  (6 8 5 2 1 3 7 4 9)
                                  (4 7 3 5 9 6 2 8 1)) )

(define mat-6
  '((5 3 0 0 7 0 0 1 2)
    (6 0 0 1 9 5 0 4 0)
    (0 9 8 0 0 0 0 6 0)
    (8 0 0 0 6 0 0 2 3)
    (4 0 0 8 0 3 7 9 1)
    (7 0 0 0 2 0 0 0 6)
    (9 6 1 0 3 7 2 8 0)
    (0 8 0 4 1 9 0 0 5)
    (3 4 0 0 8 0 0 7 9)))

(my-assert-equals (solve mat-6) '((5 3 4 6 7 8 9 1 2)
                                  (6 7 2 1 9 5 3 4 8)
                                  (1 9 8 3 4 2 5 6 7)
                                  (8 5 9 7 6 1 4 2 3)
                                  (4 2 6 8 5 3 7 9 1)
                                  (7 1 3 9 2 4 8 5 6)
                                  (9 6 1 5 3 7 2 8 4)
                                  (2 8 7 4 1 9 6 3 5)
                                  (3 4 5 2 8 6 1 7 9)) )

(define mat-7
  '((4 0 0 0 7 0 0 1 2)
    (0 0 0 1 9 5 0 4 0)
    (0 0 8 0 0 0 0 6 0)
    (8 0 0 0 6 0 0 2 3)
    (4 2 0 8 0 3 7 9 1)
    (7 0 0 0 2 0 0 0 6)
    (9 6 1 0 3 7 2 8 0)
    (0 8 2 4 1 9 0 0 5)
    (3 4 0 0 8 0 0 7 9)))

(my-assert-equals (solve mat-7) "solution does not exist")

(define mat-8
  '(( 0  6  3  0  0  0  2  5  0 14  4  8 12  9  0  0)
    ( 2  9  1 11  6  8 13  7 16  5 15 12 14  3  4 10)
    ( 4  7  5 16 10  9 12 14 11  6  1  3  8 15  2 13)
    ( 0  8 12 14  3  4  0 15  0  2 10  9  1  6 11  5)
    ( 0  0  2  0 13  5 14  0 12 15  0  0  0 10  3  0)
    ( 0 14 13 12  0  0  7  0  2 10  0  5 11  4  6  1)
    (15 10  6  8  2 16 11  0  9  4  3  0  7 13  5  0)
    ( 3  5 11  7  4 10  9  0  0 13  8  6 16  2 15 12)
    ( 1 15  9 13  8 12  4 10  5  3  6  0  2 14  7  0)
    ( 5 12  0  4  0 13  6  2 15  7  9 16  3  8  1  0)
    ( 8  2 14  6  7 11  3 16  1 12 13 10  4  5  9 15)
    ( 7 11 16  3  5  0  0  9  0  8  2 14 13 12 10  6)
    (12  0  4  0  9 14  8 11  6  0  5  0 15  7 13  3)
    (11  3  8  5  1  7 15 13 10  9 14  4  6 16 12  2)
    (14  1 15  2 12  0 10  4  3 16  7 13  5 11  8  9)
    ( 0 13  7  0  0  2  5  3  8 11 12 15 10  0 14  0)))

(my-assert-equals (solve mat-8) '((10  6  3 15 11  1  2  5 13 14  4  8 12  9 16  7)
                                  ( 2  9  1 11  6  8 13  7 16  5 15 12 14  3  4 10)
                                  ( 4  7  5 16 10  9 12 14 11  6  1  3  8 15  2 13)
                                  (13  8 12 14  3  4 16 15  7  2 10  9  1  6 11  5)
                                  (16  4  2  1 13  5 14  6 12 15 11  7  9 10  3  8)
                                  ( 9 14 13 12 15  3  7  8  2 10 16  5 11  4  6  1)
                                  (15 10  6  8  2 16 11 12  9  4  3  1  7 13  5 14)
                                  ( 3  5 11  7  4 10  9  1 14 13  8  6 16  2 15 12)
                                  ( 1 15  9 13  8 12  4 10  5  3  6 11  2 14  7 16)
                                  ( 5 12 10  4 14 13  6  2 15  7  9 16  3  8  1 11)
                                  ( 8  2 14  6  7 11  3 16  1 12 13 10  4  5  9 15)
                                  ( 7 11 16  3  5 15  1  9  4  8  2 14 13 12 10  6)
                                  (12 16  4 10  9 14  8 11  6  1  5  2 15  7 13  3)
                                  (11  3  8  5  1  7 15 13 10  9 14  4  6 16 12  2)
                                  (14  1 15  2 12  6 10  4  3 16  7 13  5 11  8  9)
                                  ( 6 13  7  9 16  2  5  3  8 11 12 15 10  1 14  4)))

