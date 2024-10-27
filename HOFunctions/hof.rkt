#lang racket

(require compatibility/defmacro)

(define-macro (my-assert-equals body1 body2)
  `(unless (equal? ,body1 ,body2)
     (error (format "Failed assertion '(assert-equals ~a ~a)'" ',body1 ',body2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; my-foldl function

 (define (my-foldl func init lst)
  (if (null? lst)
      init
      (my-foldl func (func (car lst) init) (cdr lst))))

; my-foldl function tests

(my-assert-equals (my-foldl + 0 '())
                  0)
(my-assert-equals (my-foldl + 0 '(1))
                  1)
(my-assert-equals (my-foldl + 0 '(1 2 3 4 5 6 7 8 9 10))
                  55)
(my-assert-equals (my-foldl * 1 '(1 2 3 4 5))
                  120)
(my-assert-equals (my-foldl cons '() '(1 2 3 4 5))
                  '(5 4 3 2 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; my-foldr function

(define (my-foldr func init lst)
  (if (null? lst)
      init
      (func (car lst) (my-foldr func init (cdr lst)))))

; my-foldr function tests

(my-assert-equals (my-foldr + 0 '())
                  0)
(my-assert-equals (my-foldr + 0 '(1))
                  1)
(my-assert-equals (my-foldr + 0 '(1 2 3 4 5 6 7 8 9 10))
                  55)
(my-assert-equals (my-foldr * 1 '(1 2 3 4))
                  24)
(my-assert-equals (my-foldr (lambda (x acc) (cons (* x x) acc)) null '(1 2 3 4))
                  '(1 4 9 16))
(my-assert-equals (my-foldr (lambda (x acc) (cons (+ x 1) acc)) null '(1 2 3 4))
                  '(2 3 4 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; fold-tree function

(define (fold-tree func init tree)
  (if (null? tree)
      init
      (func (car tree)
            (apply func (map (lambda (x) (fold-tree func init x)) (cdr tree) )))))

; fold-tree function tests

(my-assert-equals (fold-tree + 0 '(1 (2 ()) (3 ())))
                  6)
(my-assert-equals (fold-tree * 1 '(1 (2 ()) (3 ())))
                  6)
(my-assert-equals (fold-tree + 0 '(1 (2 (10) (2) (10 (1) (9))) (3 (7)) (8 (2))))
                  55)
(my-assert-equals (fold-tree * 1 '(1 (2 (10) (2) (10 (1) (9))) (3 (7)) (8 (2))))
                  1209600)
(my-assert-equals (fold-tree + 0 '(1 (1 (1 (1 (1 (1) (1) (1) (1) (1) ))))))
                  10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; my-map-r function

(define (my-map-r func lst)
  (my-foldr (lambda  (x acc) (cons (func x) acc)) null lst))

; my-map-r function tests

(my-assert-equals (my-map-r (lambda (x) (* x x)) '(1 2 3))
                  '(1 4 9))
(my-assert-equals (my-map-r (lambda (x) ( * (* x x) x)) '(1 2 3))
                  '(1 8 27))
(my-assert-equals (my-map-r - '(1 2 3 4 5 6 7 8 9))
                  '(-1 -2 -3 -4 -5 -6 -7 -8 -9))
(my-assert-equals (my-map-r (lambda (x) (> x 5)) '(1 2 3 4 5 6 7 8 9 10))
                  '(#f #f #f #f #f #t #t #t #t #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; my-map-l function

(define (my-map-l func lst)
  (my-foldl (lambda  (x acc) (cons acc (func x))) null lst))

; my-map-l function tests

(my-assert-equals (my-map-l (lambda (x) (* x x)) '(1 2 3))
                  '(((() . 1) . 4) . 9))
(my-assert-equals (my-map-l - '(1 2 3 4 5 6 7 8 9))
                  '(((((((((() . -1) . -2) . -3) . -4) . -5) . -6) . -7) . -8) . -9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; my-reduce-r function

(define (my-reduce-r func lst)
  (if (null? lst)
      null
      (my-foldr func (car lst) (cdr lst))))

; my-reduce-r function tests

(my-assert-equals (my-reduce-r + '(1))
                  1)
(my-assert-equals (my-reduce-r + '(1 2 3 4 5 6 7 8 9 10))
                  55)
(my-assert-equals (my-reduce-r * '(1 2 3 4 5))
                  120)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; my-reduce-l function

(define (my-reduce-l func lst)
  (if (null? lst)
      null
      (my-foldl func (car lst) (cdr lst))))

; my-reduce-l function tests

(my-assert-equals (my-reduce-l + '(1))
                  1)
(my-assert-equals (my-reduce-l + '(1 2 3 4 5 6 7 8 9 10))
                  55)
(my-assert-equals (my-reduce-l * '(1 2 3 4 5))
                  120)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; my-filter-r function

(define (my-filter-r func lst)
  (my-foldr (lambda (x acc)
              (cond
                [(func x) (cons x acc)]
                [else acc]))
            null lst))

; my-filter-r function tests

(my-assert-equals (my-filter-r (lambda (x) (> x 5)) '(1 2 3 6 7 8 9))
                  '(6 7 8 9))
(my-assert-equals (my-filter-r (lambda (x) (and (>= x 8) (<= x 10))) '(100 22 33 4 5 9 10))
                  '(9 10))
(my-assert-equals (my-filter-r (lambda (x) (equal? (modulo x 2) 0)) '(1 2 3 6 7 8 9))
                  '(2 6 8))
(my-assert-equals (my-filter-r (lambda (x) (equal? x "ppa")) '("ppa" "aag" "ag1" "zdm"))
                  '("ppa"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; my-filter-l function

(define (my-filter-l func lst)
  (my-foldl (lambda (x acc)
              (cond
                [(func x) (cons x acc)]
                [else acc]))
            null lst))

; my-filter-l function tests

(my-assert-equals (my-filter-l (lambda (x) (> x 5)) '(1 2 3 6 7 8 9))
                  '(9 8 7 6))
(my-assert-equals (my-filter-l (lambda (x) (and (>= x 8) (<= x 10))) '(100 22 33 4 5 9 10))
                  '(10 9))
(my-assert-equals (my-filter-l (lambda (x) (equal? (modulo x 2) 0)) '(1 2 3 6 7 8 9))
                  '(8 6 2))
(my-assert-equals (my-filter-l (lambda (x) (equal? x "ppa")) '("ppa" "aag" "ag1" "zdm"))
                  '("ppa"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; bubble-sort

(define (my-atom x)
  (cond
    [(or (null? x) (pair? x)) #f]
    [else #t]))

(define (my-length-list lst)
  (cond
    [(null? lst) 0]
    [(my-atom (car lst))
     (+ (my-length-list (cdr lst)) 1)]))

(define (bubble lst comp)
   (cond
     [(null? (cdr lst)) lst]
     [(comp (car lst) (cadr lst))
      (cons (cadr lst) (bubble (cons (car lst) (cddr lst)) comp))]
     [else (cons (car lst) (bubble (cdr lst) comp))]))

(define (bubble-sort comp lst)
  (bubble-sort-aux lst (my-length-list lst) comp))
(define (bubble-sort-aux lst length comp)
  (cond
    [(null? lst) null]
    [(equal? length 1) lst]
    [else (bubble-sort-aux (bubble lst comp) (- length 1) comp)]))

; bubble-sort tests

(my-assert-equals (bubble-sort > '(2 3 4 2 1 5))
                  '(1 2 2 3 4 5))
(my-assert-equals (bubble-sort > '( 3 4 2 9 8 6 7 5 3 2 3 4 2 1 5))
                  '(1 2 2 2 3 3 3 4 4 5 5 6 7 8 9))
(my-assert-equals (bubble-sort > '(1))
                  '(1))
(my-assert-equals (bubble-sort > '(5 4 3 2 1))
                  '(1 2 3 4 5))
(my-assert-equals (bubble-sort > '(5 4 3 2 1 1 2 3 4 5 0 0 1 1 2 2 3 3 4 4 5 5))
                  '(0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5))
(my-assert-equals (bubble-sort < '(2 3 4 2 1 5))
                  '(5 4 3 2 2 1))
(my-assert-equals (bubble-sort < '(5 4 3 2 1 1 2 3 4 5 0 0 1 1 2 2 3 3 4 4 5 5))
                  '(5 5 5 5 4 4 4 4 3 3 3 3 2 2 2 2 1 1 1 1 0 0))
(my-assert-equals (bubble-sort = '())
                  '())
(my-assert-equals (bubble-sort >= '(5 4 3 2 1))
                  '(1 2 3 4 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; insertion-sort

(define (insert comp lst1 lst2)
  (cond
    [(null? lst1) lst2]
    [(null? lst2) lst1]
    [(comp (car lst1) (car lst2))
     (cons (car lst2) (insert comp (cdr lst2) lst1))]
    [else (cons (car lst1) (insert comp (cdr lst1) lst2))]))

(define (insertion-sort comp lst)
  (if (null? lst)
      null
      (insert comp (cons (car lst) null) (insertion-sort comp (cdr lst)))))

; insertion-sort tests

(my-assert-equals (insertion-sort > '(2 3 4 2 1 5))
                  '(1 2 2 3 4 5))
(my-assert-equals (insertion-sort > '( 3 4 2 9 8 6 7 5 3 2 3 4 2 1 5))
                  '(1 2 2 2 3 3 3 4 4 5 5 6 7 8 9))
(my-assert-equals (insertion-sort > '(1))
                  '(1))
(my-assert-equals (insertion-sort > '(5 4 3 2 1))
                  '(1 2 3 4 5))
(my-assert-equals (insertion-sort > '(5 4 3 2 1 1 2 3 4 5 0 0 1 1 2 2 3 3 4 4 5 5))
                  '(0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5))
(my-assert-equals (insertion-sort < '(2 3 4 2 1 5))
                  '(5 4 3 2 2 1))
(my-assert-equals (insertion-sort < '(5 4 3 2 1 1 2 3 4 5 0 0 1 1 2 2 3 3 4 4 5 5))
                  '(5 5 5 5 4 4 4 4 3 3 3 3 2 2 2 2 1 1 1 1 0 0))
(my-assert-equals (insertion-sort = '())
                  '())
(my-assert-equals (insertion-sort >= '(5 4 3 2 1))
                  '(1 2 3 4 5))
