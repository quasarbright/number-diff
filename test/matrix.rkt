#lang racket

;; A tiny matrix library

; Module Interface ;

(module+ test (require rackunit))
(provide matrix?
         matrix/c
         ; parameters for custom arithmetic
         current-*
         current-+
         #;(number? -> any/c)
         ; lift a plain number into custom number system
         current-plain-number
         ; matrix operations
         (contract-out
          ; variadic matrix multiplication
          ; shapes must line up
          [m* (->* () #:rest (cons/c matrix? (listof matrix?)) matrix?)]
          ; variadic matrix addition
          ; shapes must be the same
          [m+ (->* () #:rest (cons/c matrix? (listof matrix?)) matrix?)]
          ; swaps rows and columns
          [matrix-transpose (-> matrix? matrix?)]
          ; get number of rows and columns
          [matrix-shape (-> matrix? (list/c natural? natural?))]
          ; like map for matrices
          [matrix-map (->* (procedure?) #:rest (cons/c matrix? (listof matrix?)) matrix?)]))

; Dependencies ;

(require)

; Data Definitions ;

; A [Matrix A m n] is a
#;(listof (listof A))
; CONSTRAINT: outer list has length m and inner lists have length n
; CONSTRAINT: n > 0, m > 0
; Represents a Matrix or a 2D array of numbers of type A with m rows and n columns
; Examples:
(define id2
  '((1 0)
    (0 1)))

(define matrix? (listof list?))
(define (matrix/c a/c r c)
  (and/c (listof (listof a/c))
         (λ (mat) (and (= r (length mat)) (apply = c (map length mat))))))

; Funcitonality ;

(define current-* (make-parameter *))
(define current-+ (make-parameter +))
(define current-plain-number (make-parameter identity))


#;((Matrix A m n) ...+ -> (Matrix A m n))
; Variadic matrix addition
(define (m+ mat1 . mats)
  (assert-same-shapes 'm+ (cons mat1 mats))
  (apply matrix-map (current-+) mat1 mats))

#;(symbol? (listof Matrix))
; assert matrices are the same shape
(define (assert-same-shapes who mats)
  (define shapes (map matrix-shape mats))
  (unless (apply equal? shapes)
    (error who "expected matrices of equal dimensions, got ~a" shapes)))

#;((Matrix A m n) -> (list m n))
; get the shape of a matrix
(define (matrix-shape mat) (list (num-rows mat) (num-cols mat)))

#;(A ... -> A)
; sum using current-+
(define (sum nums)
  (for/fold ([sum ((current-plain-number) 0)])
            ([num nums])
    ((current-+) sum num)))

#;((Matrix A m n1) (Matrix A n1 n2) ... (Matrix A n2 n) -> (Matrix A m n))
; Variadic matrix multiplication.
; Each matrix's number of columns must be equal to the next one's number of rows.
(define (m* mat1 . mats)
  (for/fold ([product mat1])
            ([mat mats])
    (m*/bin product mat)))

#;((Matrix A m p) (Matrix A p n) -> (Matrix ))
; Binary matrix multiplication
(define (m*/bin mat1 mat2)
  (unless (= (num-cols mat1) (num-rows mat2))
    (error 'm* "matrix multiplication with incompatible dimensions: ~a vs ~a"
           (matrix-shape mat1)
           (matrix-shape mat2)))
  (for/list ([row (matrix-rows mat1)])
    (for/list ([col (matrix-cols mat2)])
      (vector-dot row col))))

#;((Matrix A m n) -> (listof (listof A)))
; Convert a matrix to a list of row lists
(define matrix-rows identity)

#;((Matrix A m n) -> (listof (listof A)))
; Convert a matrix to a list of column lists
(define (matrix-cols mat) (matrix-rows (matrix-transpose mat)))

#;((Matrix A m n) -> (Matrix A n m))
; Matrix transpose
(define (matrix-transpose mat)
  (for/list ([c (num-cols mat)])
    (for/list ([r (num-rows mat)])
      (matrix-ref mat r c))))

(module+ test
  (check-equal? (matrix-transpose '((1 2)
                                    (3 4)
                                    (5 6)))
                '((1 3 5)
                  (2 4 6))))

#;(Matrix -> natural?)
; number of columns in a matrix
(define (num-cols mat) (length (car mat)))

#;(Matrix -> natural?)
; number of rows in a matrix
(define (num-rows mat) (length mat))

#;((Matrix A m n) natural? natural? -> A)
; Get element of mat at row r, column c
(define (matrix-ref mat r c)
  (list-ref (list-ref mat r) c))

#;((listof A) (listof A) -> A)
; dot product
; sum of pairwise products of two vectors
(define (vector-dot v1 v2)
  (sum (map (current-*) v1 v2)))

#;((A B ... -> R) (Matrix A m n) (Matrix B m n) ... -> (Matrix R m n))
; like map for matrices
(define (matrix-map proc mat1 . mats)
  (assert-same-shapes 'matrix-map (cons mat1 mats))
  (for/list ([r (in-range (num-rows mat1))])
    (for/list ([c (in-range (num-cols mat1))])
      (apply proc (map (λ (mat) (matrix-ref mat r c)) (cons mat1 mats))))))

; Tests

(module+ test
  (define m1
    '((1 2 3)
      (4 5 6)))
  (define m2
    '((1 2 3 4)
      (5 6 7 8)
      (9 10 11 12)))
  (define m1m2
    '((38 44 50 56)
      (83 98 113 128)))
  (check-equal? (m* m1 m2) m1m2)
  (check-equal?
   (m+ '((1 2 3)
         (4 5 6))
       '((7 8 9)
         (10 11 12)))
   '((8 10 12)
     (14 16 18))))
