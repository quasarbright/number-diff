#lang racket

;;; module interface ;;;

(module+ test (require rackunit))
(provide #;((or/c dnumber? number?) -> dnumber?)
         ; converts a number to a DNumber, does nothing if it is one.
         ->dnumber
         #;((number? ... -> (values number? (listof number?)))
            ->
            (dnumber? ... -> dnumber?))
         ; converts the given differentiable function to a function from DNumbers to a DNumber
         ; the given function uses plain numbers and must return its result and its gradient
         ; the resulting function produces a DNumber that stores the gradient of input DNumbers.
         make-operator
         ; differentiable operators
         +o
         -o
         *o
         expto
         expo
         /o
         ; like for/sum
         for/sumo)

;;; dependencies ;;;

(require racket/generator "core.rkt")

;;; data definitions ;;;

; An Operator is a
#;(DNumber ... -> DNumber)
; Represents a differentiable function.
; The output is a DNumber whose value is the result of the computation
; and whose children correspond to the partial derivatives of the function with
; respect to each argument

;;; functionality ;;;

(define (plain-number n) (dnumber n '()))

(define (->dnumber n) (if (dnumber? n) n (plain-number n)))

#;((number? ... -> (values number? (listof number?))) -> Operator)
; converts the given differentiable function to an Operator
; fun returns its result and its gradient
(define (make-operator fun)
  (lambda dnums
    (let ([dnums (map ->dnumber dnums)])
      (define nums (map (compose dnumber-value ->dnumber) dnums))
      (define-values (value derivatives) (apply fun nums))
      (dnumber value
               (for/list ([x dnums]
                          [d/dx derivatives])
                 (dchild x d/dx))))))

; Operator
; addition
(define +o
  (make-operator (lambda nums (values (apply + nums) (map (λ (x) 1) nums)))))

; Operator
; subtraction
(define (-o z . ws)
  (if (null? ws)
      (*o -1 z)
      (+o z (*o -1 (apply +o ws)))))

; Operator
; multiplication
(define *o
  (make-operator (lambda nums (values (apply * nums) (dtimes nums)))))

#;(number? ... -> (listof number?))
; gradient of multiplication
(define (dtimes nums)
  (for/list ([(lefts _ rights) (in-zippers nums)])
    (* (apply * lefts) (apply * rights))))

(module+ test
  (check-equal? (dtimes '(2 3 4 5)) '(60 40 30 24))
  (check-equal? (dtimes '(2 3 0 5)) '(0 0 30 0))
  (check-equal? (dtimes '(2 0 0 5)) '(0 0 0 0)))

#;(list? -> sequence?)
; generates a sequence of zipper states for each position in the list.
; each element has three values: the previous elements in reverse, the current element, and the next elements
; see tests for an example.
; kind of like comonad duplicate.
(define (in-zippers lst)
  (in-generator #:arity 3
                (let loop ([prevs '()] [lst lst])
                  (when (cons? lst)
                    (yield prevs (car lst) (cdr lst))
                    (loop (cons (car lst) prevs) (cdr lst))))))

(module+ test
  (check-equal? (for/list ([(prevs x nexts) (in-zippers '(1 2 3))])
                  (list prevs x nexts))
                '((() 1 (2 3))
                  ((1) 2 (3))
                  ((2 1) 3 ()))))

; Operator
; exponentiation
(define expto
  (make-operator (lambda (z w) (values (expt z w)
                                       (list (* w (expt z (sub1 w)))
                                             (* (log z) (expt z w)))))))

; Operator
; e^x
; not really necessary, but saves a log and keeps things small
(define expo
  (make-operator (lambda (w) (values (exp w) (list (exp w))))))

; Operator
; 1 / x
; not really necessary, but saves a log and keeps things small
(define reciprocalo
  (make-operator (lambda (w) (values (/ w) (list (- (/ (* w w))))))))

; Operator
; division, same behavior as /
(define (/o z . ws)
  ; this is a good example because it just composes primitives
  ; no make-operator needed!
  (if (null? ws)
      (reciprocalo z)
      (*o z (reciprocalo (apply *o ws)))))


; TODO use the derived fold thing
(define-syntax-rule
  (for/sumo (clause ...) body ...)
  (for/fold ([sum (->dnumber 0)])
            (clause ...)
    (+o sum (let () body ...))))

;;; tests ;;;

(module+ test
  (require (submod "core.rkt" examples))
  (check-equal? (derivative (+o plain2 3) plain2) 1)
  (check-equal? (derivative (+o plain3 plain3) plain3) 2)
  (check-equal? (derivative (*o plain2 3) plain2) 3)
  (check-equal? (derivative (*o 2 plain3 4) plain3) 8)
  (check-equal? (derivative (*o plain3 plain3) plain3) 6)
  (check-equal? (derivative (*o plain3 plain3 plain3 plain3) plain3) 108)
  (check-equal? (derivative (expto plain3 4) plain3) 108)
  (check-equal? (derivative (expto (exp 1) plain3) plain3) (* (log (exp 1)) (expt (exp 1) 3)))
  (check-equal? (derivative (expo plain3) plain3) (exp 3))
  (check-equal? (derivative (reciprocalo plain3) plain3) (- (/ 9)))
  (check-equal? (derivative (/o plain3) plain3) (- (/ 9)))
  (check-equal? (derivative (/o 6 plain3) plain3) (- (/ 2 3)))
  (check-equal? (dnumber-value (/o 6 plain3)) 2))
