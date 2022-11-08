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
         *o
         expto
         expo
         /o)

;;; dependencies ;;;

(require "core.rkt")

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
; multiplication
(define *o
  (make-operator (lambda nums (values (apply * nums) (dtimes nums)))))

#;(number? ... -> (listof number?))
; gradient of multiplication
(define (dtimes nums)
  (define prod (apply * nums))
  (map (λ (x) (/ prod x)) nums))

(module+ test
  (check-equal? (dtimes '(2 3 4 5)) '(60 40 30 24)))

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
