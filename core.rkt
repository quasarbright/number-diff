#lang racket

;;; module interface ;;;

(module+ examples
  (provide (all-defined-out)))

(module+ test
  (require rackunit)
  (require (submod ".." examples)))

(provide ; data types
         (struct-out dnumber)
         (struct-out dchild)
         (contract-out
          [number->dnumber (-> number? dnumber?)]
          [ensure-dnumber (-> (or/c number? dnumber?) dnumber?)]
          [dnumber->number (-> dnumber? number?)]
          #;(derivative y x #:order n)
          ; computes the nth derivative of y with respect to x
          [derivative (->* (dnumber? dnumber?) (#:order natural?) dnumber?)]
          [+o (->* () #:rest (listof (or/c dnumber? number?)) dnumber?)]
          [*o (->* () #:rest (listof (or/c dnumber? number?)) dnumber?)])
         for/sumo
         for/producto)

;;; dependencies ;;;

(require)

;;; data definitions ;;;

(struct dnumber [value children] #:transparent)
; A DNumber is a
#;(dnumber number? (promise/c (listof DChild)))
; Represents the result of a differentiable computation
; where
; value is the numerical result (plain number)
; children are the inputs and their derivatives.
; The list of children is lazy because derivatives of functions like exp lead to infinite
; trees of derivative children.
; CONSTRAINT: The graph formed from a DNumber and its child values (ignoring derivatives) must be a DAG.
; In other words, a DNumber must not contain itself (by eq?) as an input.

(struct dchild [input derivative] #:transparent)
; A DChild is a
#;(dchild DNumber DNumber)
; Represents an input to a differentiable computation and its derivative
; where
; input is the input
; derivative is its first derivative of this child's parent with respect to this input

; TODO figure out how to do make a proper contract for this thing
; It's a mutually recursive chaperone contract. chaperone bc of promise/c
(module+ examples
  ; 2 * 3 = 6
  (define plain2 (number->dnumber 2))
  (define plain3 (number->dnumber 3))
  (define plain4 (number->dnumber 4))
  (define plain5 (number->dnumber 5))
  ; d(* x y) / dx = y WLOG
  (define factor2 (dchild plain2 plain3))
  (define factor3 (dchild plain3 plain2))
  (define prod23 (dnumber 6 (delay (list factor2
                                         factor3))))
  ; 4 + 5 = 9
  ; d(+ x y) / dx = 1 WLOG
  (define term4 (dchild plain4 (number->dnumber 1)))
  (define term5 (dchild plain5 (number->dnumber 1)))
  (define sum45 (dnumber 9 (delay (list term4 term5))))
  ; (2 * 3) * (4 + 5)
  (define prod69 (dnumber 100 (delay (list (dchild prod23 sum45) (dchild sum45 prod23)))))
  ; (2 * 3 + 1) ^ 2
  ; where square is repeated multiplication for now. Otherwise we'd have to do ln and its derivative.
  (define square-sum
    (let ([factor (dnumber 7 (delay (list (dchild prod23 (number->dnumber 1))
                                          (dchild (number->dnumber 1) (number->dnumber 1)))))])
      (dnumber 49 (delay (list (dchild factor factor) (dchild factor factor))))))
  ; 3^2 as repeated multiplication
  (define square3 (dnumber 9 (delay (list (dchild plain3 plain3)
                                          (dchild plain3 plain3)))))
  ; 3^3 like x^x
  ; they're the same 3
  ; I cheat and use constant derivatives to avoid ln's derivative. This'll only work for first order.
  (define self-exp3 (dnumber 27 (delay (list (dchild plain3 (number->dnumber 27))
                                             (dchild plain3 (number->dnumber (* 27 (log 3)))))))))

;;; functionality ;;;

#;(number? -> DNumber)
; convert a plain number to a DNumber.
; The result has no inputs.
(define (number->dnumber n) (dnumber n (delay '())))

#;((or/c number? DNumber) -> DNumber)
; Convert a number to a DNumber if it isn't one already
(define (ensure-dnumber n) (if (dnumber? n) n (number->dnumber n)))

#;(DNumber -> number?)
; Convert a DNumber to a number
(define (dnumber->number n) (dnumber-value n))

#;(DNumber DNumber [#:order natural?] -> number?)
; computes the derivative of y with respect to x
; optionally supply order of the derivative. e.g. 1 for first derivative, 2 for second derivative
(define (derivative y x #:order [n 1])
  ; single derivative
  (define (d/dx y)
    (if (eq? y x)
        (number->dnumber 1)
        (match-let ([(dnumber _ (app force (list (dchild u* dy/du*) ...))) y])
          (for/sumo ([dy/du dy/du*]
                     [u u*])
            ; chain rule
            (*o dy/du (derivative u x))))))
  ; apply d/dx n times
  (let loop ([n n] [y y])
    (if (zero? n)
        y
        (loop (sub1 n) (d/dx y)))))

; TODO you get exponential blowup from extra + 0 in for/sumo while computing higher order derivatives
; Ideas:
; * instead of blindly converting numbers to dnumbers, leave them out of the derivative
; * instead of a for/sumo, do apply +o and make a special case for (+ n). dirty.

; core operators

#;((or/c DNumber number?) ... -> DNumber)
; Differentiable +
; Plain numbers get lifted.
(define (+o . nums)
  (let ([nums (map ensure-dnumber nums)])
    (dnumber (apply + (map dnumber->number nums))
             (delay (for/list ([num nums]) (dchild num (number->dnumber 1)))))))

; TODO proper derived fold
(define-syntax-rule
  (for/sumo (clause ...) body ...)
  (for/fold ([sum (number->dnumber 0)])
            (clause ...)
    (let ([term (let () body ...)])
      (+o sum term))))

#;((or/c DNumber number?) ... -> DNumber)
; Differentiable *
; Plain numbers get lifted.
(define (*o . nums)
  (for/producto ([num nums])
    (ensure-dnumber num)))

; TODO proper derived fold
(define-syntax-rule
  (for/producto (clause ...) body ...)
  (for/fold ([product (number->dnumber 1)])
            (clause ...)
    (let ([factor (let () body ...)])
      (*o/bin product factor))))

#;(DNumber DNumber -> DNumber)
; Differentiable binary multiplication
; Does not lift plain numbers.
(define (*o/bin a b)
  (dnumber (* (dnumber->number a)
              (dnumber->number b))
           (delay (list (dchild a b) (dchild b a)))))

;;; tests ;;;

(module+ test
  ; self
  (check-equal? (dnumber->number (derivative plain2 plain2)) 1)
  ; constant
  (check-equal? (dnumber->number (derivative plain2 plain3)) 0)
  ; binary addition
  (check-equal? (dnumber->number (derivative sum45 plain4)) 1)
  ; binary multiplication
  (check-equal? (dnumber->number (derivative prod23 plain2)) 3)
  ; square
  (check-equal? (dnumber->number (derivative square3 plain3)) 6)
  ; square-sum
  (check-equal? (dnumber->number (derivative square-sum plain2)) (* (* 2 7) 3))
  ; e^x
  ; x^x (derivative is x^x * (ln(x) + 1))
  (check-equal? (dnumber->number (derivative self-exp3 plain3)) (+ 27 (* 27 (log 3))))
  ; For recursive numbers: L(a) = 1 + a * L(a), dL/da = a^2 (L = 1 / (1 - a))
  (check-equal? (dnumber->number (derivative (*o plain2 plain3) plain2)) 3)
  (check-equal? (dnumber->number (derivative (*o plain2 plain3) plain3)) 2)
  ; second derivative of x*x = 2
  (check-equal? (dnumber->number (derivative (derivative (*o plain3 plain3) plain3) plain3)) 2)
  (check-equal? (dnumber->number (derivative (*o plain3 plain3) plain3 #:order 2)) 2)
  ; third derivative of x*x = 0
  (check-equal? (dnumber->number (derivative (*o plain3 plain3) plain3 #:order 3)) 0)
  ; derivatives of x*x*x
  (check-equal? (dnumber->number (derivative (*o plain4 plain4 plain4) plain4)) 48)
  (check-equal? (dnumber->number (derivative (*o plain4 plain4 plain4) plain4 #:order 2)) 24)
  (check-equal? (dnumber->number (derivative (*o plain4 plain4 plain4) plain4 #:order 3)) 6)
  (check-equal? (dnumber->number (derivative (*o plain4 plain4 plain4) plain4 #:order 4)) 0)
  (let ()
    (define (f x)
      (+o (*o 3 x x) (*o 5 x) 1))
    (check-equal? (dnumber->number (derivative (f plain4) plain4)) 29)
    (check-equal? (dnumber->number (derivative (f plain4) plain4 #:order 2)) 6)))
