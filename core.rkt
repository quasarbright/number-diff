#lang racket

;;; module interface ;;;

(module+ examples
  (provide (all-defined-out)))

(module+ test
  (require rackunit)
  (require (submod ".." examples)))

(provide #;()
         (struct-out dnumber)
         (struct-out dchild)
         #;(DNumber DNumber -> DNumber)
         #;(derivative y x)
         ; computes the derivative of y with respect to x
         derivative)

;;; dependencies ;;;

(require)

;;; data definitions ;;;

(struct dchild [input derivative] #:transparent)
; A DChild is a
#;(dchild DNumber number?)
; TODO make derivative a DNumber to support higher order derivatives
; Represents an input to a differentiable computation and its derivative
; where
; input is the input
; derivative is the numerical value of its first derivative (plain number)

(struct dnumber [value children] #:transparent)
; A DNumber is a
#;(dnumber number? (listof DChild))
; Represents the result of a differentiable computation
; where
; value is the numerical result (plain number)
; children are the inputs and their derivatives
(module+ examples
  ; 2 * 3 = 6
  (define plain2 (dnumber 2 '()))
  (define plain3 (dnumber 3 '()))
  (define plain4 (dnumber 4 '()))
  (define plain5 (dnumber 5 '()))
  ; d(* x y) / dx = y WLOG
  (define factor2 (dchild plain2 3))
  (define factor3 (dchild plain3 2))
  (define prod23 (dnumber 6 (list factor2 factor3)))
  ; 4 + 5 = 9
  ; d(+ x y) / dx = 1 WLOG
  (define term4 (dchild plain4 1))
  (define term5 (dchild plain5 1))
  (define sum45 (dnumber 9 (list term4 term5)))
  ; (2 * 3) * (4 + 5)
  (define prod69 (dnumber 100 (list (dchild prod23 9) (dchild sum45 6))))
  ; (2 * 3 + 1) ^ 2
  ; the second 2 isn't eq? to the first 2, so don't worry about that
  (define square-sum (dnumber 49 (list (dchild (dnumber 7 (list (dchild prod23 1)
                                                                (dchild (dnumber 1 '()) 1)))
                                               ; d x^2 / dx = 2*x
                                               14)
                                       (dchild (dnumber 2 '())
                                               ; d 6^x / dx = 6^x * ln(x)
                                               (* 36 (log 6))))))
  ; 3^2
  (define square3 (dnumber 9 (list (dchild plain3 (* 2 3))
                                   (dchild plain2 (* 9 (log 3))))))
  ; 3^3 like x^x
  ; they're the same 3
  (define self-exp3 (dnumber 27 (list (dchild plain3 27)
                                      (dchild plain3 (* 27 (log 3))))))
  )

;;; functionality ;;;

#;(DNumber DNumber -> number?)
; computes the derivative of y with respect to x
(define (derivative y x)
  (if (eq? y x)
      1
      (match-let ([(dnumber _ (list (dchild u* dy/du*) ...)) y])
        (for/sum ([dy/du dy/du*]
                  [u u*])
          ; chain rule
          (* dy/du (derivative u x))))))

;;; tests ;;;

(module+ test
  ; self
  (check-equal? (derivative plain2 plain2) 1)
  ; constant
  (check-equal? (derivative plain2 plain3) 0)
  ; binary addition
  (check-equal? (derivative sum45 plain4) 1)
  ; binary multiplication
  (check-equal? (derivative prod23 plain2) 3)
  ; square
  (check-equal? (derivative square3 plain3) 6)
  ; square-sum
  (check-equal? (derivative square-sum plain2) (* (* 2 7) 3))
  ; e^x
  ; x^x (derivative is x^x * (ln(x) + 1))
  (check-equal? (derivative self-exp3 plain3) (+ 27 (* 27 (log 3))))
  ; For recursive numbers: L(a) = 1 + a * L(a), dL/da = a^2 (L = 1 / (1 - a))
  )
