#lang racket

;;; module interface ;;;

(module+ test (require rackunit))
(provide (contract-out
          [-o (->* ((or/c number? dnumber?))
                   #:rest (listof (or/c number? dnumber?))
                   dnumber?)]
          ; a ^ b
          [expto (-> (or/c number? dnumber?)
                     (or/c number? dnumber?)
                     dnumber?)]
          ; e ^ x
          [expo (-> (or/c number? dnumber?)
                    dnumber?)]
          ; second argument is optional base, defaults to e
          [logo (->* ((or/c number? dnumber?))
                     ((or/c number? dnumber?))
                     dnumber?)]
          [/o (->* ((or/c number? dnumber?))
                   #:rest (listof (or/c number? dnumber?))
                   dnumber?)]))

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

; Operator
; subtraction
(define (-o z . ws)
  (if (null? ws)
      (*o -1 z)
      (+o z (*o -1 (apply +o ws)))))

; Operator
; exponentiation
(define (expto z w)
  (let ([z (ensure-dnumber z)]
        [w (ensure-dnumber w)])
    (define result
      (dnumber (expt (dnumber->number z) (dnumber->number w))
               (delay (list (dchild z (*o w (expto z (-o w 1))))
                            (dchild w (*o (logo z) result))))))
    result))

; Operator
; e^x
; not really necessary, but saves a log and keeps things small
(define (expo z)
  (let ([z (ensure-dnumber z)])
    (define result
      (dnumber (exp (dnumber->number z))
               (delay (list (dchild z result)))))
    result))

; Operator
; log (default base is e)
(define (logo z [b #f])
  (if b
      (/o (lno z) (lno b))
      (lno z)))

; Operator
; natural log
(define (lno z)
  (let ([z (ensure-dnumber z)])
    (dnumber (log (dnumber->number z))
             (delay (list (dchild z (reciprocalo z)))))))

; Operator
; 1 / x
; can be expressed with expto and *o, but saves a log and keeps things small
(define (reciprocalo z)
  (let ([z (ensure-dnumber z)])
    (dnumber (/ (dnumber->number z))
             (delay (list (dchild z (-o (reciprocalo (*o z z)))))))))

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
  (check-equal? (dnumber->number (derivative (+o plain2 3) plain2)) 1)
  (check-equal? (dnumber->number (derivative (+o plain3 plain3) plain3)) 2)
  (check-equal? (dnumber->number (derivative (*o plain2 3) plain2)) 3)
  (check-equal? (dnumber->number (derivative (*o 2 plain3 4) plain3)) 8)
  (check-equal? (dnumber->number (derivative (*o plain3 plain3) plain3)) 6)
  (check-equal? (dnumber->number (derivative (*o plain3 plain3 plain3 plain3) plain3)) 108)
  (check-equal? (dnumber->number (derivative (expto plain3 4) plain3)) 108)
  (check-equal? (dnumber->number (derivative (expto (exp 1) plain3) plain3)) (* (log (exp 1)) (expt (exp 1) 3)))
  (check-equal? (dnumber->number (derivative (expo plain3) plain3)) (exp 3))
  (check-equal? (dnumber->number (derivative (expo plain3) plain3 #:order 2)) (exp 3))
  (check-equal? (dnumber->number (derivative (expo plain3) plain3 #:order 4)) (exp 3))
  (check-equal? (dnumber->number (derivative (reciprocalo plain3) plain3)) (- (/ 9)))
  (check-equal? (dnumber->number (derivative (/o plain3) plain3)) (- (/ 9)))
  (check-equal? (dnumber->number (derivative (/o plain3) plain3 #:order 2)) 2/27)
  (check-equal? (dnumber->number (derivative (/o 6 plain3) plain3)) (- (/ 2 3)))
  (check-equal? (dnumber->number (/o 6 plain3)) 2)
  (check-equal? (dnumber->number (derivative (expto plain3 4) plain3 #:order 4)) 24)
  (check-equal? (dnumber->number (derivative (logo plain3) plain3)) 1/3)
  (check-equal? (dnumber->number (derivative (logo plain3) plain3 #:order 2)) -1/9)
  (check-equal? (dnumber->number (derivative (logo plain3 plain2) plain3)) (/ 1/3 (log 2)))
  (check-equal? (dnumber->number (derivative (logo plain3 plain2) plain3 #:order 2)) (/ -1/9 (log 2)))
  (check-equal? (dnumber->number (derivative (logo plain3 plain2) plain2)) (- (/ (log 3) (* 2 (log 2) (log 2)))))
  (check-within (dnumber->number (derivative (logo plain3 plain2) plain2 #:order 2))
                (/ (* (log 3) (+ (log 2) 2))
                   (* 4 (log 2) (log 2) (log 2)))
                1e-10))
