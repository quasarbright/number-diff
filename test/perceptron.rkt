#lang racket

; perceptron classifier using auto diff

; module interface ;

(module+ test (require rackunit))
(provide perceptron?
         make-perceptron
         train-perceptron
         test-perceptron)

; dependencies ;

(require "../core.rkt"
         "../operators.rkt")

; data definintions ;

(struct perceptron [weights activation])
; A Perceptron is a
#;(perceptron (Matrix DNumber natural? natural?)
              Operator)
; Represents a single-layer perceptron binary classifier
; with a non-linear activation function like sigmoid or tanh

; functionality ;

; Operator
; sigmoid activation function
(define sigmoid
  (make-operator
   (lambda (x) (let ([result (/ (add1 (exp (- x))))])
                 (values result (list (* result (- 1 result))))))))

#;((listof DNumber) (listof DNumber) -> DNumber)
; mean squared error
(define (mse-loss guesses actuals)
  (/o (for/sumo ([guess guesses]
                 [actual actuals])
        (expto (-o guess actual) 2))
      (length guesses)))

#;(natural? [Operator] -> perceptron?)
; Creates a perceptron that takes in num-inputs inputs (excluding bias)
; and uses the given (optional) activation function.
; Initializes weights to zero
(define (make-perceptron num-inputs [activation sigmoid])
  (perceptron (build-list (add1 num-inputs) (thunk* (->dnumber 0)))
              activation))

#;(perceptron? (listof (list/c (listof number?) (or/c 0 1))) -> perceptron?)
; basic batch gradient optimization
(define (train-perceptron prc
                          data
                          #:loss-fn [loss-fn mse-loss]
                          #:learning-rate [lr 0.1]
                          #:epochs [epochs 100])
  (match-define (list (list inputs outputs) ...) data)
  (for/fold ([prc prc])
            ([epoch (in-range epochs)])
    (train-perceptron/epoch prc inputs outputs loss-fn lr)))

#;(perceptron?
   (listof (listof number?))
   (listof (or/c 0 1))
   ((listof DNumber) (listof DNumber) -> DNumber)
   number?
   ->
   perceptron?)
; one epoch (train on batch once) of training
(define (train-perceptron/epoch prc inputs outputs loss-fn lr)
  (define guesses (run-perceptron* prc inputs))
  (define loss (loss-fn guesses outputs))
  (displayln (dnumber-value loss))
  (backprop prc loss lr))

#;(perceptron? (listof (listof number?)) -> (listof DNumber))
; run on batch of inputs
(define (run-perceptron* prc inputs)
  (map (λ (input) (run-perceptron prc input)) inputs))

#;(perceptron? (listof number?) -> DNumber)
; run on input
(define (run-perceptron prc input)
  (define input-with-bias (cons 1 input))
  (match-define (perceptron weights activation) prc)
  (activation (dot-producto weights input-with-bias)))

#;((listof DNumber) (listof DNumber) -> DNumber)
; dot product. v1 and v2 numbers could be numbers or dnumbers
(define (dot-producto v1 v2)
  (for/sumo ([a v1] [b v2]) (*o a b)))

#;(perceptron? DNumber number? -> perceptron?)
; backprop loss gradient to update weights
(define (backprop prc loss lr)
  (define weights (perceptron-weights prc))
  (define weights^ (for/list ([weight weights])
                     (let ([dw (derivative loss weight)])
                       ; throw away old weights
                       ; subtract dw to minimize loss
                       (->dnumber (- (dnumber-value weight) (* dw lr))))))
  (struct-copy perceptron prc [weights weights^]))

#;(perceptron? (list/c (listof number?) (or/c 0 1)) (listof (or/c 0 1)))
; use prc to classify inputs (rounds to 0 or 1)
(define (test-perceptron prc inputs)
  (define guesses (run-perceptron* prc inputs))
  (for/list ([guess guesses])
    (let ([guess (dnumber-value guess)])
      (list (if (< guess 1/2)
                0
                1)
            guess))))

; tests ;

; TODO test meta gradient descent to learn lr lol

(module+ test
  (test-case "y = x"
    (define data
      (for*/list ([x (in-range 1 10)]
                  [y (in-range 1 10)])
        (list (list x y) (if (> y x) 1 0))))
    (define prc (train-perceptron (make-perceptron 2) data))
    (define outputs (test-perceptron prc (map car data)))
    (define actuals (map second data))
    (for ([elem  (map list actuals outputs)])
      (displayln elem))))
