#lang racket

; perceptron using auto diff

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
; Represents a linear perceptron estimator.
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

#;(perceptron? (listof (list/c (listof number?) number?)) -> perceptron?)
; basic batch gradient optimization
(define (train-perceptron prc
                          data
                          #:loss-fn [loss-fn mse-loss]
                          #:learning-rate [lr 0.01]
                          #:epochs [epochs 100])
  (match-define (list (list inputs outputs) ...) data)
  (for/fold ([prc prc])
            ([epoch (in-range epochs)])
    (train-perceptron/epoch prc inputs outputs loss-fn lr)))

#;(perceptron?
   (listof (listof number?))
   (listof number?)
   ((listof DNumber) (listof DNumber) -> DNumber)
   number?
   ->
   perceptron?)
; one epoch (train on batch once) of training
(define (train-perceptron/epoch prc inputs outputs loss-fn lr)
  (define guesses (run-perceptron* prc inputs))
  (define loss (loss-fn guesses outputs))
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

#;(perceptron? (list/c (listof number?) number?) (listof number?))
; use prc to estimate inputs
(define (test-perceptron prc inputs)
  (define guesses (run-perceptron* prc inputs))
  (for/list ([guess guesses])
    (dnumber-value guess)))

#;((listof any/c) (listof any/c) -> number?)
; computes the portion of the guesses that are correct
(define (accuracy-score guesses actuals)
  (define num-correct
    (for/sum ([guess guesses]
              [actual actuals])
      (if (equal? guess actual) 1 0)))
  (/ num-correct (length guesses)))

; tests ;

; TODO test meta gradient descent to learn lr lol
; I tried, it's hard. You'll need to rewrite training around it to support it.
; It's tricky since you throw the weights away. And what loss will you propagate to update the lr?
; It'll have to be some meta-loss on the losses themselves. And you'll have to keep that gradient around.
; Maybe you could use delta loss between epochs as meta-loss and learn lr on the fly, instead of between entire
; trainings. Might make the bookkeeping easier.
; look into this: https://arxiv.org/abs/1502.03492

(module+ test
  (test-case "y = x classification"
    (define data
      (for*/list ([x (in-range 1 10)]
                  [y (in-range 1 10)])
        (list (list x y) (if (> y x) 1 0))))
    (define prc (train-perceptron (make-perceptron 2) data))
    (define outputs (test-perceptron prc (map car data)))
    (define output-classes (map (λ (n) (if (>= n 1/2) 1 0)) outputs))
    (define actuals (map second data))
    (define accuracy (accuracy-score output-classes actuals))
    (check-within accuracy 1 0.1))
  (test-case "y = x regression"
    (define data
      (for/list ([x (in-range 1 100)])
        (list (list x) x)))
    (define prc (train-perceptron (make-perceptron 1 identity) data #:learning-rate 0.0001))
    (define outputs (test-perceptron prc (map car data)))
    (define actuals (map second data))
    (define mse (dnumber-value (mse-loss outputs actuals)))
    (check-within mse 0 0.1)))
