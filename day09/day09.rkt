#lang racket

(require "../intcode.rkt")

(module+ test
  (require rackunit))

(define (part12 program input)
  (define-values (p pc relbase out r) (execute program (list input)))
  (car out))

(module+ test
  (test-case "quine"
    (define quine (parse "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"))
    (define-values (p pc relbase out terminated) (execute quine '()))
    (check-true terminated)
    (check-equal? (reverse out) (vector->list quine)))
  (test-case "16-digit number"
    (define program (parse "1102,34915192,34915192,7,4,7,99,0"))
    (define-values (p pc relbase out terminated) (execute program '()))
    (check-true terminated)
    (check-equal? (string-length (number->string (car out))) 16))
  (test-case "large number"
    (define program (parse "104,1125899906842624,99"))
    (define-values (p pc relbase out terminated) (execute program '()))
    (check-true terminated)
    (check-equal? (car out) 1125899906842624))
  (check-equal? (part12 (parse-file "input") 1) 3601950151)
  (check-equal? (part12 (parse-file "input") 2) 64236))
