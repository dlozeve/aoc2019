#lang racket/base

(require "../intcode.rkt")

(module+ test
  (require rackunit))

(define (part12 program input)
  (define vm (execute (start-machine program (list input))))
  (car (machine-outputs vm)))

(module+ test
  (test-case "quine"
    (define quine (parse "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"))
    (define vm (execute (start-machine quine '())))
    (check-true (machine-terminated vm))
    (check-equal? (reverse (machine-outputs vm)) (vector->list quine)))
  (test-case "16-digit number"
    (define program (parse "1102,34915192,34915192,7,4,7,99,0"))
    (define vm (execute (start-machine program '())))
    (check-true (machine-terminated vm))
    (check-equal? (string-length (number->string (car (machine-outputs vm)))) 16))
  (test-case "large number"
    (define program (parse "104,1125899906842624,99"))
    (define vm (execute (start-machine program '())))
    (check-true (machine-terminated vm))
    (check-equal? (car (machine-outputs vm)) 1125899906842624))
  (check-equal? (part12 (parse-file "input") 1) 3601950151)
  (check-equal? (part12 (parse-file "input") 2) 64236))
