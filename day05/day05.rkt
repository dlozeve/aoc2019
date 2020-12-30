#lang racket

(require "../intcode.rkt")

(module+ test
  (require rackunit))

(define (part1 filename)
  (define program (parse-file filename))
  (define-values (p pc relbase out r) (execute program '(1)))
  (car out))

(module+ test
  (check-equal? (part1 "input.txt") 12896948))

(define (part2 filename)
  (define program (parse-file filename))
  (define-values (p pc relbase out r) (execute program '(5)))
  (car out))

(module+ test
  (check-equal? (part2 "input.txt") 7704130))
