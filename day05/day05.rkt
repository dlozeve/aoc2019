#lang racket/base

(require "../intcode.rkt")

(module+ test
  (require rackunit))

(define (part1 filename)
  (define program (parse-file filename))
  (define vm (execute (start-machine program '(1))))
  (car (machine-outputs vm)))

(module+ test
  (check-equal? (part1 "input.txt") 12896948))

(define (part2 filename)
  (define program (parse-file filename))
  (define vm (execute (start-machine program '(5))))
  (car (machine-outputs vm)))

(module+ test
  (check-equal? (part2 "input.txt") 7704130))
