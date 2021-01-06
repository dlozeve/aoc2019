#lang racket/base

(require "../intcode.rkt")

(module+ test
  (require rackunit))

(define (part1 program)
  (define inputs #<<EOF
OR A J
AND B J
AND C J
NOT J J
AND D J
WALK

EOF
    )
  (define vm (execute (start-machine program inputs)))
  (car (machine-outputs vm)))

(module+ test
  (check-equal? (part1 (parse-file "input")) 19359533))

(define (part2 program)
  (define inputs #<<EOF
OR A J
AND B J
AND C J
NOT J J
AND D J
OR E T
OR H T
AND T J
RUN

EOF
    )
  (define vm (execute (start-machine program inputs)))
  (car (machine-outputs vm)))

(module+ test
  (check-equal? (part2 (parse-file "input")) 1140310551))
