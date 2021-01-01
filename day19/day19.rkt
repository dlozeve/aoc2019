#lang racket

(require "../intcode.rkt")

(module+ test
  (require rackunit))

(define (pulled? program x y)
  (define vm (execute (start-machine program (list x y))))
  (= 1 (car (machine-outputs vm))))

(define (part1 program)
  (length (for*/list ([x (in-range 50)]
                      [y (in-range 50)]
                      #:when (pulled? program x y))
            (list x y))))

(module+ test
  (check-equal? (part1 (parse-file "input")) 229))
