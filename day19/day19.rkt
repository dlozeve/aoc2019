#lang racket/base

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

(define (part2 program)
  (let loop ([x 0] [y 99])
    (cond
      [(not (pulled? program x y)) (loop (add1 x) y)]
      [(pulled? program (+ x 99) (- y 99)) (+ (* 10000 x) (- y 99))]
      [else (loop x (add1 y))])))

(module+ test
  (check-equal? (part2 (parse-file "input")) 6950903))
