#lang racket/base

(require "../intcode.rkt"
         racket/vector)

(module+ test
  (require rackunit))

(define (part1 filename)
  (define program (parse-file filename))
  (vector-set! program 1 12)
  (vector-set! program 2 2)
  (define vm (execute (start-machine program '())))
  (vector-ref (machine-program vm) 0))

(module+ test
  (check-equal? (part1 "input.txt") 6627023))

(define (try-inputs program noun verb)
  (define my-program (vector-copy program))
  (vector-set! my-program 1 noun)
  (vector-set! my-program 2 verb)
  (define vm (execute (start-machine my-program '())))
  (vector-ref (machine-program vm) 0))

(define (part2 filename)
  (define program (parse-file filename))
  (for*/first ([noun (in-range 100)]
               [verb (in-range 100)]
               #:when (eq? 19690720 (try-inputs program noun verb)))
    (+ (* 100 noun) verb)))

(module+ test
  (check-equal? (part2 "input.txt") 4019))
