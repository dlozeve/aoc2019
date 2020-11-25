#lang racket

(module+ test
  (require rackunit))

(module+ main
  (displayln "Day 14"))

(define (read-rules filename)
  (with-input-from-file filename
    (lambda ()
      (define h (make-hash))
      (for ([line (in-lines)])
        (define rule
          (for/list ([elt (regexp-match* #rx"([0-9]+) ([A-Z]+)" line #:match-select cdr)])
            (list (string->number (car elt)) (string->symbol (cadr elt)))))
        (define output (last rule))
        (hash-set! h
                   (cadr output)
                   (cons (car output)
                         (make-hash (for/list ([elt (drop-right rule 1)])
                                      (cons (cadr elt) (car elt)))))))
      h)))

(define (generate! rules needed available)
  (for ([(target target-amount) (in-hash needed)]
        #:unless (eq? target 'ORE))
    (define inputs (cdr (hash-ref rules target)))
    (define output-amount (car (hash-ref rules target)))
    (define new-target-amount (- target-amount (hash-ref available target 0)))
    (define multiple (ceiling (/ new-target-amount output-amount)))
    (hash-set! available target (- (* multiple output-amount) new-target-amount))
    (hash-set! needed target 0)
    (for ([(input input-amount) (in-hash inputs)])
      (hash-update! needed input (lambda (v) (+ v (* multiple input-amount))) 0))))

(define (part1 filename)
  (define rules (read-rules filename))
  (define needed (make-hash '((FUEL . 1))))
  (define available (make-hash))
  (for ([i (in-naturals)]
        #:break (= (apply + (hash-values needed)) (hash-ref needed 'ORE 0)))
    (generate! rules needed available))
  (hash-ref needed 'ORE))

(module+ test
  (check-eq? 31 (part1 "test1"))
  (check-eq? 165 (part1 "test2"))
  (check-eq? 13312 (part1 "test3"))
  (check-eq? 180697 (part1 "test4"))
  (check-eq? 2210736 (part1 "test5")))

(module+ main
  (display "Part 1: ")
  (displayln (part1 "input")))
