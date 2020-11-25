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

(define (ore-per-fuel rules amount)
  (define needed (make-hash `((FUEL . ,amount))))
  (define available (make-hash))
  (for ([i (in-naturals)]
        #:break (= (apply + (hash-values needed)) (hash-ref needed 'ORE 0)))
    (generate! rules needed available))
  (hash-ref needed 'ORE))

(define (part1 filename)
  (define rules (read-rules filename))
  (ore-per-fuel rules 1))

(module+ test
  (check-eq? 31 (part1 "test1"))
  (check-eq? 165 (part1 "test2"))
  (check-eq? 13312 (part1 "test3"))
  (check-eq? 180697 (part1 "test4"))
  (check-eq? 2210736 (part1 "test5")))

(module+ main
  (display "Part 1: ")
  (displayln (part1 "input")))

(define (part2 filename)
  (define rules (read-rules filename))
  (define max-fuel 1000000000000)
  (define min-fuel 1)
  (bisect rules 1000000000000 min-fuel max-fuel))

(define (bisect rules target-ore min-fuel max-fuel)
  (define mid-fuel (floor (+ min-fuel (/ (- max-fuel min-fuel) 2))))
  (define ore (ore-per-fuel rules mid-fuel))
  (cond
    [(= 1 (- max-fuel min-fuel)) min-fuel]
    [(< ore target-ore) (bisect rules target-ore mid-fuel max-fuel)]
    [(< target-ore ore) (bisect rules target-ore min-fuel mid-fuel)]))

(module+ test
  (check-eq? 82892753 (part2 "test3"))
  (check-eq? 5586022 (part2 "test4"))
  (check-eq? 460664 (part2 "test5")))

(module+ main
  (display "Part 2: ")
  (displayln (part2 "input")))
