#lang racket

(require racket/string
         math/number-theory)

(module+ test
  (require rackunit))

(module+ main
  (displayln "Day 22"))

(define (read-instructions filename)
  (with-input-from-file filename
    (lambda ()
      (for/list ([line (in-lines)])
        (cond
          [(string-prefix? line "deal into new stack") 'deal]
          [(string-prefix? line "cut") `(cut ,(string->number (last (string-split line " "))))]
          [(string-prefix? line "deal with increment") `(increment ,(string->number (last (string-split line " "))))]
          [else 'other])))))

(define (deal deck)
  (reverse deck))

(define (cut deck n)
  (define split-fn (if (> n 0) split-at split-at-right))
  (define-values (a b) (split-fn deck (abs n)))
  (append b a))

(define (increment deck n)
  (for/list ([i (in-range (length deck))])
    (list-ref deck
              (remainder (* i (modular-inverse n (length deck))) (length deck)))))

(define (execute instruction deck)
  (match instruction
    ['deal (deal deck)]
    [(list 'cut n) (cut deck n)]
    [(list 'increment n) (increment deck n)]))

(define (shuffle deck instructions)
  (for/fold ([d deck]) ([i instructions])
    (execute i d)))

(module+ test
  (check-equal? (shuffle (range 10) (read-instructions "test1")) '(0 3 6 9 2 5 8 1 4 7))
  (check-equal? (shuffle (range 10) (read-instructions "test2")) '(3 0 7 4 1 8 5 2 9 6))
  (check-equal? (shuffle (range 10) (read-instructions "test3")) '(6 3 0 7 4 1 8 5 2 9))
  (check-equal? (shuffle (range 10) (read-instructions "test4")) '(9 2 5 8 1 4 7 0 3 6)))

(module+ main
  (display "Part 1: ")
  (define instructions (read-instructions "input"))
  (define deck (range 10007))
  (index-of (shuffle deck instructions) 2019))
