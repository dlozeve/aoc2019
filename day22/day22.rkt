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
  (displayln "Part 1:")
  (define instructions (read-instructions "input"))
  (define deck (range 10007))
  (time (index-of (shuffle deck instructions) 2019)))

(define (deal-card deck-size card)
  (- (sub1 deck-size) card))

(define (cut-card deck-size n card)
  (modulo (- card n) deck-size))

(define (increment-card deck-size n card)
  (modulo (* n card) deck-size))

(define (execute-card deck-size instruction card)
  (match instruction
    ['deal (deal-card deck-size card)]
    [(list 'cut n) (cut-card deck-size n card)]
    [(list 'increment n) (increment-card deck-size n card)]))

(define (shuffle-card deck-size instructions card)
  (for/fold ([c card]) ([i instructions])
    (execute-card deck-size i c)))

(module+ main
  (displayln "Part 1 (better):")
  (time (shuffle-card 10007 instructions 2019)))

(define (deal-card-inv deck-size card)
  (- (sub1 deck-size) card))

(define (cut-card-inv deck-size n card)
  (modulo (+ card n) deck-size))

(define (increment-card-inv deck-size n card)
  (remainder (* card (modular-inverse n deck-size)) deck-size))

(define (execute-card-inv deck-size instruction card)
  (match instruction
    ['deal (deal-card-inv deck-size card)]
    [(list 'cut n) (cut-card-inv deck-size n card)]
    [(list 'increment n) (increment-card-inv deck-size n card)]))

(define (shuffle-card-inv deck-size reversed-instructions card)
  (for/fold ([c card]) ([i reversed-instructions])
    (execute-card-inv deck-size i c)))

(module+ main
  (displayln "Part 2:")
  (define deck-size 119315717514047)
  (define iterations 101741582076661)

  ;; Operations are linear, we find the coefficients and apply them directly
  (define x 2020)
  (define y (shuffle-card-inv deck-size (reverse instructions) x))
  (define z (shuffle-card-inv deck-size (reverse instructions) y))

  (define a (modulo (* (- z y) (modular-inverse (- y x) deck-size)) deck-size))
  (define b (modulo (- y (* a x)) deck-size))

  (modulo (+ (* (modular-expt a iterations deck-size)
                x)
             (* (sub1 (modular-expt a iterations deck-size))
                (modular-inverse (sub1 a) deck-size)
                b))
          deck-size))
