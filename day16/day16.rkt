#lang racket

(require racket/sequence)

(module+ test
  (require rackunit))

(module+ main
  (displayln "Day 16"))

(define (pattern n i)
  (define index (remainder (add1 i) (* 4 n)))
  (cond
    [(< index n) 0]
    [(< index (* 2 n)) 1]
    [(< index (* 3 n)) 0]
    [(< index (* 4 n)) -1]))

(define (last-digit n)
  (abs (remainder n 10)))

(define (phase signal)
  (for/vector #:length (vector-length signal)
      ([i (in-range (vector-length signal))])
    (last-digit (for/sum ([d (in-vector signal i)]
                          [j (in-naturals i)])
                  (* d (pattern (add1 i) j))))))

(define (number->digits n)
  (define (aux n)
    (define-values (q r) (quotient/remainder n 10))
    (if (> q 0)
        (cons r (aux q))
        (list r)))
  (list->vector (reverse (aux n))))

(define (digits->number vec)
  (for/sum ([d (in-vector vec (sub1 (vector-length vec)) -1 -1)]
            [i (in-naturals)])
    (* d (expt 10 i))))

(define (apply-n-times f n x)
  (for/fold ([val x]) ([i (in-range n)])
    (f val)))

(define (part1 n)
  (digits->number (vector-take (apply-n-times phase 100 n) 8)))

(module+ test
  (check-eq? (part1 (number->digits 80871224585914546619083218645595)) 24176176)
  (check-eq? (part1 (number->digits 19617804207202209144916044189917)) 73745418)
  (check-eq? (part1 (number->digits 69317163492948606335995924319873)) 52432133))

(module+ main
  (display "Part 1: ")
  (define input (number->digits
                 (string->number
                  (string-trim
                   (port->string (open-input-file "input") #:close? #t)))))
  (displayln (part1 input)))
