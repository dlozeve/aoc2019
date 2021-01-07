#lang racket

(module+ test
  (require rackunit))

(define (read-input filename)
  (for/set ([line (file->lines filename)]
            [i (in-naturals)]
            #:when #t
            [c (in-string line)]
            [j (in-naturals)]
            #:when (eq? c #\#))
    (make-rectangular j i)))

(define (grid-size grid)
  (define indices (flatten (map (λ (x) (list (real-part x) (imag-part x))) (set->list grid))))
  (add1 (apply max indices)))

(define (display-grid grid)
  (define size (grid-size grid))
  (for ([j (in-range size)])
    (for ([i (in-range size)])
      (if (set-member? grid (make-rectangular i j))
          (printf "#")
          (printf ".")))
    (printf "~n")))

(define (count-neighbours grid pos)
  (for/sum ([step '(-i -1 +i 1)]
            #:when (set-member? grid (+ pos step)))
    1))

(define (lives? grid pos)
  (define n (count-neighbours grid pos))
  (define alive (set-member? grid pos))
  (cond
    [(and alive (not (= 1 n))) #f]
    [(and (not alive) (or (= 1 n) (= 2 n))) #t]
    [else alive]))

(define (step grid)
  (define size (grid-size grid))
  (for*/set ([i (in-range size)]
             [j (in-range size)]
             #:when (lives? grid (make-rectangular i j)))
    (make-rectangular i j)))

(define (biodiversity-rating grid)
  (define size (grid-size grid))
  (for*/sum ([i (in-range size)]
             [j (in-range size)]
             #:when (set-member? grid (make-rectangular i j)))
    (expt 2 (+ i (* j size)))))

(define (part1 filename)
  (define grid (read-input filename))
  (define final
    (car (for/fold ([grids (list grid)])
                   ([i (in-naturals)]
                    #:break (member (car grids) (cdr grids)))
           (cons (step (car grids)) grids))))
  (biodiversity-rating final))

(module+ test
  (check-equal? (part1 "test") 2129920)
  (check-equal? (part1 "input") 32573535))
