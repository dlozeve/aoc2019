#lang racket

(module+ test
  (require rackunit))

(define (read-input filename)
  (string->grid (file->string filename)))

(define (string->grid str)
  (for/set ([line (string-split str)]
            [i (in-naturals)]
            #:when #t
            [c (in-string line)]
            [j (in-naturals)]
            #:when (eq? c #\#))
    (make-rectangular j i)))

(define (segment->direction x)
  (define a (real-part x))
  (define b (imag-part x))
  (define r (gcd a b))
  (make-rectangular (/ a r) (/ b r)))

(define (detected grid)
  (for/hash ([x (in-set grid)])
    (values x (for/set ([y (in-set grid)]
                        #:unless (= x y))
                (segment->direction (- y x))))))

(define (find-station grid)
  (define visible (detected grid))
  (define max-visible (apply max (map set-count (hash-values visible))))
  (define pos (for/first ([(k v) (in-hash visible)]
                      #:when (= max-visible (set-count v)))
                k))
  (values pos (hash-ref visible pos)))

(define (part1 filename)
  (define grid (read-input filename))
  (define-values (loc dirs) (find-station grid))
  (set-count dirs))

(module+ test
  (check-equal? (part1 "test") 8)
  (check-equal? (part1 "input") 286))

(define (find-target grid station direction)
  (define targets
    (for/list ([x (in-set grid)]
               #:unless (= x station)
               #:when (real? (/ (- x station) direction))
               #:when (<= 0 (/ (- x station) direction)))
      x))
  (car (sort targets
             (λ (x y) (< (magnitude (- x station))
                         (magnitude (- y station)))))))

(define (rotate-left lst)
  (if (empty? lst)
      '()
      (append (cdr lst) (list (car lst)))))

(define (rotate-right lst)
  (reverse (rotate-left (reverse lst))))

(define (find-nth-target grid n)
  (define-values (station directions) (find-station grid))
  (define directions-sorted (rotate-right
                             (sort (set->list directions)
                                   (λ (x y) (< (angle (* -i x)) (angle (* -i y)))))))
  (define-values (final-grid targets)
    (for/fold ([grid grid]
               [targets '()])
              ([i (in-range n)]
               [dir (in-cycle directions-sorted)])
      (define target (find-target grid station dir))
      (values (set-remove grid target)
              (cons target targets))))
  (car targets))

(define (part2 filename)
  (define grid (read-input filename))
  (define target (find-nth-target grid 200))
  (+ (* 100 (real-part target)) (imag-part target)))

(module+ test
  (check-equal? (part2 "input") 504))
