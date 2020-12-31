#lang racket

(require "../intcode.rkt")

(module+ test
  (require rackunit))

(define (part1 program)
  (define-values (p pc relbase out r) (execute program '()))
  (define screen
    (let loop ([screen (hash)]
               [instructions (reverse out)])
      (if (empty? instructions)
          screen
          (let ([x (car instructions)]
                [y (cadr instructions)]
                [tile-id (caddr instructions)])
            (loop (hash-set screen (list x y) tile-id)
                  (cdddr instructions))))))
  (for/sum ([(k v) (in-hash screen)]
            #:when (= v 2))
    1))

(module+ test
  (check-equal? (part1 (parse-file "input")) 363))

(define (ball-x screen)
  (for/first ([(k v) (in-hash screen)]
              #:when (= v 4))
    (car k)))

(define (paddle-x screen)
  (for/first ([(k v) (in-hash screen)]
              #:when (= v 3))
    (car k)))

(define (block-count screen)
  (for/sum ([(k v) (in-hash screen)]
            #:when (= v 2))
    1))

(define (draw-screen screen instructions)
  (if (empty? instructions)
      screen
      (let ([x (car instructions)]
            [y (cadr instructions)]
            [tile-id (caddr instructions)])
        (draw-screen (hash-set screen (list x y) tile-id)
                     (cdddr instructions)))))

(define (part2 program)
  (vector-set! program 0 2)
  (define-values (p pc relbase out r) (execute program '()))
  (define screen
    (let loop ([p p]
               [pc pc]
               [relbase relbase]
               [out out]
               [screen (hash)])
      (define new-screen (draw-screen screen (reverse out)))
      (define ball-pos (ball-x new-screen))
      (define paddle-pos (paddle-x new-screen))
      (define joystick (cond
                         [(< ball-pos paddle-pos) -1]
                         [(= ball-pos paddle-pos) 0]
                         [(> ball-pos paddle-pos) 1]))
      (define-values (new-p new-pc new-relbase new-out terminated)
        (execute p (list joystick) pc relbase))
      (if (= 0 (block-count new-screen))
          new-screen
          (loop new-p new-pc new-relbase new-out new-screen))))
  (hash-ref screen (list -1 0)))

(module+ test
  (check-equal? (part2 (parse-file "input")) 17159))
