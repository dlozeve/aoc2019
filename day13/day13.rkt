#lang racket/base

(require "../intcode.rkt"
         racket/list)

(module+ test
  (require rackunit))

(define (part1 program)
  (define vm (execute (start-machine program '())))
  (define screen
    (let loop ([screen (hash)]
               [instructions (reverse (machine-outputs vm))])
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
  (define vm (execute (start-machine program '())))
  (define screen
    (let loop ([vm vm]
               [screen (hash)])
      (define new-screen (draw-screen screen (reverse (machine-outputs vm))))
      (define ball-pos (ball-x new-screen))
      (define paddle-pos (paddle-x new-screen))
      (define joystick (cond
                         [(< ball-pos paddle-pos) -1]
                         [(= ball-pos paddle-pos) 0]
                         [(> ball-pos paddle-pos) 1]))
      (define new-vm
        (execute (machine (machine-program vm) (list joystick)
                          (machine-pc vm) (machine-relative-base vm) #f '())))
      (if (= 0 (block-count new-screen))
          new-screen
          (loop new-vm new-screen))))
  (hash-ref screen (list -1 0)))

(module+ test
  (check-equal? (part2 (parse-file "input")) 17159))
