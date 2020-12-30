#lang racket

(require "../intcode.rkt")

(module+ test
  (require rackunit))

(define (part12 program start-panel [show-grid #f])
  (define-values (p pc relbase out r) (execute program '()))
  (define grid (make-hash))
  (hash-set! grid 0 start-panel)
  (define painted (mutable-set))
  (let loop ([p p]
             [pc pc]
             [relbase relbase]
             [direction 0+1i]
             [position 0])
    (define-values (new-p new-pc new-relbase out terminated)
      (execute p (list (hash-ref grid position 0)) pc relbase))
    (unless terminated
      (hash-set! grid position (cadr out))
      (set-add! painted position)
      (define new-direction (if (= 0 (car out))
                                (* direction 0+1i)
                                (* direction 0-1i)))
      (define new-position (+ position new-direction))
      (loop new-p new-pc new-relbase new-direction new-position)))
  (when show-grid
    (display-grid grid))
  (set-count painted))

(define (display-grid grid)
  (define min-x (apply min (map real-part (hash-keys grid))))
  (define max-x (apply max (map real-part (hash-keys grid))))
  (define min-y (apply min (map imag-part (hash-keys grid))))
  (define max-y (apply max (map imag-part (hash-keys grid))))
  (for* ([j (in-range max-y (sub1 min-y) -1)]
         [i (in-range min-x max-x)])
    (if (= 1 (hash-ref grid (make-rectangular i j) 0))
        (printf "#")
        (printf " "))
    (when (= i (sub1 max-x))
      (printf "\n"))))

(module+ test
  (check-equal? (part12 (parse-file "input") 0) 2511))

;; > (part12 (parse-file "input") 1 #t)
;; #  #   ## #  #   ## #  #  ##  ###  #  #                                              
;; #  #    # # #     # # #  #  # #  # #  #                                              
;; ####    # ##      # ##   #    #  # ####                                              
;; #  #    # # #     # # #  # ## ###  #  #                                              
;; #  # #  # # #  #  # # #  #  # #    #  #                                              
;; #  #  ##  #  #  ##  #  #  ### #    #  #   
