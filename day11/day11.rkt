#lang racket/base

(require "../intcode.rkt"
         racket/set)

(module+ test
  (require rackunit))

(define (part12 program start-panel [show-grid #f])
  (define vm (execute (start-machine program '())))
  (define grid (make-hash))
  (hash-set! grid 0 start-panel)
  (define painted (mutable-set))
  (let loop ([vm vm]
             [direction 0+1i]
             [position 0])
    (define new-vm
      (execute (machine (machine-program vm)
                        (list (hash-ref grid position 0))
                        (machine-pc vm)
                        (machine-relative-base vm)
                        #f
                        '())))
    (unless (machine-terminated new-vm)
      (hash-set! grid position (cadr (machine-outputs new-vm)))
      (set-add! painted position)
      (define new-direction (if (= 0 (car (machine-outputs new-vm)))
                                (* direction 0+1i)
                                (* direction 0-1i)))
      (define new-position (+ position new-direction))
      (loop new-vm new-direction new-position)))
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
