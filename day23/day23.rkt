#lang racket/base

(require "../intcode.rkt"
         racket/match
         racket/list)

(module+ test
  (require rackunit))

(define (get-packets vms)
  (let loop ([packets (reverse (append* (map machine-outputs vms)))]
             [h (hash)])
    (match packets
      ['() h]
      [(list* addr x y r) (loop r (hash-update h addr (λ (lst) (append (list x y) lst)) '()))])))

(define (part1 program)
  (define vms (for/list ([i (in-range 50)])
                (execute (start-machine program (list i)))))
  (let loop ([vms vms])
    (define packets (get-packets vms))
    (if (hash-has-key? packets 255)
        (cadr (hash-ref packets 255))
        (loop
         (for/list ([i (in-range 50)]
                    [vm (in-list vms)])
           (execute (struct-copy machine vm [inputs (hash-ref packets i '(-1))])))))))

(module+ test
  (check-equal? (part1 (parse-file "input")) 23266))
