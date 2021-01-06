#lang racket/base

(require racket/random
         racket/list
         racket/match
         graph
         "../intcode.rkt")

(module+ test
  (require rackunit))

(define (possible-directions grid pos)
  (define unexplored-directions
    (for/list ([step '(-1 -i 1 +i)]
               #:unless (hash-has-key? grid (+ pos step)))
      step))
  (if (empty? unexplored-directions)
      (for/list ([step '(-1 -i 1 +i)]
                 #:unless (eq? 'wall (hash-ref grid (+ pos step) 'floor)))
        step)
      unexplored-directions))

(define (shortest-distances grid source)
  (define g (unweighted-graph/undirected
             (for*/list ([(pos tile) (in-hash grid)]
                         #:unless (eq? 'wall tile)
                         [step '(-1 -i 1 +i)]
                         #:unless (eq? 'wall (hash-ref grid (+ pos step) 'wall)))
               (list pos (+ pos step)))))
  (define-values (distances predecessors) (dijkstra g source))
  distances)

(define (part12 program)
  (define grid (make-hash '((0 'floor))))
  (let loop ([vm (execute (start-machine program '()))]
             [pos 0]
             [i 0])
    (define direction (random-ref (possible-directions grid pos)))
    (define direction-code (match direction
                             [+i 1]
                             [-i 2]
                             [-1 3]
                             [1 4]))
    (define new-vm (execute (struct-copy machine vm [inputs (list direction-code)])))
    (define next-pos (+ pos direction))
    (unless (> i 100000)
      (match (car (machine-outputs new-vm))
        [0 (hash-set! grid next-pos 'wall)
           (loop new-vm pos (add1 i))]
        [1 (hash-set! grid next-pos 'floor)
           (loop new-vm next-pos (add1 i))]
        [2 (hash-set! grid next-pos 'oxygen)
           (loop new-vm next-pos (add1 i))])))
  (define oxygen (for/first ([(pos tile) (in-hash grid)]
                             #:when (eq? 'oxygen tile))
                   pos))
  (define distances (shortest-distances grid oxygen))
  (values (hash-ref distances 0)
          (apply max (hash-values distances))))

(module+ test
  (define-values (p1 p2) (part12 (parse-file "input")))
  (check-equal? p1 296)
  (check-equal? p2 302))
