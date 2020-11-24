#lang racket

(require racket/string
         graph)

(define (read-graph filename)
  (with-input-from-file filename
    (lambda ()
     (define g (unweighted-graph/directed '()))
     (for ([line (in-lines)])
       (define bodies (string-split line ")"))
       (add-edge! g (car bodies) (cadr bodies)))
     g)))

(define (part1 filename)
  (define g (read-graph filename))
  (define-values (dists _) (bfs g "COM"))
  (for/sum ([i (in-hash-values dists)]) i))

(define (part2 filename)
  (define g (read-graph filename))
  (define-values (dists _) (bfs g "YOU"))
  (- (hash-ref dists "SAN") 2))
