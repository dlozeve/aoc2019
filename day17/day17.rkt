#lang racket

(require "../intcode.rkt")

(module+ test
  (require rackunit))

(define (get-grid program)
  (define vm (execute (start-machine program '())))
  (define map-str (list->string (map integer->char (reverse (machine-outputs vm)))))
  (define-values (grid pos)
    (for/fold ([grid (set)]
               [pos 0])
              ([c (in-list (reverse (machine-outputs vm)))])
      (match (integer->char c)
        [#\newline (values grid (make-rectangular 0 (add1 (imag-part pos))))]
        [#\# (values (set-add grid pos) (add1 pos))]
        [x (values grid (add1 pos))])))
  (values map-str grid))

(define (part1 program)
  (define-values (map-str grid) (get-grid program))
  (define intersections
    (for/list ([x (in-set grid)]
               #:when (for/and ([step '(-i -1 +i 1)])
                        (set-member? grid (+ x step))))
      x))
  (for/sum ([int (in-list intersections)])
    (* (real-part int) (imag-part int))))

(module+ test
  (check-equal? (part1 (parse-file "input")) 5972))
