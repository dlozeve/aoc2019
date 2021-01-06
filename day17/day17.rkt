#lang racket/base

(require "../intcode.rkt"
         racket/match
         racket/set
         racket/list
         racket/string)

(module+ test
  (require rackunit))

(define (get-grid program)
  (define vm (execute (start-machine program '())))
  (define map-str (list->ascii (reverse (machine-outputs vm))))
  (define-values (grid robot-pos robot-dir pos)
    (for/fold ([grid (set)]
               [robot-pos #f]
               [robot-dir #f]
               [pos 0])
              ([c (in-list (reverse (machine-outputs vm)))])
      (match (integer->char c)
        [#\newline (values grid robot-pos robot-dir (make-rectangular 0 (add1 (imag-part pos))))]
        [#\# (values (set-add grid pos) robot-pos robot-dir (add1 pos))]
        [#\^ (values grid pos -i (add1 pos))]
        [#\v (values grid pos +i (add1 pos))]
        [#\< (values grid pos -1 (add1 pos))]
        [#\> (values grid pos 1 (add1 pos))]
        [x (values grid robot-pos robot-dir (add1 pos))])))
  (values map-str grid robot-pos robot-dir))

(define (part1 program)
  (define-values (map-str grid robot-pos robot-dir) (get-grid program))
  (define intersections
    (for/list ([x (in-set grid)]
               #:when (for/and ([step '(-i -1 +i 1)])
                        (set-member? grid (+ x step))))
      x))
  (for/sum ([int (in-list intersections)])
    (* (real-part int) (imag-part int))))

(module+ test
  (check-equal? (part1 (parse-file "input")) 5972))

(define (find-direction grid robot-pos robot-dir)
  (for/first ([step '(-i +i -1 1)]
              #:unless (= step (- robot-dir))
              #:when (set-member? grid (+ robot-pos step)))
    step))

(define (get-path grid robot-pos robot-dir)
  (define path
    (let loop ([pos robot-pos]
               [dir robot-dir]
               [len 0]
               [path '()])
      (if (set-member? grid (+ pos dir))
          (loop (+ pos dir) dir (add1 len) path)
          (let ([new-dir (find-direction grid pos dir)]
                [new-path (cons len path)])
            (if new-dir
                (loop pos new-dir 0 (cons (match (/ new-dir dir) [+i 'R] [-i 'L]) new-path))
                (cdr (reverse new-path)))))))
  path)

(define (remove-substring lst sublst)
  (cond
    [(empty? lst) '()]
    [(list-prefix? sublst lst) (remove-substring (drop lst (length sublst)) sublst)]
    [else (cons (car lst) (remove-substring (cdr lst) sublst))]))

(define (compress path)
  (define (prefixes lst)
    (for/list ([size (in-range 2 10 2)])
      (take lst size)))
  (define possibilities
    (for*/list ([a (in-list (prefixes path))]
                [b (in-list (prefixes (remove-substring path a)))]
                [c (in-list (prefixes (remove-substring (remove-substring path a) b)))]
                #:when (empty? (remove-substring (remove-substring (remove-substring path a) b) c)))
      (list a b c)))
  (for/last ([poss (in-list possibilities)])
    (match-define (list a b c) poss)
    (define result (let loop ([lst path])
                     (cond
                       [(empty? lst) '()]
                       [(list-prefix? c lst) (cons 'C (loop (drop lst (length c))))]
                       [(list-prefix? b lst) (cons 'B (loop (drop lst (length b))))]
                       [(list-prefix? a lst) (cons 'A (loop (drop lst (length a))))]
                       [else '(#f)])))
    #:final (last result)
    (list result a b c)))

(define (lst->str path)
  (string-join (map (λ (v) (format "~a" v)) path) ","))

(define (part2 program)
  (define-values (map-str grid robot-pos robot-dir) (get-grid program))
  (define path (get-path grid robot-pos robot-dir))
  (define inputs (string-join (append (map lst->str (compress path)) '("n\n")) "\n"))
  (vector-set! program 0 2)
  (define vm (execute (start-machine program inputs)))
  (car (machine-outputs vm)))

(module+ test
  (check-equal? (part2 (parse-file "input")) 933214))
