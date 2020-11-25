#lang racket

(struct moon
  (x y z vx vy vz)
  #:transparent)

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (for/list ([line (in-lines)])
        (define positions-str
          (cdr (regexp-match #rx"<x=(-?[0-9]+), y=(-?[0-9]+), z=(-?[0-9]+)>" line)))
        (define positions (map string->number positions-str))
        (apply moon (append positions '(0 0 0)))))))

(define (step moons)
  (for/list ([cur moons])
    (define new-vx (moon-vx cur))
    (define new-vy (moon-vy cur))
    (define new-vz (moon-vz cur))
    (define (gravity-pull pos-cur pos-other)
      (cond
        [(> pos-other pos-cur) 1]
        [(< pos-other pos-cur) -1]
        [else 0]))
    (for ([other moons] #:unless (eq? other cur))
      (set! new-vx (+ new-vx (gravity-pull (moon-x cur) (moon-x other))))
      (set! new-vy (+ new-vy (gravity-pull (moon-y cur) (moon-y other))))
      (set! new-vz (+ new-vz (gravity-pull (moon-z cur) (moon-z other)))))
    (moon (+ (moon-x cur) new-vx)
          (+ (moon-y cur) new-vy)
          (+ (moon-z cur) new-vz)
          new-vx
          new-vy
          new-vz)))

(define (total-energy moons)
  (for/sum ([m moons])
    (* (+ (abs (moon-x m))
          (abs (moon-y m))
          (abs (moon-z m)))
       (+ (abs (moon-vx m))
          (abs (moon-vy m))
          (abs (moon-vz m))))))

(define (part1 filename)
  (define moons (read-input filename))
  (total-energy (for/fold ([ms moons]) ([i (in-range 1000)]) (step ms))))

(define (cycle-length moons pos speed)
  (define-values (states final)
    (for/fold ([states (mutable-set)] [ms moons])
              ([_ (in-naturals)]
               #:break (set-member? states (append (map pos ms) (map speed ms))))
      (set-add! states (append (map pos ms) (map speed ms)))
      (values states (step ms))))
  (set-count states))

(define (part2 filename)
  (define moons (read-input filename))
  (define cycle-x (cycle-length moons moon-x moon-vx))
  (define cycle-y (cycle-length moons moon-y moon-vy))
  (define cycle-z (cycle-length moons moon-z moon-vz))
  (lcm cycle-x cycle-y cycle-z))
