#lang racket/base

(provide parse
         parse-file
         (struct-out machine)
         start-machine
         ascii->list
         list->ascii
         execute)

(require racket/list
         racket/string
         racket/file
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Program I/O                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse str)
  (list->vector (map string->number (map string-trim (string-split str ",")))))

(define (parse-file filename)
  (parse (string-trim (file->string filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Instructions                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct instruction
  (name
   parameter-modes
   inparams
   outparams)
  #:transparent)

(define opcodes
  (hash
   99 'terminate
   1 '(add 2 1)
   2 '(mul 2 1)
   3 '(input 0 1)
   4 '(output 1 0)
   5 '(jmpt 2 0)
   6 '(jmpf 2 0)
   7 '(lt 2 1)
   8 '(eq 2 1)
   9 '(rel 1 0)))

(define (parse-instruction program pc relative-base)
  (define opcode (vector-ref program pc))

  (define-values (name nin nout)
    (match (hash-ref opcodes (remainder opcode 100))
      [(list name n m) (values name n m)]
      [name (values name 0 0)]))

  (define parameter-modes (make-hash))
  (for/fold ([n (quotient opcode 100)])
            ([i (in-naturals)]
             #:break (= n 0))
    (hash-set! parameter-modes i (remainder n 10))
    (quotient n 10))

  (define inparams
    (for/list ([i (in-range nin)])
      (match (hash-ref parameter-modes i 0)
        [0 (vector-ref program (vector-ref program (+ pc i 1)))]
        [1 (vector-ref program (+ pc i 1))]
        [2 (vector-ref program (+ relative-base (vector-ref program (+ pc i 1))))])))

  (define outparams
    (for/list ([i (in-range nout)])
      (match (hash-ref parameter-modes (+ nin i) 0)
        [0 (vector-ref program (+ pc nin i 1))]
        [1 (error "output parameter cannot be in immediate mode" name pc)]
        [2 (+ relative-base (vector-ref program (+ pc nin i 1)))])))

  (instruction name parameter-modes inparams outparams))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          Virtual machine interfaces                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct machine
  (program
   inputs
   pc
   relative-base
   terminated
   outputs)
  #:transparent
  #:guard (lambda (program inputs pc relative-base terminated outputs name)
            (when (not (<= 0 pc (vector-length program)))
              (error "invalid program counter" pc))
            (values program
                    (preprocess-inputs inputs)
                    pc
                    relative-base
                    terminated outputs)))

(define (start-machine program inputs)
  (machine program inputs 0 0 #f '()))

(define (ascii->list str)
  (map char->integer (string->list str)))

(define (list->ascii lst)
  (list->string (map integer->char lst)))

(define (preprocess-inputs inputs)
  (cond
    [(list? inputs) inputs]
    [(string? inputs) (ascii->list inputs)]
    [else (error "unsupported input type: " inputs)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          Virtual machine execution                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (execute vm)
  (define p (make-vector 10000 0))
  (vector-copy! p 0 (machine-program vm))

  (let loop ([pc (machine-pc vm)]
             [relative-base (machine-relative-base vm)]
             [inputs (machine-inputs vm)]
             [outputs (machine-outputs vm)])
    (define instr (parse-instruction p pc relative-base))
    (define inparams (instruction-inparams instr))
    (define outparams (instruction-outparams instr))
    (define next-pc (+ pc (length inparams) (length outparams) 1))
    
    (match (instruction-name instr)
      ['terminate (machine p inputs pc relative-base #t outputs)]
      ['add (vector-set! p (car outparams) (apply + inparams))
            (loop next-pc relative-base inputs outputs)]
      ['mul (vector-set! p (car outparams) (apply * inparams))
            (loop next-pc relative-base inputs outputs)]
      ['input (if (empty? inputs)
                  (machine p inputs pc relative-base #f outputs)
                  (begin
                    (vector-set! p (car outparams) (car inputs))
                    (loop next-pc relative-base (cdr inputs) outputs)))]
      ['output (loop next-pc relative-base inputs (cons (car inparams) outputs))]
      ['jmpt (if (not (= 0 (car inparams)))
                 (loop (cadr inparams) relative-base inputs outputs)
                 (loop next-pc relative-base inputs outputs))]
      ['jmpf (if (= 0 (car inparams))
                 (loop (cadr inparams) relative-base inputs outputs)
                 (loop next-pc relative-base inputs outputs))]
      ['lt (vector-set! p (car outparams)
                        (if (< (car inparams) (cadr inparams)) 1 0))
           (loop next-pc relative-base inputs outputs)]
      ['eq (vector-set! p (car outparams)
                        (if (= (car inparams) (cadr inparams)) 1 0))
           (loop next-pc relative-base inputs outputs)]
      ['rel (loop next-pc (+ relative-base (car inparams)) inputs outputs)])))
