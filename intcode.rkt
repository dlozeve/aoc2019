#lang racket

(provide parse
         parse-file
         execute)

(define (parse str)
  (list->vector (map string->number (map string-trim (string-split str ",")))))

(define (parse-file filename)
  (parse (string-trim (file->string filename))))

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
   8 '(eq 2 1)))

(define (parse-instruction program pc)
  (define opcode (vector-ref program pc))

  (define-values (name nin nout)
    (match (hash-ref opcodes (remainder opcode 100))
      [(list name n m) (values name n m)]
      [name (values name 0 0)]))

  (define parameter-modes (make-hash))
  (for/fold ([n (quotient opcode 100)])
            ([i (in-naturals)]
             #:break (= n 0))
    (when (< 0 (remainder n 10))
      (hash-set! parameter-modes i (remainder n 10)))
    (quotient n 10))

  (define inparams
    (for/list ([i (in-range nin)])
      (match (hash-ref parameter-modes i 0)
        [0 (vector-ref program (vector-ref program (+ pc i 1)))]
        [1 (vector-ref program (+ pc i 1))])))

  (define outparams
    (for/list ([i (in-range nout)])
      (vector-ref program (+ pc nin i 1))))

  (instruction name parameter-modes inparams outparams))

(define (execute program inputs [pc 0])
  (define p (vector-copy program))

  (let loop ([pc pc]
             [inputs inputs]
             [outputs '()])
    (define instr (parse-instruction p pc))
    (define inparams (instruction-inparams instr))
    (define outparams (instruction-outparams instr))
    (define next-pc (+ pc (length inparams) (length outparams) 1))
    
    (match (instruction-name instr)
      ['terminate (values p pc outputs #t)]
      ['add (vector-set! p (car outparams) (apply + inparams))
            (loop next-pc inputs outputs)]
      ['mul (vector-set! p (car outparams) (apply * inparams))
            (loop next-pc inputs outputs)]
      ['input (if (empty? inputs)
                  (values p pc outputs #f)
                  (begin
                    (vector-set! p (car outparams) (car inputs))
                    (loop next-pc (cdr inputs) outputs)))]
      ['output (loop next-pc inputs (cons (car inparams) outputs))]
      ['jmpt (if (not (= 0 (car inparams)))
                 (loop (cadr inparams) inputs outputs)
                 (loop next-pc inputs outputs))]
      ['jmpf (if (= 0 (car inparams))
                 (loop (cadr inparams) inputs outputs)
                 (loop next-pc inputs outputs))]
      ['lt (vector-set! p (car outparams)
                        (if (< (car inparams) (cadr inparams)) 1 0))
           (loop next-pc inputs outputs)]
      ['eq (vector-set! p (car outparams)
                        (if (= (car inparams) (cadr inparams)) 1 0))
           (loop next-pc inputs outputs)])))
