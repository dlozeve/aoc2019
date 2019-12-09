(ql:quickload :alexandria)
(use-package :alexandria)

(defparameter *input-file* #P"input.txt")
(defvar *input*)

(defvar *program*)

(defun opcode (pc)
  (mod (aref *program* pc) 100))

(defun parameter-mode (pc index)
  (mod (truncate (aref *program* pc) (expt 10 (+ 1 index))) 10))

(defun parameter (pc index)
  (ecase (parameter-mode pc index)
    (1 (aref *program* (+ index pc)))
    (0 (aref *program* (aref *program* (+ pc index))))))

(defun (setf parameter) (new-value pc index)
  (ecase (parameter-mode pc index)
    (0 (setf (aref *program* (aref *program* (+ pc index))) new-value))
    (1 (error "Cannot write with a parameter in immediate mode"))))

(defun execute (inputs)
  (defun execute-instruction (pc inputs outputs)
    (ecase (opcode pc)
      (99 (return-from execute-instruction outputs))
      (1 (setf (parameter pc 3) (+ (parameter pc 1) (parameter pc 2)))
	 (execute-instruction (+ pc 4) inputs outputs))
      (2 (setf (parameter pc 3) (* (parameter pc 1) (parameter pc 2)))
	 (execute-instruction (+ pc 4) inputs outputs))
      (3 (setf (parameter pc 1) (car inputs))
	 (execute-instruction (+ pc 2) (cdr inputs) outputs))
      (4 (execute-instruction (+ pc 2) inputs (cons (parameter pc 1) outputs)))
      (5 (if (/= 0 (parameter pc 1))
	     (execute-instruction (parameter pc 2) inputs outputs)
	     (execute-instruction (+ pc 3) inputs outputs)))
      (6 (if (= 0 (parameter pc 1))
	     (execute-instruction (parameter pc 2) inputs outputs)
	     (execute-instruction (+ pc 3) inputs outputs)))
      (7 (if (< (parameter pc 1) (parameter pc 2))
	     (setf (parameter pc 3) 1)
	     (setf (parameter pc 3) 0))
	 (execute-instruction (+ pc 4) inputs outputs))
      (8 (if (= (parameter pc 1) (parameter pc 2))
	     (setf (parameter pc 3) 1)
	     (setf (parameter pc 3) 0))
	 (execute-instruction (+ pc 4) inputs outputs))))
  (execute-instruction 0 inputs nil))

(defun parse-program (program-string)
  (map 'vector #'parse-integer (uiop:split-string program-string :separator ",")))

(defun amplify (phases)
  (let ((amp-input 0))
    (loop for phase in phases
       do (let ((*program* (parse-program *input*)))
	    (setf amp-input (car (execute (list phase amp-input))))))
    amp-input))

(defun max-thrust ()
  (let ((res nil))
    (map-permutations (lambda (x) (push (amplify x) res)) '(0 1 2 3 4))
    (apply #'max res)))

(let ((*input* "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))
  (max-thrust))

(let ((*input* "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"))
  (max-thrust))

(let ((*input* "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))
  (max-thrust))

(defun part1 ()
  (let ((*input* (uiop:read-file-string *input-file*)))
    (max-thrust)))
