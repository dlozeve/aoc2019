(ql:quickload "str")

(defparameter *input-file* #P"input.txt")
(defparameter *input* (uiop:read-file-string *input-file*))

(defun opcode (instruction-code)
  (mod instruction-code 100))

(defun parameter-mode (instruction-code parameter-index)
  (nth (- parameter-index 1)
       (reverse (map 'list #'digit-char-p
		     (prin1-to-string (floor (/ instruction-code 100)))))))

(defun get-parameter (program instruction-pointer parameter-index)
  (let ((mode (parameter-mode (aref program instruction-pointer) parameter-index)))
    ;(format t "get-parameter ~a ~a ~a ~a~%" program instruction-pointer parameter-index mode)
    (if (eql 1 mode)
	(aref program (+ parameter-index instruction-pointer))
	(aref program (aref program (+ parameter-index instruction-pointer))))))


(defun execute (program inputs)
  (defun execute-instruction (i inputs outputs)
    ;(format t "~a ~a ~a ~a~%" program i inputs outputs)
    (let ((opcode (opcode (aref program i))))
      (cond
	((= opcode 99)
	 (return-from execute-instruction (values program outputs)))
	((= opcode 1)
	 (setf (aref program (aref program (+ 3 i)))
	       (+ (get-parameter program i 1)
		  (get-parameter program i 2)))
	 (execute-instruction (+ i 4) inputs outputs))
	((= opcode 2)
	 (setf (aref program (aref program (+ 3 i)))
	       (* (get-parameter program i 1)
		  (get-parameter program i 2)))
	 (execute-instruction (+ i 4) inputs outputs))
	((= opcode 3)
	 (setf (aref program (aref program (+ 1 i)))
	       (car inputs))
	 (execute-instruction (+ i 2) (cdr inputs) outputs))
	((= opcode 4)
	 (execute-instruction (+ i 2) inputs (cons (get-parameter program i 1) outputs))))))
  (execute-instruction 0 inputs nil))

(defun part1 (program-string)
    (let ((program (map 'vector #'parse-integer (str:split "," program-string))))
      (execute program '(1))))
