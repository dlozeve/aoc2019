(defparameter *input-file* #P"input.txt")
(defparameter *input* (uiop:read-file-lines *input-file*))

(defun part1 ()
  (loop for line in *input*
     sum (- (floor (/ (parse-integer line) 3)) 2)))

(defun total-fuel-requirements (total-fuel fuel)
  (let ((new-fuel (max 0 (- (floor (/ fuel 3)) 2))))
    (if (= new-fuel 0)
	(+ total-fuel fuel)
	(total-fuel-requirements (+ total-fuel fuel) new-fuel))))

(defun part2 ()
  (loop for line in *input*
     sum (total-fuel-requirements 0 (- (floor (/ (parse-integer line) 3)) 2))))
