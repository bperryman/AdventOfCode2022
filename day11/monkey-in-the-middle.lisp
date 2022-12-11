(defstruct monkey
  items
  operation
  divisible-by
  if-test-true
  if-test-false)

(defvar *worry-modifier* (lambda (x) (floor x 3)))

(defun make-worry-level (fn)
  (lambda (item)
    (funcall *worry-modifier* (funcall fn item))))

(defun divisible-by (num n)
  (zerop (mod num n)))

(defun monkey-number (monkeys n)
  (aref monkeys n))

(defun throw-to (target-monkey)
  (lambda (monkeys item)
    (push item (monkey-items (monkey-number monkeys target-monkey)))))

(defun make-sample-monkeys ()
  (make-array 4
              :initial-contents (list
                                 (make-monkey :items '(79 98)
                                              :operation (make-worry-level #'(lambda (old) (* old 19)))
                                              :divisible-by 23
                                              :if-test-true (throw-to 2)
                                              :if-test-false (throw-to 3))
                                 (make-monkey :items '(54 65 75 74)
                                              :operation (make-worry-level #'(lambda (old) (+ old 6)))
                                              :divisible-by 19
                                              :if-test-true (throw-to 2)
                                              :if-test-false (throw-to 0))
                                 (make-monkey :items '(79 60 97)
                                              :operation (make-worry-level #'(lambda (old) (* old old)))
                                              :divisible-by 13
                                              :if-test-true (throw-to 1)
                                              :if-test-false (throw-to 3))
                                 (make-monkey :items '(74)
                                              :operation (make-worry-level #'(lambda (old) (+ old 3)))
                                              :divisible-by 17
                                              :if-test-true (throw-to 0)
                                              :if-test-false (throw-to 1)))))

(defun make-monkeys ()
  (make-array 8
              :initial-contents (list
                                 (make-monkey :items '(96 60 68 91 83 57 85)
                                              :operation (make-worry-level #'(lambda (old) (* old 2)))
                                              :divisible-by 17
                                              :if-test-true (throw-to 2)
                                              :if-test-false (throw-to 5))
                                 (make-monkey :items '(75 78 68 81 73 99)
                                              :operation (make-worry-level #'(lambda (old) (+ old 3)))
                                              :divisible-by 13
                                              :if-test-true (throw-to 7)
                                              :if-test-false (throw-to 4))
                                 (make-monkey :items '(69 86 67 55 96 69 94 85)
                                              :operation (make-worry-level #'(lambda (old) (+ old 6)))
                                              :divisible-by 19
                                              :if-test-true (throw-to 6)
                                              :if-test-false (throw-to 5))
                                 (make-monkey :items '(88 75 74 98 80)
                                              :operation (make-worry-level #'(lambda (old) (+ old 5)))
                                              :divisible-by 7
                                              :if-test-true (throw-to 7)
                                              :if-test-false (throw-to 1))
                                 (make-monkey :items '(82)
                                              :operation (make-worry-level #'(lambda (old) (+ old 8)))
                                              :divisible-by 11
                                              :if-test-true (throw-to 0)
                                              :if-test-false (throw-to 2))
                                 (make-monkey :items '(72 92 92)
                                              :operation (make-worry-level #'(lambda (old) (* old 5)))
                                              :divisible-by 3
                                              :if-test-true (throw-to 6)
                                              :if-test-false (throw-to 3))
                                 (make-monkey :items '(74 61)
                                              :operation (make-worry-level #'(lambda (old) (* old old)))
                                              :divisible-by 2
                                              :if-test-true (throw-to 3)
                                              :if-test-false (throw-to 1))
                                 (make-monkey :items '(76 86 83 55)
                                              :operation (make-worry-level #'(lambda (old) (+ old 4)))
                                              :divisible-by 5
                                              :if-test-true (throw-to 4)
                                              :if-test-false (throw-to 0)))))

(defun show-monkey-items (round monkeys)
  (format t "After round ~a, the monkeys are holding items with these worry levels:~%" round)
  (loop for i = 0 then (1+ i)
    for monkey across monkeys
    do (format t "Monkey ~a: ~{~a ~}~%" i (monkey-items monkey))))

(defun monkey-in-the-middle (monkeys &key (iterations 20))
  (let ((result (make-array (array-dimension monkeys 0) :initial-element 0)))
    (flet ((perform-monkey-step (i monkey)
             (incf (aref result i) (length (monkey-items monkey)))
             (dolist (item (monkey-items monkey))
               (let ((worry-level (funcall (monkey-operation monkey) item)))
                 (if (divisible-by worry-level (monkey-divisible-by monkey))
                     (funcall (monkey-if-test-true monkey) monkeys worry-level)
                     (funcall (monkey-if-test-false monkey) monkeys worry-level))))
             (setf (monkey-items monkey) ())))
      (dotimes (iter iterations result)
        (loop for monkey across monkeys
          for i = 0 then (1+ i)
          do (perform-monkey-step i monkey))))))

(defun result-calc (results)
  (let ((sorted (sort results #'>)))
    (* (aref sorted 0)
       (aref sorted 1))))

(format t "Monkey in the middle - round one answer ~a~%" (result-calc (monkey-in-the-middle (make-monkeys))))


(defun panic-with-monkeys (monkeys iterations)
  (let* ((lcm (apply #'lcm (map 'list 'monkey-divisible-by monkeys)))
         (*worry-modifier* (lambda (x) (rem x lcm))))
    (format t "Monkey in the middle - round two answer ~a~%" 
            (result-calc (monkey-in-the-middle monkeys :iterations iterations)))))