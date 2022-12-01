(defvar *calorie-data*
  (with-open-file (ifp "elf-calories.txt")
    (let ((result ()))
      (do ((l (read-line ifp nil nil) (read-line ifp nil nil)))
          ((null l) (reverse result))
        (push l result)))))

(load "ql:setup")                       ; Load in quicklisp

(ql:quickload "split-sequence")

(defvar *groups* (split-sequence:split-sequence "" *calorie-data* :test #'string-equal))

(defun sum-string-list (lst)
  (loop for num in lst
        sum (parse-integer num)))

(defvar *elf-callories* (mapcar #'sum-string-list *groups*))

;; problem 1 answer
(format t "Answer to the first part is ~a~%"
        (apply #'max *elf-callories*)) 

;; problem 2 answer
(setf *elf-callories* (sort *elf-callories* #'>))

(format t "Answer to the second part is ~a~%"
        (+ (first *elf-callories*)
           (second *elf-callories*)
           (third *elf-callories*)))
