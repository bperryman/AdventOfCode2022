(defun read-data (string)
  (read-from-string 
   (substitute #\( #\[ 
               (substitute #\) #\] 
                           (substitute #\space #\, string)))))

(defun ensure-list (obj)
  (if (consp obj)
      obj
      (list obj)))

(defun compare (left right)
  (declare (optimize debug))
  (labels ((iter (l r)
             (cond
              ((and (null l) (null r)) t)
              ((and (null l) (not (null r))) (return-from compare t))
              ((and (not (null l)) (null r)) nil)
              ((and (numberp l) (numberp r))
               (if (< l r)
                   (return-from compare t)
                   (<= l r)))
              ((and (consp l) (consp r)) (and (iter (car l) (car r))
                                                (iter (cdr l) (cdr r))))
              (t (iter (ensure-list l) (ensure-list r))))))
    (iter left right)))


(defun compare-file-pairs (file)
  (with-open-file (f file)
    (loop for left = (read-data (read-line f))
          for right = (read-data (read-line f))
          for index = 1 then (1+ index)
      when (compare left right)
      collect index
      while (not (null (read-line f nil))))))

(format t "Solution to part one is ~a~%"
        (apply #'+ (compare-file-pairs "signal-pairs.txt")))

(defun load-pairs-and-markers (file)
  (with-open-file (f file)
    (cons '((2))
          (cons '((6))
                (loop for line = (read-line f nil)
                      while line
                      unless (string= line "")
                        collect (read-data line))))))

(format t "Solution to part two is ~a~%"
        (let ((data (sort (load-pairs-and-markers "signal-pairs.txt")
                          #'compare)))
          (* (1+ (position '((2)) data :test #'equalp))
             (1+ (position '((6)) data :test #'equalp)))))
