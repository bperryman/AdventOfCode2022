;; Load in quicklisp
(load "ql:setup")

;; Pull in the data
(defparameter *raw-assignments*
  (uiop:read-file-lines "section-assignments.txt"))

(defvar *sample-raw-data* '("2-4,6-8"
                            "2-3,4-5"
                            "5-7,7-9"
                            "2-8,3-7"
                            "6-6,4-6"
                            "2-6,4-8"))

(defun parse-sections-pairs (section)
  "Takes a section, eg 3-4,9-10 and returns a list of start and end for
each pair, eg (3 4 9 10)."
  (let ((len (length section)))
    (labels ((process (start)
               (if (>= start len)
                   ()
                   (multiple-value-bind (number finish-index)
                       (parse-integer section :start start :junk-allowed t)
                     (cons number (process (1+ finish-index)))))))
      (process 0))))

(defparameter *cleanup-assignments*
  (mapcar #'parse-sections-pairs *raw-assignments*))

(defvar *sample-assignments*
  (mapcar #'parse-sections-pairs *sample-raw-data*))

(defun completely-overlapping-p (s1 e1 s2 e2)
  "Returns true if the numbers between s1-e1 and s2-e2 are completely contained within the other range"
  (or (and (>= s2 s1) (<= e2 e1))
      (and (>= s1 s2) (<= e1 e2))))

(format t "Completely overlapping sample count is ~a~%"
        (count-if #'identity *sample-assignments*
                  :key #'(lambda (x)
                           (apply #'completely-overlapping-p x))))

(format t "Completely overlapping assignment pairs count is ~a~%"
        (count-if #'identity *cleanup-assignments*
                  :key #'(lambda (x)
                           (apply #'completely-overlapping-p x))))

(defun any-overlap-p (s1 e1 s2 e2)
  "Returns try if there is any overlap between the ranges s1-e1 and s2-e2."
  (or (<= s1 s2 e1)
      (<= s2 s1 e2)))

(format t "Any overlap sample count is ~a~%"
        (count-if #'identity *sample-assignments*
                  :key #'(lambda (x)
                           (apply #'any-overlap-p x))))

(format t "Any overlap assignment pairs count is ~a~%"
        (count-if #'identity *cleanup-assignments*
                  :key #'(lambda (x)
                           (apply #'any-overlap-p x))))
