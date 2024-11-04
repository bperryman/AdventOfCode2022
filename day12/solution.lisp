;; Assume that we have already loaded the advent-of-code quicklisp package.

(defun start-p (ch)
  (char= ch #\S))

(defun end-p (ch)
  (char= ch #\E))

(defun elevation (ch)
  "Returns the elevation of the character"
  (cond
    ((start-p ch) 0)
    ((end-p ch) 27)
    (t (1+ (- (char-code ch) (char-code #\a))))))

(defun line-elevation (line)
  (map 'list #'(lambda (c) (cons (elevation c) nil)) line))

(defun get-elevation (es-pair)
  (car es-pair))

(defun get-steps (es-pair)
  (cdr es-pair))

(defun set-steps (es-pair steps)
  (setf (cdr es-pair) steps))

(defun elevation-at (dataset pt)
  (get-elevation (aoc:data-at dataset pt)))

(defun steps-at (dataset pt)
  (get-steps (aoc:data-at dataset pt)))

(defun load-data (file)
  "Load the data file"
  (aoc:read-data-file file
                      :line-processor #'line-elevation
                      :dataset-processor #'aoc:process-dataset-to-matrix))


(defun solved (dataset)
  (some #'(lambda (position)
            (get-steps (aoc:data-at dataset position)))
        (aoc:positions-for dataset 27 :key #'get-elevation)))


(defun valid-step-p (dataset current-point proposed-point)
  (let ((size (aoc:dataset-size dataset)))
    (and (>= (aoc:point-x proposed-point) 0)
         (>= (aoc:point-y proposed-point) 0)
         (< (aoc:point-x proposed-point) (aoc:point-x size))
         (< (aoc:point-y proposed-point) (aoc:point-y size))
         (<= (- (elevation-at dataset proposed-point)
                (elevation-at dataset current-point))
             1)
         (null (steps-at dataset proposed-point)))))

(defun next-steps (dataset current-point)
  "Return the next steps that are possible from the current point"
  (flet ((valid-step (pt) (valid-step-p dataset current-point pt)))
    (remove-if-not #'valid-step
                   (aoc:surrounding-points current-point))))

(defun take-next-steps (dataset current-point)
  "Mark the points as reachable in step-number of steps"
  (let ((current-step (get-steps (aoc:data-at dataset current-point)))
        (points (next-steps dataset current-point)))
    (loop for pt in points
          do
          (set-steps (aoc:data-at dataset pt) (1+ current-step)))))

;; TODO: There is no checking for the situation where there aren't any possible moves.
(defun solution-1 (input)
  (let ((data (load-data input)))
    (dolist (pos (aoc:positions-for data 0 :key #'get-elevation))
      (set-steps (aoc:data-at data pos) 0))
    (loop for move-number from 0
          until (solved data)
          do
          (dolist (pt (aoc:positions-for data move-number :key #'get-steps))
            (take-next-steps data pt))
          finally (return move-number))))

;; Ahhhh, the perils of destroying your input data via mutation...
(defun reset-solution-data (data)
  (aoc:do-all-data #'(lambda (datum) (set-steps datum nil)) data))

(defun path-length-from (data pt)
  (set-steps (aoc:data-at data pt) 0)
  (loop for move-number from 0
        for positions = (aoc:positions-for data move-number :key #'get-steps)
        until (or (null positions) (solved data))
        do
        (dolist (pt (aoc:positions-for data move-number :key #'get-steps))
          (take-next-steps data pt))
        finally (return (if (null positions) most-positive-fixnum move-number))))

(defun solution-2 (input)
  (let ((data (load-data input)))
    (reduce #'min (mapcar #'(lambda (pt)
                              (reset-solution-data data)
                              (path-length-from data pt))
                          (aoc:positions-for data 1 :key #'get-elevation)))))
