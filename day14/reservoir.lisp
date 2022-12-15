(defstruct point
  x y)

(defun add-points (pt1 pt2)
  (make-point :x (+ (point-x pt1) (point-x pt2))
              :y (+ (point-y pt1) (point-y pt2))))

(defun sub-points (pt1 pt2)
  (make-point :x (- (point-x pt1) (point-x pt2))
              :y (- (point-y pt1) (point-y pt2))))

(defun magnitude (pt)
  (sqrt (+ (expt (point-x pt) 2)
           (expt (point-y pt) 2))))

(defun unit-vector (pt)
  (let ((mag (magnitude pt)))
    (make-point :x (/ (point-x pt) mag)
                :y (/ (point-y pt) mag))))

(defparameter *point-readtable* (copy-readtable *readtable*))
(set-syntax-from-char #\, #\space *point-readtable*)
(set-syntax-from-char #\- #\space *point-readtable*)
(set-syntax-from-char #\> #\space *point-readtable*)

(defun parse-point (stream)
  (let ((*readtable* *point-readtable*))
    (let ((x (read stream nil))
          (y (read stream nil)))
      (unless (or (null x) (null y))
        (make-point :x x :y y)))))

(defun parse-path (line)
  (with-open-stream (s (make-string-input-stream line))
    (loop for pt = (parse-point s)
          while pt
          collect pt)))

(defun load-data (file)
  (with-open-file (f file)
    (loop for line = (read-line f nil)
          while line
          collect (parse-path line))))

(defun bounding-box (paths)
  (loop for pt in (apply #'append paths)
        minimize (point-x pt) into xmin
        minimize (point-y pt) into ymin
        maximize (point-x pt) into xmax
        maximize (point-y pt) into ymax
        finally (return (values (make-point :x xmin :y ymin)
                                (make-point :x xmax :y ymax)))))

(defstruct scan
  slice
  index-offsets
  min-bb
  max-bb)

(defun create-scan-for-data (paths)
  (multiple-value-bind (min-pt max-pt) (bounding-box paths)
    (let ((size (sub-points max-pt min-pt)))
      (make-scan :slice (make-array (list (+ 2 (point-y max-pt))
                                          (+ 3 (point-x size)))
                                    :element-type 'fixnum
                                    :initial-element 0)
                 :index-offsets (make-point :x (1- (point-x min-pt)) :y 0)
                 :min-bb (make-point :x (point-x min-pt) :y 0)
                 :max-bb max-pt))))

(defun point-inside-bounding-box (scan pt)
  (and (<= (point-x (scan-min-bb scan))
           (point-x pt)
           (point-x (scan-max-bb scan)))
       (<= (point-y (scan-min-bb scan))
           (point-y pt)
           (point-y (scan-max-bb scan)))))

(defun get-point-in-scan (scan pt)
  (if (point-inside-bounding-box scan pt)
      (let ((plot-pt (sub-points pt (scan-index-offsets scan))))
        (aref (scan-slice scan)
              (floor (point-y plot-pt))
              (floor (point-x plot-pt))))
      0))

(defun add-point-to-scan (scan pt value)
  (let ((plot-pt (sub-points pt (scan-index-offsets scan))))
    (setf (aref (scan-slice scan)
                (floor (point-y plot-pt))
                (floor (point-x plot-pt)))
          value)))

(defun add-line-to-scan (scan start end value)
  (let* ((delta (sub-points end start))
         (steps (1+ (floor (magnitude delta))))
         (move-delta (unit-vector delta))
         (current start))
    (dotimes (i steps scan)
      (add-point-to-scan scan current value)
      (setf current (add-points current move-delta)))))

(defun add-line-path-to-scan (scan path value)
  (do ((start (first path) (first remaining-path))
       (remaining-path (rest path) (rest remaining-path)))
      ((null remaining-path) scan)
    (add-line-to-scan scan start (first remaining-path) value)))


(defun add-rock-data-to-scan (scan paths)
  (loop for path in paths
        do
           (add-line-path-to-scan scan path 1)))

(defun position-is-clear-p (scan position)
  (= 0 (get-point-in-scan scan position)))

(defparameter *drop-delta* (make-point :x 0 :y 1))
(defparameter *drop-left* (make-point :x -1 :y 1))
(defparameter *drop-right* (make-point :x 1 :y 1))

(defun point-below-bounding-box (scan position)
  (declare (optimize (debug 3)))
  (> (point-y position) (point-y (scan-max-bb scan))))

(defun sand-drop (scan position)
  "Updates the scan with the final resting place for the grain of sand, if there is one.
Returns t if the grain of sand is trapped and nil if it falls into the void or there is
no space for it to go."
  (declare (optimize (debug 3)))
  (let ((down (add-points *drop-delta* position))
        (down-left (add-points *drop-left* position))
        (down-right (add-points *drop-right* position)))
    (cond
      ((or (not (position-is-clear-p scan position))
           (point-below-bounding-box scan position))
       nil)
      ((position-is-clear-p scan down) (sand-drop scan down))
      ((position-is-clear-p scan down-left) (sand-drop scan down-left))
      ((position-is-clear-p scan down-right) (sand-drop scan down-right))
      (t (add-point-to-scan scan position 2)
         t))))

(defun render-scan (scan)
  (let ((data (scan-slice scan)))
    (destructuring-bind (y x) (array-dimensions data)
      (loop for yi from 0 below y
            do
               (progn
                 (terpri)
                 (loop for xi from 0 below x
                       do
                       (format t "~[.~;#~;O~]" (aref data yi xi))))))))

(defun problem-one (file)
  (let* ((paths (load-data file))
         (scan (create-scan-for-data paths))
         (drop-point (make-point :x 500 :y (point-y (scan-min-bb scan)))))
    (add-rock-data-to-scan scan paths)
    (loop for drops = 0 then (1+ drops)
          while (sand-drop scan drop-point)
          finally (return (values drops scan)))))

(format t "Solution to problem 1 is ~a~%" (problem-one "scan.txt"))

(defun problem-two (file additional-line-length)
  (let ((paths (load-data file)))
    (multiple-value-bind (min-bb max-bb) (bounding-box paths)
      ;; Add in our "infinite" floor
      (setf paths (cons (list (make-point :x (- (point-x min-bb) additional-line-length)
                                          :y (+ (point-y max-bb) 2))
                              (make-point :x (+ (point-x max-bb) additional-line-length)
                                          :y (+ (point-y max-bb) 2)))
                        paths))
      (let ((scan (create-scan-for-data paths))
            (drop-point (make-point :x 500 :y 0)))
        (add-rock-data-to-scan scan paths)
        (loop for drops = 0 then (1+ drops)
              while (sand-drop scan drop-point)
              finally (return (values drops scan)))))))

(format t "Solution to problem 2 is ~a~%" (problem-two "scan.txt" 450))
