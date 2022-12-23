(load "interval")

(defstruct point
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defun manhatten-distance (pt1 pt2)
  (+ (abs (- (point-x pt1) (point-x pt2)))
     (abs (- (point-y pt1) (point-y pt2)))))

(defstruct sensor
  location
  located-beacon
  beacon-distance)

(defun extract-points (line &key (start (1+ (position #\= line :test #'char=))))
  (multiple-value-bind (value ends)
      (parse-integer line :start start :junk-allowed t)
    (let ((next (position #\= line :test #'char= :start ends)))
      (if (null next)
          (list value)
          (cons value (extract-points line :start (1+ next)))))))

(defun sensor-from-line (line)
  (destructuring-bind (sensor-x sensor-y beacon-x beacon-y)
      (extract-points line)
    (let ((location (make-point :x sensor-x :y sensor-y))
          (beacon (make-point :x beacon-x :y beacon-y)))
      (make-sensor :location location
                   :located-beacon beacon
                   :beacon-distance (manhatten-distance location beacon)))))

(defun read-data (file)
     (with-open-file (f file)
       (loop for line = (read-line f nil)
             while line
             collect (sensor-from-line line))))

;; Approach is to figure out how far from the line each sensor is
;; With the sensor to beacon distance that would give us a distance left
;; along the line.
;; Next problem is then combining ranges to a unified set of ranges.

(defun interval-coverage-in-line (sensor line-y)
  (declare (optimize debug))
  (let* ((location (sensor-location sensor))
         (line-point (make-point :x (point-x location) :y line-y))
         (distance-to-line (manhatten-distance location line-point))
         (line-to-beacon (- (sensor-beacon-distance sensor) distance-to-line)))
    (if (< line-to-beacon 0)
        nil
        (create-interval (- (point-x location) line-to-beacon)
                         (+ (point-x location) line-to-beacon)))))

(defun sensor-coverage-in-line (sensors line-y)
  (declare (optimize debug))
  (let ((result ()))
    (dolist (sensor sensors result)
      (let ((line-coverage (interval-coverage-in-line sensor line-y)))
        (unless (null line-coverage)
          (setf result (adjoin-interval line-coverage result)))))))

(defun count-beacons-on-line (sensors line-y)
  (let ((unique-beacons ())
        (candidates (remove-if-not (lambda (beacon) (= line-y (point-y beacon)))
                                   (mapcar #'sensor-located-beacon sensors))))
    (dolist (c candidates (length unique-beacons))
      (setf unique-beacons (adjoin c unique-beacons :test #'equalp)))))

(defun problem-one (file line-y)
  (let ((sensors (read-data file)))
    (- (reduce #'+ (mapcar #'interval-length (sensor-coverage-in-line sensors line-y)))
       (count-beacons-on-line sensors line-y))))

(format t "Answer to problem one is ~a~%" (problem-one "readings.txt" 2000000))

(defun beacon-frequency (x y)
  (+ y (* 4000000 x)))

(defun problem-two (file range-end)
  (let ((sensors (read-data file))
        (interval (create-interval 0 range-end)))
    (loop for i from 0 to range-end
          for line-coverage = (sensor-coverage-in-line sensors i)
          when (null (find-if (lambda (int) (sub-interval-p interval int)) line-coverage))
            collect (list i line-coverage))))

;; OK this next bit is a bit trixy - it would involve writing a bit
;; more code and well... throw-away problem so here's the answer I got
;; by eyeballing the results of calling problem two:
;;   (problem-two "/Users/barry/src/AdventOfCode2022/day15/readings.txt" 4000000)
;; reading off the result and calling:
;;   (beacon-frequency 3172756 2767556)
;; 12691026767556
