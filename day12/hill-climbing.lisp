;; Terrain and hill climbing

(defclass point ()
  ((x :initarg :x :accessor point-x)
   (y :initarg :y :accessor point-y)))

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(~a, ~a)" (point-x object) (point-y object))))

(defclass terrain ()
  ((landscape :initarg :landscape :reader landscape)))

(defun read-terrain (file)
  (flet ((massage-terrain (line)
           (map 'array
                (lambda (c)
                  (case c
                    (#\S 0)
                    (#\E 27)
                    (t (1+ (- (char-code c) (char-code #\a))))))
                line)))
    (make-instance 'terrain 
      :landscape (with-open-file (f file)
                   (loop for line = (read-line f nil)
                     while line
                     collect (massage-terrain line) into lines
                     finally (return (make-array (list (length lines)
                                                       (length (first lines)))
                                                 :initial-contents lines)))))))

(defmethod terrain-size ((ter terrain))
  (destructuring-bind (rows columns) (array-dimensions (landscape ter))
    (make-instance 'point :x columns :y rows)))

(defmethod height-at-location ((ter terrain) (pt point))
  (aref (landscape ter) (point-y pt) (point-x pt)))

(defmethod locations-for-height ((landscape terrain) height)
  (let ((locations ())
        (size (terrain-size landscape)))
    (dotimes (r (point-y size) locations)
      (dotimes (c (point-x size))
        (let ((pt (make-instance 'point :x c :y r)))
          (when (= height (height-at-location landscape pt))
            (push pt locations)))))))

(defmethod location-on-terrain-p ((landscape terrain) (location point))
  (let ((size (terrain-size landscape)))
    (and (< -1 (point-x location) (point-x size))
         (< -1 (point-y location) (point-y size)))))

(defmethod surrounding-locations ((landscape terrain) (point point))
  (let ((y (point-y point))
        (x (point-x point)))
    (remove-if-not (lambda (pt) (location-on-terrain-p landscape pt))
                   (list (make-instance 'point :x (1- x) :y y)
                         (make-instance 'point :x (1+ x) :y y)
                         (make-instance 'point :x x :y (1- y))
                         (make-instance 'point :x x :y (1+ y))))))

(defmethod adjacent-locations-p ((landscape terrain) (pt1 point) (pt2 point))
  (and (location-on-terrain-p landscape pt1)
       (location-on-terrain-p landscape pt2)
       (<= 1 (abs (- (point-x pt1) (point-x pt2))))
       (<= 1 (abs (- (point-y pt1) (point-y pt2))))))

(defmethod can-move-to-location-p ((landscape terrain) (current-location point) (target-location point))
  (let ((current-height (height-at-location landscape current-location))
        (target-height (height-at-location landscape target-location)))
    (and (adjacent-locations-p current-location target-location)
         (<= 0 (- target-height current-height) 1))))

(defmethod paths-to-height ((landscape terrain) (location point) height)
  (let (