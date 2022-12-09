(defun load-moves (file)
  (with-open-file (f file)
    (loop for line = (read-line f nil)
          while line
          collect (list (read-from-string line)
                        (read-from-string line nil nil :start 2)))))

(defstruct point
  x y)

(defun point-equal-p (p1 p2)
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))

(defun distance (head tail)
  (max (abs (- (point-x head) (point-x tail)))
       (abs (- (point-y head) (point-y tail)))))

(defun tail-move-required-p (head tail)
  (> (distance head tail) 1))

(defun move-head (direction head)
  "Moves the head one unit in the direction specified"
  (case direction
    (u (incf (point-y head)))
    (d (decf (point-y head)))
    (l (incf (point-x head)))
    (r (decf (point-x head)))))

(defun move-tail (head tail)
  (let ((new-tail (copy-point tail)))
   (when (tail-move-required-p head tail)
     (let ((hx (point-x head))
           (hy (point-y head))
           (tx (point-x tail))
           (ty (point-y tail)))
       (cond
         ((and (> hx tx) (> hy ty))
          (incf (point-x new-tail))
          (incf (point-y new-tail)))
         ((and (> hx tx) (< hy ty))
          (incf (point-x new-tail))
          (decf (point-y new-tail)))
         ((and (< hx tx) (< hy ty))
          (decf (point-x new-tail))
          (decf (point-y new-tail)))
         ((and (< hx tx) (> hy ty))
          (decf (point-x new-tail))
          (incf (point-y new-tail)))
         ((> hx tx) (incf (point-x new-tail)))
         ((< hx tx) (decf (point-x new-tail)))
         ((> hy ty) (incf (point-y new-tail)))
         ((< hy ty) (decf (point-y new-tail))))))
    new-tail))

(defun execute-rope-moves (file)
  (let ((head (make-point :x 0 :y 0))
        (tail (make-point :x 0 :y 0))
        (moves (load-moves file))
        y-positions)
    (dolist (move moves y-positions)
      (let ((direction (first move))
            (distance (second move)))
        (dotimes (i distance)
          (move-head direction head)
          (setf tail (move-tail head tail))
          (setf y-positions (adjoin tail y-positions
                                    :test #'point-equal-p)))))))

(format t "Tail covers a total of ~a positions~%"
        (length (execute-rope-moves "moves.txt")))

(defun execute-longer-rope-moves (file)
  (let ((rope-elements (make-array 10))
        (moves (load-moves file))
        y-positions)
    (dotimes (i 10)
      (setf (aref rope-elements i) (make-point :x 0 :y 0)))
    (dolist (move moves y-positions)
      (let ((direction (first move))
            (distance (second move)))
        (dotimes (i distance)
          (move-head direction (aref rope-elements 0))
          (dotimes (head-index 9)
            (setf (aref rope-elements (1+ head-index))
                  (move-tail (aref rope-elements head-index)
                             (aref rope-elements (1+ head-index)))))
          (setf y-positions (adjoin (aref rope-elements 9)
                                    y-positions
                                    :test #'point-equal-p)))))))

(format t "Longer tail covers a total of ~a positions~%"
        (length (execute-longer-rope-moves "moves.txt")))
