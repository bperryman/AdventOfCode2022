(defun dimensions-for-file-data (file)
  (with-open-file (f file)
    (do ((l (read-line f nil) (read-line f nil))
         (len 0 (length l))
         (lines 0 (1+ lines)))
        ((null l) (list lines len)))))

(defun read-grid-data (file)
  (let ((data (make-array (dimensions-for-file-data file) :element-type 'fixnum :initial-element 0)))
    (with-open-file (f file)
      (do ((l (read-line f nil) (read-line f nil))
           (row 0 (1+ row)))
          ((null l) data)
        (loop for i from 0 below (length l)
              do
                 (setf (aref data row i) (- (char-code (char l i)) (char-code #\0))))))))

(defun iter-row (matrix row callback &key start from-end)
  (let ((columns (array-dimension matrix 1))) 
    (let ((start-point (if start start (if from-end (1- columns) 0)))
          (delta (if from-end -1 1))
          (stop (if from-end -1 columns)))
      (do ((index start-point (+ delta index)))
          ((= index stop) nil)
        (funcall callback matrix row index)))))

(defun iter-column (matrix column callback &key start from-end)
  (let ((rows (array-dimension matrix 0))) 
    (let ((start-point (if start start (if from-end (1- rows) 0)))
          (delta (if from-end -1 1))
          (stop (if from-end -1 rows)))
      (do ((index start-point (+ delta index)))
          ((= index stop) nil)
        (funcall callback matrix index column)))))

(defun mark-tallest-for-direction (src marks &key row column from-end)
  (unless (or row column)
    (error "No row or column specified"))
  (let ((tallest -1))
    (flet ((mark-tallest (data r c)
             (when (> (aref data r c) tallest)
               (setf (aref marks r c) 1)
               (setf tallest (aref data r c)))))
      (if row
          (iter-row src row #'mark-tallest :from-end from-end)
          (iter-column src column #'mark-tallest :from-end from-end)))))

(defun mark-tallest (src-grid mark-grid)
  (destructuring-bind (rows columns) (array-dimensions src-grid)
    (loop for r from 0 below rows
          do
             (loop for c from 0 below columns
                   do
                   (mark-tallest-for-direction src-grid mark-grid :row r)
                   (mark-tallest-for-direction src-grid mark-grid :row r :from-end t)
                   (mark-tallest-for-direction src-grid mark-grid :column c)
                   (mark-tallest-for-direction src-grid mark-grid :column c :from-end t))))
  mark-grid)

(defun create-mark-array (src)
  (make-array (array-dimensions src) :element-type 'fixnum :initial-element 0))

(defun count-tallest-marks (marks)
  (destructuring-bind (rows columns) (array-dimensions marks)
    (loop for r from 0 below rows
          sum (loop for c from 0 below columns
                    sum (aref marks r c)))))

(defun tallest-in-file (file)
  (let* ((src (read-grid-data file))
         (marks (create-mark-array src)))
    (mark-tallest src marks)
    (count-tallest-marks marks)))

(format t "Visible count is ~a~%" (tallest-in-file "grid.txt"))

;; part 2
(defun view-in-column-from (grid column start &key from-end)
  (let ((view-distance 0)
        (view-blocked nil)
        (location-height (aref grid start column)))
    (flet ((view-blocked (data r c)
             (unless view-blocked
               (incf view-distance)
               (setf view-blocked (>= (aref data r c) location-height)))))
      (iter-column grid column #'view-blocked :start (if from-end (1- start) (1+ start)) :from-end from-end)
      view-distance)))

(defun view-in-row-from (grid row start &key from-end)
  (let ((view-distance 0)
        (view-blocked nil)
        (location-height (aref grid row start)))
    (flet ((view-blocked (data r c)
             (unless view-blocked
               (incf view-distance)
               (setf view-blocked (>= (aref data r c) location-height)))))
      (iter-row grid row #'view-blocked :start (if from-end (1- start) (1+ start)) :from-end from-end)
      view-distance)))


(defun view-extends-for (grid r c)
  (* (view-in-column-from grid c r)
     (view-in-column-from grid c r :from-end t)
     (view-in-row-from grid r c)
     (view-in-row-from grid r c :from-end t)))

(defun best-view-score (file)
  (let ((src (read-grid-data file)))
    (destructuring-bind (rows columns) (array-dimensions src)
      (loop for r from 0 below rows
            maximize (loop for c from 0 below columns
                           maximize (view-extends-for src r c))))))

(format t "Best view score for data is ~a~%" (best-view-score "grid.txt"))
