(defgeneric is-dir-p (entry)
  (:documentation "Returns true if the entry is a fs-dir"))
(defgeneric size (entry)
  (:documentation "Returns the size of the entry"))

(defclass fs-dir ()
  ((name :initarg :name :accessor name)
   (contents :initarg :contents :accessor contents))
  (:default-initargs :name "/" :contents ()))

(defclass fs-file ()
  ((name :initarg :name :accessor name)
   (size :initarg :size :accessor size)))

(defmethod print-object ((object fs-dir) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s" (name object))))

(defmethod print-object ((object fs-file) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s (~a)" (name object) (size object))))

(defmethod is-dir-p ((dir fs-dir)) t)
(defmethod is-dir-p ((file fs-file)) nil)

(defmethod size ((dir fs-dir))
  (loop for entry in (contents dir)
        sum (size entry)))

(defmethod find-directory ((dir fs-dir) path)
  (if (null path)
      dir
      (let ((next (find (car path) (contents dir) :test #'string= :key #'name)))
        (if (null next)
            (error "No path to directory")
            (find-directory next (cdr path))))))

(defmethod register-in-directory ((dir fs-dir) path entry)
  (let ((dest-dir (find-directory dir path)))
    (push entry (contents dest-dir))))

(defmethod walk-file-system ((dir fs-dir) callback)
  (funcall callback dir)
  (dolist (f (contents dir))
    (walk-file-system f callback)))

(defmethod walk-file-system ((file fs-file) callback)
  (funcall callback file))

(defun string-starts-with-p (string leader)
  (string= string leader :end1 (length leader)))

(defun is-command-p (string)
  (char= #\$ (char string 0)))

(defun is-directory-entry-p (string)
  (string-starts-with-p string "dir"))

(defun is-file-entry-p (string)
  (not (or (is-command-p string)
           (is-directory-entry-p string))))


(defun cd (current-path change)
  (cond
    ((string= "/" change) ())
    ((string= ".." change) (butlast current-path))
    (t (append current-path (list change)))))

(defun parse-command (cmd)
  (cond
    ((string-starts-with-p cmd "$ ls") '(:ls))
    ((string-starts-with-p cmd "$ cd") (list :cd (subseq cmd 5)))
    (t (error "unknown command"))))

(defun parse-directory-entry (line)
  (cond
    ((string-starts-with-p line "dir")
     (make-instance 'fs-dir :name (subseq line 4)))
    (t (make-instance 'fs-file
                      :size (parse-integer line :junk-allowed t)
                      :name (subseq line (1+ (position #\space line :test #'char=)))))))

(defun process-interaction (file)
  (let ((current-path ())
        (fs-top (make-instance 'fs-dir)))
    (with-open-file (commands file :direction :input :if-does-not-exist :error)
      (do ((line (read-line commands nil) (read-line commands nil)))
          ((null line) fs-top)
        (if (is-command-p line)
            (let ((cmd (parse-command line)))
              (case (first cmd)
                (:ls 'ignore)
                (:cd (setf current-path (cd current-path (second cmd))))))
            (register-in-directory fs-top current-path (parse-directory-entry line)))))))

(defun all-large-folders (start &optional (size 100000))
  (let ((count 0)
        (total-size 0))
    (walk-file-system start #'(lambda (entry)
                                (when (and (is-dir-p entry)
                                           (<= (size entry) size))
                                  (incf count)
                                  (incf total-size (size entry)))))
    (values count total-size)))


(defvar *fs* (process-interaction "commands.txt"))

(multiple-value-bind (count size) (all-large-folders *fs*)
    (format t "There are a total of ~a directories <= 100000 bytes, with a total size of ~a~%" count size))

(defun find-directory-to-delete (start &key (disk-size 70000000) (target-free-space 30000000))
  (let ((minimum-size-directory (- target-free-space (- disk-size (size start))))
        (candidates ()))
    (walk-file-system start
                      #'(lambda (entry)
                          (when (and (is-dir-p entry)
                                     (>= (size entry) minimum-size-directory))
                            (push entry candidates))))
    candidates))

(format t "Smalled directory to delete is ~a bytes in size~%" (first (sort (mapcar #'size (find-directory-to-delete *fs*)) #'<)))
