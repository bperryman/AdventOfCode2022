(defstruct node
  location
  flow-rate
  paths)


(defvar *valve-readtable* (copy-readtable))
(set-syntax-from-char #\= #\space *valve-readtable*)
(set-syntax-from-char #\; #\space *valve-readtable*)
(set-syntax-from-char #\, #\space *valve-readtable*)


(defun read-valve-entry (line)
  (unless (null line)
    (let ((*readtable* *valve-readtable*))
      (with-open-stream (stream (make-string-input-stream (format nil "(~a)" line)))
        (let ((data (read stream nil)))
          (make-node :location (second data)
                     :flow-rate (sixth data)
                     :paths (nthcdr 10 data)))))))

(defun read-data (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          collect (read-valve-entry line))))

(defun find-node (system location-label)
  (find location-label system :key #'node-location))

(defun can-open-valve-p (node open-valves)
  (and (> (node-flow-rate node) 0)      ; 0 is a broken valve
       (not (find node open-valves :test #'equalp))))

(defun all-valves-open-p (system open-valves)
  (dolist (valve system t)
    (when (can-open-valve-p valve open-valves)
      (return-from all-valves-open-p nil))))

(defun visitable-nodes (system node visited-nodes)
  (mapcar #'(lambda (label) (find-node system label))
          (remove-if #'(lambda (next-location)
                         (find (list (node-location node) next-location)
                               visited-nodes
                               :test #'equal))
                     (node-paths node))))

(defun release-valves (system current-node final-flow time open-valves paths-visited)
  (cond
    ((or (= 0 time) (all-valves-open-p system open-valves))
     (format t "Final flow of ~a at ~a minutes is ~a~%" final-flow time paths-visited)
     final-flow)
    ((can-open-valve-p current-node open-valves)
     (release-valves system
                     current-node
                     (+ final-flow (* (1- time) (node-flow-rate current-node)))
                     (1- time)
                     (cons current-node open-valves)
                     paths-visited))
    (t (apply #'max final-flow
              (mapcar #'(lambda (next-node)
                          (release-valves system
                                          next-node
                                          final-flow
                                          (1- time)
                                          open-valves
                                          (cons (list (node-location current-node)
                                                      (node-location next-node))
                                                paths-visited)))
                      (visitable-nodes system current-node paths-visited))))))
