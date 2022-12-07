(load "stack-data.lisp")

(defvar *working-stack* nil)

(defun get-stack (id)
  (aref *working-stack* (1- id)))

(defun (setf get-stack) (new-value stack-id)
  (setf (aref *working-stack* (1- stack-id)) new-value))

(defun display-working-stack ()
  (loop for stack across *working-stack*
        do (format t "~a~%" stack)
        finally (format t "====================~%")))

(defun apply-move (quantity from to)
  (when (> quantity 0)
    (let ((val (pop (get-stack from))))
      (when (null val)
        (error "Stack empty, no pop performed"))
      (push val (get-stack to))
      (apply-move (1- quantity) from to))))

(defun make-moves (stack moves)
  (let ((*working-stack* stack))
    (dolist (m moves)
      (apply-move (second m) (fourth m) (sixth m)))
    (display-working-stack)))

;; Just read down the screen after this last move.
(make-moves *boat-stacks*  *crane-moves*)
(format t "~&Read down the list for the answer to part 1~%")

(defparameter *sample-moves*
  '((move 1 from 2 to 1)
    (move 3 from 1 to 3)
    (move 2 from 2 to 1)
    (move 1 from 1 to 2)))

(defparameter *sample-stack*
  (make-array 3 :initial-contents '((n z)
                                    (d c m)
                                    (p))))

(defun apply-9001-move (quantity from to)
  (let ((to-move (subseq (get-stack from) 0 quantity)))
    (setf (get-stack from) (subseq (get-stack from) quantity)
          (get-stack to) (append to-move (get-stack to)))))

(defun make-9001-moves (stack moves)
  (let ((*working-stack* stack))
    (dolist (m moves)
      (apply-9001-move (second m) (fourth m) (sixth m)))
    (display-working-stack)))

;; Pop quiz - why did I think this might be the right time to use specials?
(load "stack-data.lisp")

;; Just read down the screen after this last move.
(make-9001-moves *boat-stacks*  *crane-moves*)
(format t "~&Read down the list for the answer to part 2~%")
