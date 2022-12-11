;; Keeping things simple the registers will be stored in an association list
(defun initialize-cpu ()
  (list (cons 'x 1)
        (cons 'cycle 0)))

(defun get-register (state register)
  (cdr (assoc register state)))

(defun set-register (state register new-value)
  (rplacd (assoc register state) new-value))

(defun noop (state)
  (declare (ignore state))
  ())

(defun add-to-register (state register arg)
  (set-register state register (+ (get-register state register) arg)))

(defun add-x (state arg)
  (add-to-register state 'x arg))

(defstruct cpu-op
  instruction
  cycles
  arguments
  executer)

(defparameter *cpu-ops*
  (list
   (make-cpu-op :instruction 'noop :cycles 1 :arguments 0 :executer #'noop)
   (make-cpu-op :instruction 'addx :cycles 2 :arguments 1 :executer #'add-x)))

(defun decode-instruction (instruction)
  (find instruction *cpu-ops* :key #'cpu-op-instruction))

(defun read-n-arguments (stream n)
  (loop for i from 1 to n
        collect (read stream)))

(defun fetch-next-instruction (stream)
  (let ((instruction (read stream nil)))
    (when instruction
      (let ((decoded (decode-instruction instruction)))
        (cons decoded (read-n-arguments stream (cpu-op-arguments decoded)))))))

(defun execute-instruction (instruction state)
  (let ((op (first instruction))
        (op-args (rest instruction)))
    (apply (cpu-op-executer op) state op-args)
    (add-to-register state 'cycle (cpu-op-cycles op))))

(defun trace-data (state)
  (list (get-register state 'cycle)
        (get-register state 'x)))

(defun evaluate-file (file)
  (with-open-file (stream file)
    (let ((state (initialize-cpu)))
      (with-open-file (memory file)
        (do ((trace () (cons (trace-data state) trace))
             (instruction (fetch-next-instruction stream)
                          (fetch-next-instruction stream)))
            ((null instruction) (reverse trace))
          (execute-instruction instruction state))))))

;; Horribly in-effient
(defun find-x-at-cycle (cycle cycle-data)
  (let ((x nil))
    (dolist (entry cycle-data x)
      (when (< (first entry) cycle) (setf x (second entry))))))

(defun compute-signal-strength-at (points cycle-data)
  (loop for pt in points
        sum (* pt (find-x-at-cycle pt cycle-data))))

(defun analyze-file (file)
  (let ((trace (evaluate-file file)))
    (compute-signal-strength-at '(20 60 100 140 180 220) trace)))

(defun sprite-visible (beam-x x-register)
  (<= (1- x-register) beam-x (1+ x-register)))

(defun draw-data (file)
  (let ((cycle-data (evaluate-file file)))
    (let ((cycle 0)
          (register-x 1)
          (entries cycle-data))
      (do ((y 0 (1+ y)))
          ((= y 6))
        (format t "~&")
        (dotimes (x 40)
          (format t "~:[.~;#~]" (sprite-visible x register-x))
          (incf cycle)
          (when (= cycle (first (first entries)))
            (setf register-x (second (first entries)))
            (setf entries (rest entries))))))))
