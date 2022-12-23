#|
This is a pretty basic implementation of intervals

|#
(defstruct interval
  (start 0 :type fixnum)
  (end 0 :type fixnum))

(defun create-interval (start end)
  (when (< end start)
    (error "End occurs before the start"))
  (make-interval :start start :end end))

(defun starts-first-p (interval-1 interval-2)
  "Returns try if interval-1 starts before or at the same point as
interval-2. "
  (<= (interval-start interval-1) (interval-start interval-2)))

(defun overlapping-p (interval-1 interval-2)
  "Returns true if there is an overlap between interval-1 and
interval-2"
  (if (starts-first-p interval-1 interval-2)
      (<= (interval-start interval-1)
          (interval-start interval-2)
          (interval-end interval-1))
      (<= (interval-start interval-2)
          (interval-start interval-1)
          (interval-end interval-2))))

(defun sub-interval-p (smaller larger)
  "Returns true if the first, smaller, interval is a subset of the
second, larger, one"
  (and (<= (interval-start larger) (interval-start smaller))
       (>= (interval-end larger) (interval-end smaller))))

(defun merge-intervals (interval-1 interval-2)
  "if there is an overlap in the intervals then return a new interval
that covers them both, otherwise return null"
  (when (overlapping-p interval-1 interval-2)
    (create-interval (min (interval-start interval-1)
                          (interval-start interval-2))
                     (max (interval-end interval-1)
                          (interval-end interval-2)))))

(defun interval-length (interval)
  "Returns the length of the interval - assumes that the interval is a
closed interval."
  (1+ (- (interval-end interval) (interval-start interval))))

(defun adjoin-interval (interval interval-list)
  (cond
    ((null interval-list)
     (list interval))
    ((overlapping-p (car interval-list) interval)
     (adjoin-interval (merge-intervals (car interval-list) interval)
                      (cdr interval-list)))
    ((starts-first-p (car interval-list) interval)
     (cons (car interval-list)
           (adjoin-interval interval (cdr interval-list))))
    (t 
     (cons interval interval-list))))

