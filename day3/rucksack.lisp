(load "ql:setup")

(defun priority (char)
  (cond
    ((lower-case-p char) (1+ (- (char-code char) (char-code #\a))))
    ((upper-case-p char) (+ 27 (- (char-code char) (char-code #\A))))
    (t (error "Not a letter"))))

(defun compartmentalize-rucksack (contents)
  (let ((len (length contents)))
    (list (subseq contents 0 (/ len 2))
          (subseq contents (/ len 2)))))

(defun misplaced-items-value (contents)
  (let* ((content-values (map 'list #'priority contents))
         (compartments (compartmentalize-rucksack content-values))
         (item-values (apply #'intersection compartments)))
    (cond
      ((= 0 (length item-values))
       (error "Expect only a single item to have been misplaced"))
      ((or (= 1 (length item-values))
           (apply #'= item-values))      ; Check all values are the same
       (first item-values))
      (t (error "Different values detected")))))

(defvar *sample-data* '("vJrwpWtwJgWrhcsFMMfFFhFp"
                        "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                        "PmmdzqPrVvPwwTWBwg"
                        "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                        "ttgJtRGJQctTZtZT"
                        "CrZsJsPPZsGzwwsLwLmpwMDw"))

(format t "Sample data sum is ~a~%"
        (reduce #'+ (mapcar #'misplaced-items-value *sample-data*)))

(defvar *elf-rucksacks* (uiop:read-file-lines "elf-rucksacks.txt"))

(format t "Sum of misplaced items is ~a~%"
        (reduce #'+ (mapcar #'misplaced-items-value *elf-rucksacks*)))

;; There may well be an easier way to grab the rucksacks in groups of 3
;; possibly using loop since it seems to be a kitchen sink tool.
(defun locate-badges (rucksacks)
  (if (null rucksacks)
      ()
      (destructuring-bind (r1 r2 r3 &rest remainder) rucksacks
          (let ((r1-list (map 'list #'identity r1))
                (r2-list (map 'list #'identity r2))
                (r3-list (map 'list #'identity r3)))
            (cons (first (intersection (intersection r1-list r2-list :test #'char=)
                                        r3-list :test #'char=))
                  (locate-badges remainder))))))

(format t "Sum of the badges priorities is ~a"
        (reduce #'+ (mapcar #'priority (locate-badges *elf-rucksacks*))))
