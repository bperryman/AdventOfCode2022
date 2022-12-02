;; Assumes we have loaded quicklisp or ASDF at least

(defun read-game-data (file)
  (flet ((read-data-line (line)
           (list (read-from-string line)
                 (read-from-string line nil nil :start 2))))
    (let ((data (uiop:read-file-lines file)))
      (mapcar #'read-data-line data))))

(defstruct rock-paper-scissors
  play
  beaten-by
  player-1-representation
  player-2-representation
  play-value)

(defparameter *rock-paper-scissors*
  (list
   (make-rock-paper-scissors 
    :play 'rock 
    :beaten-by 'paper
    :player-1-representation 'a 
    :player-2-representation 'x
    :play-value 1)
   (make-rock-paper-scissors
    :play 'paper
    :beaten-by 'scissors
    :player-1-representation 'b
    :player-2-representation 'y
    :play-value 2)
   (make-rock-paper-scissors
    :play 'scissors 
    :beaten-by 'rock
    :player-1-representation 'c
    :player-2-representation 'z
    :play-value 3)))

(defun lookup-rock-paper-scissors (played player)
  (find played *rock-paper-scissors*
        :key (if (eq player 'player-1) 
                 #'rock-paper-scissors-player-1-representation
                 #'rock-paper-scissors-player-2-representation)))

(defun my-played-value (play)
  (let ((played (second play)))
    (rock-paper-scissors-play-value (lookup-rock-paper-scissors played 'player-2))))

(defun score-plays (player-1 player-2)
  (cond
   ((eq (rock-paper-scissors-play player-1) (rock-paper-scissors-play player-2)) 3)
   ((eq (rock-paper-scissors-beaten-by player-1) (rock-paper-scissors-play player-2)) 6)
   (t 0)))

(defun played-score (play)
  (let ((player-1 (lookup-rock-paper-scissors (first play) 'player-1))
        (player-2 (lookup-rock-paper-scissors (second play) 'player-2)))
    (score-plays player-1 player-2)))

(defun score-play (play)
  (+ (my-played-value play)
     (played-score play)))

(defvar *demo-game-data* '((A Y) (B X) (C Z)))

(defparameter *game-data* (read-game-data "home:src;AdventOfCode2022;day2;round-1.txt"))

(defparameter *play-scores* (mapcar #'score-play *game-data*))

(format t "Solution for part one - ~a~%" (reduce #'+ *play-scores*))

;; part 2
(defun find-my-play (player-1 play-code)
  (let ((player-1 (rock-paper-scissors-play (lookup-rock-paper-scissors player-1 'player-1))))
    (case play-code
      (x (find player-1 *rock-paper-scissors* :key #'rock-paper-scissors-beaten-by))
      (y (find player-1 *rock-paper-scissors* :key #'rock-paper-scissors-play))
      (z (find (rock-paper-scissors-beaten-by (find player-1 *rock-paper-scissors* :key #'rock-paper-scissors-play))
               *rock-paper-scissors* :key #'rock-paper-scissors-play)))))

(defun strategy-score (play)
  (let ((player-1 (lookup-rock-paper-scissors (first play) 'player-1))
        (player-2 (find-my-play (first play) (second play))))
    (+ (score-plays player-1 player-2)
       (my-played-value (list (first play) (rock-paper-scissors-player-2-representation player-2))))))

(defparameter *strategy-scores* (mapcar #'strategy-score *game-data*))

(format t "Solution for part two - ~a~%" (reduce #'+ *strategy-scores*))
