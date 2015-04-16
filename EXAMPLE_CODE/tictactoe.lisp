;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File   : tictactoe.lisp
;; Author : Sandeep Koranne
;; Purpose: Board evaluation example
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
The "board" in any game is implemented for performance.
|#

(defvar *board* nil)       ; global board
(defvar *A-board* nil)     ; player-A board
(defvar *B-board* nil)     ; player-B board
(defvar *prime-array* nil) ; our evaluation board system
(defvar *move-hash* nil)
(defvar *winning-combinations* nil)

(defun initialize-boards ()
  (setf *board* (make-array '(3 3) :initial-element nil))
  (setf *A-board* (make-array '(3 3) :initial-element 0))
  (setf *B-board* (make-array '(3 3) :initial-element 0))
  (setf *prime-array* (make-array '(3 3) :initial-contents '((2 3 5) (7 11 13) (17 19 23))))
  (setf *move-hash* (make-hash-table))
  (let ((prime-list '(2 3 5 7 11 13 17 19 23)))
    (dotimes (i 3)
      (dotimes (j 3)
        (setf (gethash (nth (+ (* i 3) j) prime-list) *move-hash*) (list i j)))))
  (maphash (lambda (k v) (format t "~%~D = ~D" k v)) *move-hash*)
  (values))

(defun reset-game ()
  (dotimes (i 3)
    (dotimes (j 3)
      (setf (aref *board* i j) nil (aref *A-board* i j) 0 (aref *B-board* i j) 0))))


#|
So what are the winning combinations
(* 2 3 5) (* 7 11 13) (* 17 19 23) etc
|#


(defun legal-move (i j)
  (not (aref *board* i j)))

(defun evaluate-all-rows (P row winc)
  (let ((pprod 1))
    (dotimes (j 3)
      (setf pprod (* pprod (aref P row j))))
    (unless (= 0 pprod ) (push pprod winc))
    #+nil(format t "~%~D" pprod))
  winc)

(defun evaluate-all-cols (P col winc)
  (let ((pprod 1))
    (dotimes (j 3)
      (setf pprod (* pprod (aref P j col))))
    (unless (= 0 pprod ) (push pprod winc))
    #+nil(format t "~%~D" pprod))
  winc)
  

(defun evaluate-diagonals (P count winc)
  (let ((pprod 1))
    (dotimes (j count)
      (setf pprod (* pprod (aref P j j))))
    (unless (= 0 pprod ) (push pprod winc))
    #+nil(format t "~%~D" pprod))
  winc)

(defun evaluate-diagonals-opposite (P count winc)
  (let ((pprod 1))
    (dotimes (j count)
      (setf pprod (* pprod (aref P j (- count j 1)))))
    (unless (= 0 pprod ) (push pprod winc))
    #+nil(format t "~%~D" pprod))
  winc)

(defun evaluate-all (P winc)
  (dotimes (i 3)
    (setf winc (evaluate-all-rows P i winc)))
  (dotimes (i 3)
    (setf winc (evaluate-all-cols P i winc)))
  (setf winc (evaluate-diagonals P 3 winc))
  (setf winc (evaluate-diagonals-opposite P 3 winc))
  winc)

(defun print-board (B)
  (format t "~%+---+---+---+")
  (dotimes (i 3)
    (format t "~%|")
    (dotimes (j 3)
      (format t "~3:@<~A~>|" (if (aref B i j) (aref B i j) (format nil ""))))
    (format t "~%+---+---+---+"))
  (values))


(defun evaluate-as-win (B row col)
  (let ((win-list nil)
        (temp-prime (make-array '(3 3) :initial-element 0)))
  (dotimes (i 3)
    (dotimes (j 3)
      (setf (aref temp-prime i j) (* (aref B i j) (aref *prime-array* i j)))))
  (setf (aref temp-prime row col) (aref *prime-array* row col))
  #|
  (format t "~%% EVALUATION PRINT BOARD ~%~%")
  (print-board B)
  (print-board temp-prime)
  |#
  (setf win-list (evaluate-all temp-prime win-list))
  (if (and (> (length win-list) 0) (intersection win-list *winning-combinations*)) t nil)
  #|
  (format t "~%At this point win list = ~A" win-list)
  |#
  ))

(defun has-won (B)
  "Check to see if Board has won the game"
  (let ((win-list nil)
        (temp-prime (make-array '(3 3) :initial-element 0)))
  (dotimes (i 3)
    (dotimes (j 3)
      (setf (aref temp-prime i j) (* (aref B i j) (aref *prime-array* i j)))))
  (setf win-list (evaluate-all temp-prime win-list))
  (if (and (> (length win-list) 0) (intersection win-list *winning-combinations*)) t nil)))

(defun will-win (B i j)
  "Will I win if I play next move as position"
  (if (and (legal-move i j) (evaluate-as-win B i j)) t nil))

(defun play-random-move (print-move)
  (let ((choice-move nil))
    (dotimes (i 3)
      (dotimes (j 3)
        (when (legal-move i j) (push (aref *prime-array* i j) choice-move))))
    (when print-move (format t "~%Legal move(s) are ~A" choice-move))
    (nth (random (length choice-move)) choice-move)))

(defun calculate-board-help (B other-board print-move)
  "Return a valid move, winning and non-losing"
  (let ((winning-move nil)
        (blocking-move nil))
  (dotimes (i 3)
    (dotimes (j 3)
      (when (will-win B i j) (push (aref *prime-array* i j) winning-move))
      (when (will-win other-board i j) (push (aref *prime-array* i j) blocking-move))))
  ;(format t "~%Score ~D ~D" (length winning-move) (length blocking-move))
  (cond
   ((> (length blocking-move) 1)
    (format t "~%Resign")
    (first blocking-move))
   ((= (length blocking-move) 1)
    (first blocking-move))
   ((> (length winning-move) 0)
    (nth (random (length winning-move)) winning-move))
   (t (play-random-move print-move)))))
  

(defun print-board-help (B other-board print-move)
  (format t " Play ~A : " (calculate-board-help B other-board t)))

(defun print-help (A-or-B)
  (if A-or-B (print-board-help *A-board* *B-board* t) (print-board-help *B-board* *A-board* t)))

(defun calculate-next-move (A-or-B)
  (if A-or-B (calculate-board-help *A-board* *B-board* nil)))

(defun play( computer )
  "Game loop, potentially playing against the computer"
  (let ((move-number 0)
        (a-won nil)
        (b-won nil))
    (do () ((or (> move-number 8) a-won b-won))
        (progn
          (when (and computer (evenp move-number)) (format t "~%Player ~S, enter move: " (if (evenp move-number) 'A 'B)))
          (let ((m (if (and computer (oddp move-number)) (calculate-next-move (oddp move-number)) (read)))) ;; the format of the move is '(0 0) or '(1 2)
            (when (eql m '?)
              (format t "~%Printing help for Player ~A" (if (evenp move-number) 'A 'B))
              (print-help (evenp move-number))
              (setf m (read))
              #+nil(format t "~%~A" m))
            (unless (listp m) (setf m (gethash m *move-hash*)))
            #+nil(format t "~%~A" m)
            (if (evenp move-number)
                (setf (aref *board* (first m) (second m)) 'A (aref *A-board* (first m) (second m)) 1)
              (setf (aref *board* (first m) (second m)) 'B (aref *B-board* (first m) (second m)) 1)))
          (when (and computer (oddp move-number)) (print-board *board*))
          (setf a-won (has-won *A-board*))
          (setf b-won (has-won *B-board*))
          #+nil(cond
           (a-won (format t "~%~% CONGRATULATION TO PLAYER-A ~%~%"))
           (b-won (format t "~%~% CONGRATULATION TO PLAYER-B ~%~%")))
          #+nil(print-board *A-board*)
          #+nil(print-board *B-board*)
          (incf move-number)))
    (cond
     ((has-won *A-board*) (format t "~%~% CONGRATULATION TO PLAYER-A ~%~%"))
     ((has-won *B-board*) (format t "~%~% CONGRATULATION TO PLAYER-B ~%~%"))
     (t (format t "~%~%Match ended in draw~%~%")))))

(defun play-new-game ( computer )
  (reset-game)
  (play t))


(defun main()
  (format t "~%Welcome to Tic-Tac-Toe by Sandeep Koranne~%")  
  (initialize-boards)
  (setf *winning-combinations* (evaluate-all *prime-array* *winning-combinations*))
  (format t "~%Winning combinations calculated to be ~A~%" *winning-combinations*)
  (format t "~%Game board~%")
  (print-board *board*)
  (format t "~%Player-A board~%")
  (print-board *A-board*)
  (format t "~%Player-B board~%")
  (print-board *B-board*)
  (format t "~%Evaluation board~%")
  (print-board *prime-array*)
  (play-new-game t)
  (do () ((yes-or-no-p "Quit?")) (play-new-game t))
  (format t "~%Bye~%")
  (values))
  

(main)
(quit)

