;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File   : lab2.lisp
;; Author : Sandeep Koranne
;;
;; Purpose: Example for Common Lisp code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
This is a block comment.
And can be used for general documentation.
|#

#+clisp
(format t "~%Recognized Common Lisp code block.~%")

; Write a function to add 2 numbers
(defun add-two-numbers (a b) (+ a b))

; write a function to add 5 to the argument and return the value
(defun add-five (a)
  (let ((val 5))
    (+ a val)))

(setf *global-val* 10)
(defun add-ten (a) (+ a *global-val*))


(let ((closure-val *global-val*))
  (defun closure-add-ten (a) (+ a closure-val)))

(setf *global-val* 20)


(defun hanoi-move (n from to via)
  (cond ((= n 1)
         (format t "Move from ~A to ~A.~%" from to))
        (t
         (hanoi-move (- n 1) from via to)
         (format t "Move from ~A to ~A.~%" from to)
         (hanoi-move (- n 1) via to from))))

(defun count-collatz (n till-now)
  ;(format t "~%CC ~D = ~D" n till-now)
  (cond
   ((= n 1) (+ 1 till-now))
   ((= n 0) (+ 1 till-now))
   ((evenp n) (count-collatz (/ n 2) (+ 1 till-now)))
   ((oddp n) ( count-collatz (+ 1 (* n 3)) (+ 1 till-now)))))

(defun print-collatz (n)
  (format t "~%~D = ~D" n (count-collatz n 0)))


(defun main()
  (format t "~%Begin Main()~%")
  (format t "~%(add-two-numbers 2 3) = ~D~%" (add-two-numbers 2 3))
  (format t "~%(add-five 10) = ~D~%" (add-five 10))
  (format t "~%(add-ten 10) = ~D~%" (add-ten 10))
  (format t "~%(closure-add-ten 10) = ~D~%" (closure-add-ten 10))
  (format t "~%Move 4 disks from 1 to 3~%")
  (hanoi-move 4 1 2 3)
  (dotimes (i 21) 
    ;(format t "~%Printing Collatz for ~D" i)
    (print-collatz i))
  (values))


(main)
