(setf *random-state* (make-random-state t))

(defun makeTable(w h)
 (let ((table (make-array `(,(* w h)))))
 (loop :for i :from 0 :below (- (* w h) 1) :do 
       (setf (aref table i) (+ i 1)))
 (return-from makeTable table)))

(defun makeShuffledTable(w h)
  (let ((arr (makeTable w h)) (len (- (* w h) 1)))
  (loop :for i :from (- len 1) :downto 0 :do
        (let ((randomIndex (random (+ i 1))) (currEl (+ (aref arr i) 0)))
        (setf (aref arr i) (aref arr randomIndex) 
               (aref arr randomIndex) currEl))) (return-from makeShuffledTable arr)))

(defun printTable(table w h)
  (loop :for i :from 0 :below h :do
        (loop :for j :from 0 :below w :do 
              (format t "~A " (aref table (+ (* i w) j)))) (fresh-line)))

(defun findEmptySpaceIndex(table)
  (loop :for i :from (- (length table) 1) :downto 0 do
        (if (= (aref table i) 0) (return i) ())))

(defun swapZero(table zeroI newI) (setf (aref table zeroI) (aref table newI)
                                        (aref table newI) 0))
(defun moveLeft(table w h)
  (let ((i (findEmptySpaceIndex table)))
    (swapZero table i (- i 1))
    (return-from moveLeft table)))

(defun moveRight(table w h)
  (let ((i (findEmptySpaceIndex table)))
    (swapZero table i (+ i 1)) 
    (return-from moveRight table)))

(defun iToX(i w) (mod i w))
(defun iToY(i h) (floor (/ i h)))

;;; Get Y from I, subtract 1, and turn Y to I back
(defun iToUp(i w h) (+ (* (- (iToY i h) 1) h) (iToX i w)))
;;; Get Y from I, add 1, and turn Y to I back
(defun iToDown(i w h) (+ (* (+ (iToY i h) 1) h) (iToX i w)))

(defun moveUp(table w h)
   (let ((i (findEmptySpaceIndex table)))
   (let ((newI (iToUp i w h)))
     (swapZero table i newI)
     (return-from moveUp table))))

(defun moveDown(table w h)
   (let ((i (findEmptySpaceIndex table)))
   (let ((newI (iToDown i w h)))
     (swapZero table i newI)
     (return-from moveDown table))))


;;;; TUI
(ql:quickload "trivial-raw-io")

(defun noMove(table w h) ())

(defun getMovementInput()
  (let ((ch (trivial-raw-io:read-char)))
  (return-from getMovementInput 
        (cond ((eql ch #\a) #'moveLeft)
        ((eql ch #\d) #'moveRight)
        ((eql ch #\w) #'moveUp)
        ((eql ch #\s) #'moveDown)
        (t #'noMove)))))

(defun promptMoveAndPrint(table w h) 
  (funcall (getMovementInput) table w h)
  (printTable table w h))

(defun promptLoop(table w h) (printTable table w h) (terpri) (loop (promptMoveAndPrint table w h) (terpri)))

(defun Play(w h) (promptLoop (makeShuffledTable w h) w h))
