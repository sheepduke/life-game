(in-package :life-game)

(defun make-empty-board (size)
  "Return an empty board of given SIZE."
  (declare (type fixnum size))
  (the (simple-array boolean 2)
       (make-array (list size size)
                   :element-type 'boolean
                   :initial-element nil
                   :adjustable nil)))

(defun make-board (size seed-count)
  "Return a BOARD object of given SIZE."
  (let* ((board (make-empty-board size))
         (range (* size size))
         (state (make-random-state t)))
    (iterate (while (> seed-count 0))
             (let* ((position (random range state))
                    (row (floor (/ position size)))
                    (col (rem position size)))
               (unless (board-cell board row col)
                 (setf (board-cell board row col) t)
                 (decf seed-count))))
    board))

(declaim (inline board-size))
(defun board-size (board)
  (declare (type (simple-array boolean 2) board))
  (array-dimension board 0))

(declaim (inline board-cell-valid-p))
(defun board-cell-valid-p (board row col)
  "Return T if given cell [ROW, COL] is valid."
  (declare (type fixnum row col)
           (type (simple-array boolean 2) board))
  (and (>= row 0) (< row (board-size board))
       (>= col 0) (< col (board-size board))))

(defun board-cell (board row col)
  "Return the cell of BOARD specified by ROW and COL."
  (declare (type (simple-array boolean 2) board)
           (type fixnum row col))
  (and (board-cell-valid-p board row col)
       (aref board row col)))

(defun (setf board-cell) (value board row col)
  (and (board-cell-valid-p board row col)
       (setf (aref board row col) value)))

(defun print-board (board &optional (stream t))
  "Method for printing board"
  (iter (for row in-vector board)
        (iter (for cell in-vector row)
              (format stream "~A " (if cell #\# #\-)))
        (format stream "~&")))

(defun board-cell-alive-p (board row col)
  "Return T if given cell shall remain alive or reborn."
  (declare (type fixnum row col)
           (type (simple-array boolean 2) board))
  (let ((neighbour-count (count t (list (board-cell board (1- row) (1- col))
                                        (board-cell board (1- row) col)
                                        (board-cell board (1- row) (1+ col))
                                        (board-cell board row (1- col))
                                        (board-cell board row (1+ col))
                                        (board-cell board (1+ row) (1- col))
                                        (board-cell board (1+ row) col)
                                        (board-cell board (1+ row) (1+ col))))))
    (or (= neighbour-count 3)
        (and (= neighbour-count 2)
             (board-cell board row col)))))

(defun evolve (board)
  "Evolve to the next turn."
  (declare (optimize (speed 3) (safety 0)))
  (let* ((size (board-size board))
         (new-board (make-empty-board size)))
    (iter (for row from 0 below size)
          (iter (for col from 0 below size)
                (setf (board-cell new-board row col)
                      (board-cell-alive-p board row col))))
    new-board))
