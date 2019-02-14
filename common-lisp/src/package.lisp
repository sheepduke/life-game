(defpackage life-game
  (:use :cl
        :iterate
        :optima)
  (:export
   :board-size
   :make-board
   :place-random-seeds
   :print-board
   :evolve))
(in-package :life-game)
