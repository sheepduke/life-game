(defpackage life-game
  (:use :cl
        :iterate
        :optima)
  (:export :board-size
           :make-board
           :print-board
           :evolve))
(in-package :life-game)
