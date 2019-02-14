#|
  This file is a part of life-game project.
|#

(defsystem "life-game"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:alexandria
               :iterate
               :optima)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "life-game"))))
  :description ""
  :in-order-to ((test-op (test-op "life-game-test"))))
