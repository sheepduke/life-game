#|
  This file is a part of life-game project.
|#

(defsystem "life-game-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("life-game"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "life-game"))))
  :description "Test system for life-game"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
