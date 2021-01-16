(defsystem "diff-backend"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:anaphora :alexandria :str :cl-json :iter)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "diff-backend/tests"))))

(defsystem "diff-backend/tests"
  :author ""
  :license ""
  :depends-on ("diff-backend"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for diff-backend"
  :perform (test-op (op c) (symbol-call :rove :run c)))
