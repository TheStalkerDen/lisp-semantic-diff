(defsystem "diff-backend"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:anaphora :alexandria :str :cl-json :trivia)
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "lexer" :depends-on ("utils"))
                 (:file "parser" :depends-on ("lexer"))
                 (:file "nodes"  :depends-on ("utils"))
                 (:file "abstract-sem-tree-generator" :depends-on ("lexer" "nodes"))
                 (:file "comparator" :depends-on ("nodes"))
                 (:file "main" :depends-on ("lexer" "parser" "abstract-sem-tree-generator" "comparator")))))
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
