(defsystem "diff-backend"
  :version "0.1.0"
  :author "Denys Yermolenko"
  :license ""
  :depends-on (:anaphora :alexandria :str :cl-json :trivia)
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "lexer" :depends-on ("utils"))
                 (:file "parser" :depends-on ("lexer"))
                 (:file "nodes"  :depends-on ("utils"))
                 (:file "statistics")
                 (:file "abstract-sem-tree-generator" :depends-on ("lexer" "nodes"))
                 (:file "comparator" :depends-on ("nodes"))
                 (:file "results-generator" :depends-on ("nodes"))
                 (:file "main" :depends-on ("lexer" "parser" "abstract-sem-tree-generator" "comparator")))))
  :description ""
  :in-order-to ((test-op (test-op "diff-backend/tests"))))

(defsystem "diff-backend/tests"
  :author "Denys Yermolenko"
  :license ""
  :depends-on ("diff-backend"
               "closer-mop"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "test-utils")
                 (:file "test-engines")
                 (:file "main")
                 (:file "lexer")
                 (:file "parser")
                 (:file "abstract-sem-tree-generator")
                 (:file "statistics")
                 (:file "comparator"))))
  :description "Test system for diff-backend"
  :perform (test-op :before (o c) (format t "Start testing~%"))
  :perform (test-op (o c) (symbol-call :rove :run c :style :dot))
  :perform (test-op :after (o c) (format t "End testing~%")))

