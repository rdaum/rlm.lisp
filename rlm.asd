(defsystem "rlm"
  :version "0.1.0"
  :description "Recursive Language Model - agentic REPL loop in Common Lisp"
  :depends-on ("dexador" "yason" "bordeaux-threads" "alexandria" "cl-ppcre")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "client")
                             (:file "sandbox")
                             (:file "repl")
                             (:file "prompts")
                             (:file "tools")
                             (:file "rlm")
                             (:file "cli")))))

(defsystem "rlm/mode"
  :version "0.1.0"
  :description "RLM integration for the Lem editor"
  :depends-on ("rlm" "lem/core" "lem-markdown-mode" "lem-legit" "lem-lsp-mode")
  :serial t
  :components ((:module "mode"
                :components ((:file "package")
                             (:file "rlm-tools")
                             (:file "rlm-legit-tools")
                             (:file "rlm-lsp-tools")
                             (:file "rlm-mode")))))

(defsystem "rlm/mode-tests"
  :version "0.1.0"
  :description "Tests for the RLM Lem mode"
  :depends-on ("rlm/mode" "rove" "lem-fake-interface")
  :serial t
  :components ((:module "tests"
                :components ((:file "legit-tools"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
