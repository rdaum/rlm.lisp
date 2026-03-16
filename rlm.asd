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
  :depends-on ("rlm" "lem/core" "lem-markdown-mode" "lem-legit")
  :serial t
  :components ((:module "mode"
                :components ((:file "package")
                             (:file "rlm-tools")
                             (:file "rlm-legit-tools")
                             (:file "rlm-mode")))))
