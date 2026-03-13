(defpackage :rlm/client
  (:use :cl)
  (:export #:*api-key*
           #:*api-base-url*
           #:*model*
           #:chat-completion
           #:completion-content
           #:completion-usage
           #:list-models))

(defpackage :rlm/sandbox
  (:use :cl)
  (:export #:make-sandbox
           #:sandbox-eval
           #:sandbox-result
           #:sandbox-result-stdout
           #:sandbox-result-stderr
           #:sandbox-result-values
           #:sandbox-result-error
           #:sandbox-result-final-answer
           #:inject-function
           #:set-variable
           #:sandbox-variables
           #:sandbox-user-package
           #:sandbox-injected-functions))

(defpackage :rlm/repl
  (:use :cl)
  (:export #:make-environment
           #:environment-set-context
           #:environment-clear-final
           #:environment-execute
           #:environment-variables
           #:environment-tools
           #:environment-add-history
           #:environment-history
           #:register-tool
           #:make-tool
           #:tool
           #:tool-name
           #:tool-description
           #:tool-parameter-doc
           #:tool-execute-fn
           #:exec-result
           #:exec-result-stdout
           #:exec-result-stderr
           #:exec-result-error
           #:exec-result-final-answer
           #:exec-result-variables))

(defpackage :rlm/prompts
  (:use :cl)
  (:export #:*system-prompt-base*
           #:format-tools-section
           #:build-system-message
           #:format-conversation-history
           #:build-context-metadata-message
           #:build-iteration-zero-message
           #:build-iteration-continue-message
           #:build-fallback-message))

(defpackage :rlm/tools
  (:use :cl)
  (:export #:make-list-directory-tool
           #:make-read-file-tool
           #:make-web-read-tool
           #:make-web-search-tool
           #:*jina-api-key*))

(defpackage :rlm/cli
  (:use :cl)
  (:export #:main
           #:default-tools
           #:*prompt*
           #:*default-max-iterations*
           #:*tools*
           #:*context*))

(defpackage :rlm
  (:use :cl)
  (:export #:query
           #:make-llm-query-fn
           #:*max-iterations*
           #:*max-output-chars*
           #:*verbose*
           #:query-result
           #:query-result-answer
           #:query-result-iterations
           #:query-result-token-usage
           #:query-result-environment))
