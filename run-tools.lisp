(asdf:load-system "rlm")

(setf rlm/client:*model* "openrouter/hunter-alpha")
(setf rlm:*verbose* t)

(let* ((project-dir #P"/home/ryan/rlm/")
       (context "You have access to a project directory via the list-directory and read-file tools. Use them to explore.")
       (question "List the project directory, read all the source files, then give a TLDR description of what this project is and how it works.")
       (result (rlm:query context question
                         :max-iterations 15
                         :tools (list (rlm/tools:make-list-directory-tool
                                       :allowed-dirs (list project-dir))
                                      (rlm/tools:make-read-file-tool
                                       :allowed-dirs (list project-dir))))))
  (format t "~%~%========================================~%")
  (format t "Answer: ~A~%" (rlm:query-result-answer result))
  (format t "Iterations: ~A~%" (rlm:query-result-iterations result))
  (format t "Token usage: ~S~%" (rlm:query-result-token-usage result)))
