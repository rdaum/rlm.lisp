(asdf:load-system "rlm")

;; Test code block extraction with realistic content
(let ((text (format nil "I'll explore the context first.~%~%```lisp~%(print context)~%```~%~%Let me check.")))
  (let ((blocks (rlm::extract-code-blocks text)))
    (format t "Blocks from realistic: ~S~%" blocks)
    (assert (equal blocks '("(print context)")))))

;; Original tests
(let ((text (format nil "Some text~%```lisp~%(+ 1 2)~%```~%More text~%```lisp~%(print \"hello\")~%```")))
  (assert (equal (rlm::extract-code-blocks text) '("(+ 1 2)" "(print \"hello\")"))))
(format t "PASS: code block extraction~%")

;; All other tests from before
(assert (equal (rlm::find-final-answer (format nil "blah~%FINAL(42)~%blah")) "42"))
(assert (null (rlm::find-final-answer "no final here")))
(format t "PASS: find-final-answer~%")

(let ((env (rlm/repl:make-environment)))
  (rlm/repl:environment-set-context env "The capital of France is Paris.")
  (let ((r (rlm/repl:environment-execute env "(final (subseq context 25))")))
    (assert (equal (rlm/repl:exec-result-final-answer r) "Paris."))))
(format t "PASS: final~%")

(let ((env (rlm/repl:make-environment)))
  (rlm/repl:environment-set-context env "hello world")
  (rlm/repl:environment-execute env "(defvar *result* (string-upcase context))")
  (let ((r (rlm/repl:environment-execute env "(final-var \"*result*\")")))
    (assert (equal (rlm/repl:exec-result-final-answer r) "HELLO WORLD"))))
(format t "PASS: final-var~%")

(let ((env (rlm/repl:make-environment)))
  (let ((r (rlm/repl:environment-execute env "(cl:open \"/etc/passwd\")")))
    (assert (rlm/repl:exec-result-error r))))
(format t "PASS: safety~%")

(let ((env (rlm/repl:make-environment
            :llm-query-fn (lambda (prompt)
                            (format nil "Mock: ~A" (subseq prompt 0 (min 20 (length prompt))))))))
  (rlm/repl:environment-set-context env "test")
  (rlm/repl:environment-execute env "(defvar *a* (llm-query \"hello world\"))")
  (let ((r (rlm/repl:environment-execute env "(print *a*)")))
    (assert (search "Mock:" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: llm-query~%")

;; Test tool registration and call-tool
(let ((env (rlm/repl:make-environment)))
  ;; Register a simple echo tool
  (rlm/repl:register-tool env
    (rlm/repl:make-tool
     :name "echo"
     :description "Echo back the input"
     :parameter-doc "(call-tool \"echo\" message)"
     :execute-fn (lambda (msg) (format nil "ECHO: ~A" msg))))
  ;; Call it from sandbox code
  (let ((r (rlm/repl:environment-execute env "(defvar *out* (call-tool \"echo\" \"hello\"))")))
    (assert (null (rlm/repl:exec-result-error r))))
  (let ((r (rlm/repl:environment-execute env "(print *out*)")))
    (assert (search "ECHO: hello" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: call-tool~%")

;; Test list-tools
(let ((env (rlm/repl:make-environment)))
  (rlm/repl:register-tool env
    (rlm/repl:make-tool :name "foo" :description "Does foo" :execute-fn (lambda () "foo")))
  (let ((r (rlm/repl:environment-execute env "(print (list-tools))")))
    (assert (search "foo" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: list-tools~%")

;; Test nil and t work in sandbox (regression: NIL wasn't imported due to when-guard)
(let ((env (rlm/repl:make-environment)))
  (let ((r (rlm/repl:environment-execute env "(defvar *x* nil)")))
    (assert (null (rlm/repl:exec-result-error r))))
  (let ((r (rlm/repl:environment-execute env "(defvar *y* (if t 42 0))")))
    (assert (null (rlm/repl:exec-result-error r))))
  (let ((r (rlm/repl:environment-execute env "(print *y*)")))
    (assert (search "42" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: nil and t in sandbox~%")

;; Test unknown tool error
(let ((env (rlm/repl:make-environment)))
  (let ((r (rlm/repl:environment-execute env "(call-tool \"nonexistent\")")))
    (assert (rlm/repl:exec-result-error r))
    (assert (search "Unknown tool" (rlm/repl:exec-result-error r)))))
(format t "PASS: unknown tool error~%")

;; Test file tools (using /tmp for safety)
(ensure-directories-exist #P"/tmp/rlm-test/")
(with-open-file (out "/tmp/rlm-test/hello.txt" :direction :output :if-exists :supersede)
  (write-string "Hello from file!" out))
(let ((env (rlm/repl:make-environment)))
  (rlm/repl:register-tool env (rlm/tools:make-read-file-tool :allowed-dirs '(#P"/tmp/rlm-test/")))
  (rlm/repl:register-tool env (rlm/tools:make-list-directory-tool :allowed-dirs '(#P"/tmp/rlm-test/")))
  ;; Read file
  (let ((r (rlm/repl:environment-execute env "(defvar *content* (call-tool \"read-file\" \"/tmp/rlm-test/hello.txt\"))")))
    (assert (null (rlm/repl:exec-result-error r))))
  (let ((r (rlm/repl:environment-execute env "(print *content*)")))
    (assert (search "Hello from file!" (rlm/repl:exec-result-stdout r))))
  ;; List directory
  (let ((r (rlm/repl:environment-execute env "(print (call-tool \"list-directory\" \"/tmp/rlm-test/\"))")))
    (assert (search "hello.txt" (rlm/repl:exec-result-stdout r))))
  ;; Access denied outside allowed dirs
  (let ((r (rlm/repl:environment-execute env "(call-tool \"read-file\" \"/etc/passwd\")")))
    (assert (rlm/repl:exec-result-error r))
    (assert (search "Access denied" (rlm/repl:exec-result-error r)))))
(format t "PASS: file tools~%")

;; Test text processing functions
(let ((env (rlm/repl:make-environment)))
  ;; split-string
  (let ((r (rlm/repl:environment-execute env "(defvar *s* (split-string \"a,b,c\" \",\"))")))
    (assert (null (rlm/repl:exec-result-error r))))
  (let ((r (rlm/repl:environment-execute env "(print *s*)")))
    (assert (search "a" (rlm/repl:exec-result-stdout r)))
    (assert (search "b" (rlm/repl:exec-result-stdout r)))
    (assert (search "c" (rlm/repl:exec-result-stdout r))))
  ;; split-string default delimiter (whitespace)
  (let ((r (rlm/repl:environment-execute env "(print (split-string \"hello  world\"))")))
    (assert (search "hello" (rlm/repl:exec-result-stdout r)))
    (assert (search "world" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: split-string~%")

(let ((env (rlm/repl:make-environment)))
  ;; regex-match
  (let ((r (rlm/repl:environment-execute env "(print (regex-match \"(\\\\d+)-(\\\\d+)\" \"abc 123-456 def\"))")))
    (assert (search "123-456" (rlm/repl:exec-result-stdout r)))
    (assert (search "123" (rlm/repl:exec-result-stdout r)))
    (assert (search "456" (rlm/repl:exec-result-stdout r))))
  ;; regex-match no match
  (let ((r (rlm/repl:environment-execute env "(print (regex-match \"zzz\" \"abc\"))")))
    (assert (search "NIL" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: regex-match~%")

(let ((env (rlm/repl:make-environment)))
  ;; regex-scan
  (let ((r (rlm/repl:environment-execute env "(print (regex-scan \"\\\\d+\" \"abc 123 def 456\"))")))
    (assert (search "123" (rlm/repl:exec-result-stdout r)))
    (assert (search "456" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: regex-scan~%")

(let ((env (rlm/repl:make-environment)))
  ;; regex-replace
  (let ((r (rlm/repl:environment-execute env "(print (regex-replace \"\\\\d+\" \"a1b2c3\" \"X\"))")))
    (assert (search "aXbXcX" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: regex-replace~%")

(let ((env (rlm/repl:make-environment)))
  ;; words
  (let ((r (rlm/repl:environment-execute env "(print (words \"  hello   world  \"))")))
    (assert (search "hello" (rlm/repl:exec-result-stdout r)))
    (assert (search "world" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: words~%")

(let ((env (rlm/repl:make-environment)))
  ;; lines
  (let ((r (rlm/repl:environment-execute env "(defvar *l* (lines (format nil \"a~%b~%c\")))")))
    (assert (null (rlm/repl:exec-result-error r))))
  (let ((r (rlm/repl:environment-execute env "(print (length *l*))")))
    (assert (search "3" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: lines~%")

(let ((env (rlm/repl:make-environment)))
  ;; join-strings
  (let ((r (rlm/repl:environment-execute env "(print (join-strings '(\"a\" \"b\" \"c\") \",\"))")))
    (assert (search "a,b,c" (rlm/repl:exec-result-stdout r))))
  ;; join-strings default separator
  (let ((r (rlm/repl:environment-execute env "(print (join-strings '(\"hello\" \"world\")))")))
    (assert (search "hello world" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: join-strings~%")

(let ((env (rlm/repl:make-environment)))
  ;; word-frequency
  (let ((r (rlm/repl:environment-execute env "(defvar *wf* (word-frequency \"the cat sat on the mat\"))")))
    (assert (null (rlm/repl:exec-result-error r))))
  (let ((r (rlm/repl:environment-execute env "(print *wf*)")))
    (assert (search "the" (rlm/repl:exec-result-stdout r)))
    (assert (search "2" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: word-frequency~%")

;; Test that final-answer is properly cleared between queries
(let ((env (rlm/repl:make-environment)))
  ;; Set a final answer
  (let ((r (rlm/repl:environment-execute env "(final \"first answer\")")))
    (assert (equal (rlm/repl:exec-result-final-answer r) "first answer")))
  ;; Clear it
  (rlm/repl:environment-clear-final env)
  ;; Execute something unrelated — final-answer must NOT resurrect
  (let ((r (rlm/repl:environment-execute env "(+ 1 2)")))
    (assert (null (rlm/repl:exec-result-final-answer r))
            () "final-answer resurrected after clear!"))
  ;; *last-answer* should hold the old value
  (let ((r (rlm/repl:environment-execute env "(print *last-answer*)")))
    (assert (search "first answer" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: final-answer clear + *last-answer*~%")

;; Test *history* variable in sandbox
(let ((env (rlm/repl:make-environment)))
  ;; Simulate two completed queries
  (rlm/repl:environment-execute env "(final \"answer one\")")
  (rlm/repl:environment-add-history env "question one" "answer one")
  (rlm/repl:environment-clear-final env)
  (rlm/repl:environment-execute env "(final \"answer two\")")
  (rlm/repl:environment-add-history env "question two" "answer two")
  (rlm/repl:environment-clear-final env)
  ;; *history* should have both entries
  (let ((r (rlm/repl:environment-execute env "(print (length *history*))")))
    (assert (search "2" (rlm/repl:exec-result-stdout r))))
  ;; First entry question accessible
  (let ((r (rlm/repl:environment-execute env "(print (getf (first *history*) :question))")))
    (assert (search "question one" (rlm/repl:exec-result-stdout r))))
  ;; Can get answer from second entry
  (let ((r (rlm/repl:environment-execute env "(print (getf (second *history*) :answer))")))
    (assert (search "answer two" (rlm/repl:exec-result-stdout r))))
  ;; *last-answer* is the most recent
  (let ((r (rlm/repl:environment-execute env "(print *last-answer*)")))
    (assert (search "answer two" (rlm/repl:exec-result-stdout r)))))
(format t "PASS: *history* variable~%")

;; Test conversation history
(let ((env (rlm/repl:make-environment)))
  ;; Initially empty
  (assert (null (rlm/repl:environment-history env)))
  ;; Add entries
  (rlm/repl:environment-add-history env "What is 2+2?" "4")
  (rlm/repl:environment-add-history env "And 3+3?" "6")
  (let ((hist (rlm/repl:environment-history env)))
    (assert (= 2 (length hist)))
    (assert (equal (getf (first hist) :question) "What is 2+2?"))
    (assert (equal (getf (first hist) :answer) "4"))
    (assert (equal (getf (second hist) :question) "And 3+3?"))
    (assert (equal (getf (second hist) :answer) "6"))))
(format t "PASS: conversation history~%")

;; Test history formatting — legacy cons format
(let ((msgs (rlm/prompts:format-conversation-history
             '(("What is Lisp?" . "A programming language.")
               ("Why use it?" . "Homoiconicity and macros.")))))
  (assert (= 4 (length msgs)))
  (assert (equal (getf (first msgs) :role) "user"))
  (assert (search "What is Lisp?" (getf (first msgs) :content)))
  (assert (equal (getf (second msgs) :role) "assistant"))
  (assert (search "A programming language." (getf (second msgs) :content)))
  (assert (search "Why use it?" (getf (third msgs) :content))))
;; Test history formatting — new plist format with work-log
(let ((msgs (rlm/prompts:format-conversation-history
             (list (list :question "What is this?" :answer "A scheduler."
                         :work-log "Iteration 1: Read file structure. Found Scheduler struct.")))))
  (assert (= 2 (length msgs)))
  (assert (search "What is this?" (getf (first msgs) :content)))
  ;; Work log should appear in the assistant message
  (assert (search "Read file structure" (getf (second msgs) :content)))
  (assert (search "A scheduler." (getf (second msgs) :content))))
;; Empty history returns nil
(assert (null (rlm/prompts:format-conversation-history nil)))
(format t "PASS: history formatting~%")

;; Test trailing close-paren tolerance in sandbox
(let* ((env (rlm/repl:make-environment :llm-query-fn (lambda (p) (declare (ignore p)) "test")))
       ;; Extra close-paren after valid form — should still work
       (result (rlm/repl:environment-execute env "(format t \"~A\" (+ 1 2)))")))
  (assert (null (rlm/repl:exec-result-error result))
          nil "Trailing paren should not cause an error")
  (assert (search "3" (rlm/repl:exec-result-stdout result))))
;; But a leading unmatched paren with no valid forms should still error
(let* ((env (rlm/repl:make-environment :llm-query-fn (lambda (p) (declare (ignore p)) "test")))
       (result (rlm/repl:environment-execute env ")(")))
  (assert (rlm/repl:exec-result-error result)
          nil "Leading junk with no valid forms should error"))
(format t "PASS: trailing paren tolerance~%")

(format t "~%All tests passed!~%")
