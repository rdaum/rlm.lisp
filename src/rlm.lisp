(in-package :rlm)

;;; RLM — the agent loop

(defvar *max-iterations* 20
  "Maximum number of REPL iterations before forcing a final answer.")

(defvar *max-output-chars* 20000
  "Maximum characters of REPL output to feed back to the LLM.")

(defvar *verbose* t
  "When true, print iteration progress to *standard-output*.")

;;; ============================================================
;;; Code block extraction
;;; ============================================================

(defvar *code-block-scanner*
  (cl-ppcre:create-scanner "```lisp\\s*\\n(.*?)\\n\\s*```"
                           :single-line-mode t)
  "Pre-compiled scanner for extracting ```lisp code blocks.")

(defun extract-code-blocks (text)
  "Extract code from ```lisp fenced blocks in TEXT. Returns list of code strings."
  (let ((results '())
        (start 0))
    (loop
      (multiple-value-bind (match-start match-end reg-starts reg-ends)
          (cl-ppcre:scan *code-block-scanner* text :start start)
        (unless match-start (return))
        (let ((code (subseq text (aref reg-starts 0) (aref reg-ends 0))))
          (push (string-trim '(#\Space #\Tab #\Newline #\Return) code) results))
        (setf start match-end)))
    (nreverse results)))

(defun find-final-answer (text)
  "Find bare FINAL(...) in response text outside code blocks.
Returns the answer string or NIL."
  ;; Strip code blocks first
  (let* ((strip-scanner (cl-ppcre:create-scanner "```lisp\\s*\\n.*?\\n```"
                                                  :single-line-mode t))
         (stripped (cl-ppcre:regex-replace-all strip-scanner text ""))
         (final-scanner (cl-ppcre:create-scanner "^\\s*FINAL\\((.*)\\)\\s*$"
                                                  :multi-line-mode t)))
    ;; Look for FINAL(...)
    (multiple-value-bind (start end reg-starts reg-ends)
        (cl-ppcre:scan final-scanner stripped)
      (declare (ignore end))
      (when start
        (let ((content (subseq stripped (aref reg-starts 0) (aref reg-ends 0))))
          (return-from find-final-answer
            (string-trim '(#\Space #\Tab) content))))))
  nil)

;;; ============================================================
;;; Result type
;;; ============================================================

(defclass query-result ()
  ((answer :accessor query-result-answer :initarg :answer)
   (iterations :accessor query-result-iterations :initarg :iterations :initform 0)
   (token-usage :accessor query-result-token-usage :initarg :token-usage :initform nil)
   (environment :accessor query-result-environment :initarg :environment :initform nil)))

;;; ============================================================
;;; Formatting
;;; ============================================================

(defun truncate-output (text max-chars)
  "Truncate TEXT to MAX-CHARS, appending a note if truncated."
  (if (> (length text) max-chars)
      (format nil "~A~%... [truncated, ~A more chars]"
              (subseq text 0 max-chars)
              (- (length text) max-chars))
      text))

(defun format-code-echo (code output)
  "Format a code block execution for the message history."
  (format nil "Code executed:~%```lisp~%~A~%```~%~%REPL output:~%~A"
          code (truncate-output output *max-output-chars*)))

(defun format-exec-output (result)
  "Format an exec-result into a single output string."
  (with-output-to-string (s)
    (let ((stdout (rlm/repl:exec-result-stdout result))
          (stderr (rlm/repl:exec-result-stderr result))
          (err (rlm/repl:exec-result-error result))
          (vars (rlm/repl:exec-result-variables result)))
      (when (and stdout (plusp (length stdout)))
        (write-string stdout s))
      (when (and stderr (plusp (length stderr)))
        (format s "~&STDERR: ~A" stderr))
      (when err
        (format s "~&ERROR: ~A" err))
      (when vars
        (format s "~&REPL variables: ~{~A~^, ~}"
                (mapcar #'car vars))))))

;;; ============================================================
;;; Verbose output
;;; ============================================================

(defun verbose-iteration (n response)
  "Print iteration info when *verbose* is true."
  (when *verbose*
    (format t "~&~%━━━ Iteration ~A ━━━~%" n)
    (format t "~A~%" (if (> (length response) 500)
                         (format nil "~A..." (subseq response 0 500))
                         response))
    (force-output)))

(defun verbose-code (code output)
  "Print code execution info when *verbose* is true."
  (when *verbose*
    (format t "~&  ▶ Code: ~A~%" (if (> (length code) 200)
                                      (format nil "~A..." (subseq code 0 200))
                                      code))
    (format t "  ◀ Output: ~A~%" (if (> (length output) 300)
                                      (format nil "~A..." (subseq output 0 300))
                                      output))
    (force-output)))

(defun verbose-final (answer)
  "Print final answer when *verbose* is true."
  (when *verbose*
    (format t "~&~%━━━ FINAL ANSWER ━━━~%~A~%" answer)
    (force-output)))

;;; ============================================================
;;; The loop
;;; ============================================================

(defun make-llm-query-fn ()
  "Create a function that makes sub-LLM calls via the configured client."
  (lambda (prompt)
    (let ((response (rlm/client:chat-completion
                     (list (list :role "user" :content prompt)))))
      (rlm/client:completion-content response))))

(defun query (context question &key (max-iterations *max-iterations*) tools environment on-event cancel-flag)
  "Run the RLM loop: give the LLM a QUESTION about CONTEXT.
CONTEXT is a string or list of strings.
TOOLS is an optional list of rlm/repl:tool instances to register (ignored if ENVIRONMENT given).
ENVIRONMENT is an optional existing environment to reuse (preserves sandbox state).
ON-EVENT is an optional callback (lambda (event-type &rest args)) called during the loop.
  Event types:
    :thinking iteration-number
    :response iteration-number content
    :code code-string
    :output code-string output-string
    :error error-message
    :final answer-string
CANCEL-FLAG is an optional function — when it returns true, the loop aborts.
Returns a QUERY-RESULT."
  (let* ((env (or environment
                  (rlm/repl:make-environment :llm-query-fn (make-llm-query-fn))))
         (registered-tools (rlm/repl:environment-tools env))
         (session-vars (when environment
                         ;; Filter out internal vars
                         (remove-if (lambda (pair)
                                      (member (car pair) '("context" "final-answer")
                                              :test #'string=))
                                    (rlm/repl:environment-variables env))))
         (messages nil)
         (total-prompt-tokens 0)
         (total-completion-tokens 0))
    ;; Register tools only for fresh environments
    (unless environment
      (dolist (tool tools)
        (rlm/repl:register-tool env tool)
        (push tool registered-tools))
      (setf registered-tools (nreverse registered-tools)))
    ;; Clear final-answer from any prior query
    (rlm/repl:environment-clear-final env)
    ;; Build messages with tool info and session state
    (setf messages (list (rlm/prompts:build-system-message registered-tools session-vars)
                         (rlm/prompts:build-context-metadata-message context)))
    ;; Include conversation history from prior queries
    (let ((history (rlm/repl:environment-history env)))
      (let ((history-msgs (rlm/prompts:format-conversation-history history)))
        (when history-msgs
          (setf messages (append messages history-msgs))))
      (setf messages (append messages
                             (list (rlm/prompts:build-iteration-zero-message
                                    question :follow-up (not (null history)))))))
    ;; Load context into the REPL
    (rlm/repl:environment-set-context env context)

    (let ((work-log-parts nil))
    (flet ((notify (event-type &rest args)
             (when on-event (apply on-event event-type args)))
           (log-work (text)
             (push text work-log-parts)))
    (dotimes (i max-iterations)
      (when (and cancel-flag (funcall cancel-flag))
        (notify :final "[Cancelled by user]")
        (return-from query
          (make-instance 'query-result
                         :answer "[Cancelled by user]"
                         :iterations (1+ i)
                         :token-usage (list :prompt-tokens total-prompt-tokens
                                            :completion-tokens total-completion-tokens)
                         :environment env)))
      (when *verbose*
        (format t "~&~%⏳ Thinking (iteration ~A)..." (1+ i))
        (force-output))
      (notify :thinking (1+ i))
      (let* ((response (handler-case (rlm/client:chat-completion messages)
                         (error (e)
                           (when *verbose*
                             (format t " API error: ~A~%" e))
                           nil)))
             (content (when response
                        (handler-case (rlm/client:completion-content response)
                          (error (e)
                            (when *verbose*
                              (format t " Response parse error: ~A~%" e))
                            nil))))
             (usage (when response
                      (handler-case (rlm/client:completion-usage response)
                        (error () nil)))))
        (unless content
          ;; API error — return best effort
          (notify :error (format nil "API error on iteration ~A" (1+ i)))
          (return-from query
            (make-instance 'query-result
                           :answer "[API error - no response]"
                           :iterations (1+ i)
                           :token-usage (list :prompt-tokens total-prompt-tokens
                                              :completion-tokens total-completion-tokens)
                           :environment env)))
        ;; Track tokens
        (when usage
          (incf total-prompt-tokens (getf usage :prompt-tokens 0))
          (incf total-completion-tokens (getf usage :completion-tokens 0)))
        (verbose-iteration (1+ i) content)
        (notify :response (1+ i) content)
        ;; Log the reasoning (strip code blocks) for work history
        (let ((reasoning (cl-ppcre:regex-replace-all
                          "```[a-z]*\\s*\\n.*?\\n\\s*```"
                          content "")))
          (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) reasoning)))
            (when (plusp (length trimmed))
              (log-work (format nil "Iteration ~A reasoning: ~A"
                                (1+ i)
                                (if (> (length trimmed) 300)
                                    (format nil "~A..." (subseq trimmed 0 300))
                                    trimmed))))))

        (let ((code-blocks (extract-code-blocks content))
              (bare-final (find-final-answer content)))

          (macrolet ((finish (answer-expr)
                       `(let ((ans ,answer-expr))
                          (verbose-final ans)
                          (notify :final ans)
                          (rlm/repl:environment-add-history
                           env question ans
                           :work-log (format nil "~{~A~^~%~%~}"
                                             (nreverse work-log-parts)))
                          (return-from query
                            (make-instance 'query-result
                                           :answer ans
                                           :iterations (1+ i)
                                           :token-usage (list :prompt-tokens total-prompt-tokens
                                                              :completion-tokens total-completion-tokens)
                                           :environment env)))))

            ;; No code blocks + bare FINAL → done
            (when (and (null code-blocks) bare-final)
              (finish bare-final))

            (cond
              ;; No code blocks, no FINAL → nudge for code
              ((null code-blocks)
               (setf messages
                     (append messages
                             (list (list :role "assistant" :content content)
                                   (list :role "user" :content
                                         "Please write Common Lisp code in ```lisp blocks to explore the context. Don't just describe what you'd do — actually write the code.")))))

              ;; Has code blocks → execute them
              (t
               (setf messages (append messages (list (list :role "assistant" :content content))))
               (block execute-blocks
                 (dolist (code code-blocks)
                   (let* ((result (rlm/repl:environment-execute env code))
                          (output (format-exec-output result)))
                     (verbose-code code output)
                     (notify :code code)
                     (notify :output code output)
                     ;; Log code + key output for work history
                     (let ((out-preview (if (> (length output) 200)
                                            (format nil "~A..." (subseq output 0 200))
                                            output)))
                       (log-work (format nil "Code: ~A~%Output: ~A"
                                         (if (> (length code) 150)
                                             (format nil "~A..." (subseq code 0 150))
                                             code)
                                         out-preview)))

                     ;; Check for final answer from execution
                     (when (rlm/repl:exec-result-final-answer result)
                       (finish (rlm/repl:exec-result-final-answer result)))

                     ;; Add code echo to messages
                     (setf messages
                           (append messages
                                   (list (list :role "user" :content
                                              (format-code-echo code output))))))))

               ;; Check bare FINAL after code blocks ran
               (when bare-final
                 (finish bare-final))

               ;; Add continuation prompt
               (setf messages
                     (append messages
                             (list (rlm/prompts:build-iteration-continue-message question))))))))))

    ;; Max iterations — ask for final answer
    (let* ((fallback-messages
             (append messages (list (rlm/prompts:build-fallback-message))))
           (response (rlm/client:chat-completion fallback-messages))
           (content (rlm/client:completion-content response)))
      (verbose-final content)
      (notify :final content)
      (rlm/repl:environment-add-history
       env question content
       :work-log (format nil "~{~A~^~%~%~}" (nreverse work-log-parts)))
      (make-instance 'query-result
                     :answer content
                     :iterations max-iterations
                     :token-usage (list :prompt-tokens total-prompt-tokens
                                        :completion-tokens total-completion-tokens)
                     :environment env))))))
