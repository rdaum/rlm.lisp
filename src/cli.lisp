(in-package :rlm/cli)

;;; Interactive CLI for the RLM agent loop.

(defvar *prompt* "rlm> "
  "The prompt string displayed to the user.")

(defvar *default-max-iterations* 15
  "Default max iterations for each query.")

(defvar *tools* '()
  "List of tools available in the CLI session.")

(defvar *context* nil
  "Current context string. Set with /context or starts nil.")

(defvar *environment* nil
  "Persistent environment across queries. Holds sandbox state.")

(defun print-banner ()
  (format t "~%RLM — Recursive Language Model (Common Lisp)~%")
  (format t "Model: ~A~%" rlm/client:*model*)
  (format t "Tools: ~{~A~^, ~}~%" (mapcar #'rlm/repl:tool-name *tools*))
  (unless rlm/tools:*jina-api-key*
    (format t "Note: web-search disabled (set JINA_API_KEY to enable)~%"))
  (format t "Type a prompt to run the agent loop. /help for commands.~%~%"))

(defun print-help ()
  (format t "Commands:~%")
  (format t "  /help         — show this help~%")
  (format t "  /quit         — exit~%")
  (format t "  /model NAME   — switch model~%")
  (format t "  /context TEXT  — set context string~%")
  (format t "  /context-file PATH — load context from file~%")
  (format t "  /verbose      — toggle verbose output~%")
  (format t "  /tools        — list registered tools~%")
  (format t "  /vars         — show persistent sandbox variables~%")
  (format t "  /reset        — clear sandbox state (fresh session)~%")
  (format t "  /iterations N — set max iterations~%"))

(defun read-multiline ()
  "Read input from the user. A trailing backslash continues to the next line."
  (let ((lines '()))
    (loop
      (let ((line (read-line *standard-input* nil :eof)))
        (when (eq line :eof)
          (if lines
              (return (format nil "~{~A~%~}" (nreverse lines)))
              (return nil)))
        (cond
          ((and (plusp (length line))
                (char= (char line (1- (length line))) #\\))
           (push (subseq line 0 (1- (length line))) lines))
          (t
           (push line lines)
           (return (string-right-trim '(#\Newline)
                                      (format nil "~{~A~%~}" (nreverse lines))))))))))

(defun ensure-environment ()
  "Return the persistent environment, creating it if needed."
  (unless *environment*
    (setf *environment* (rlm/repl:make-environment
                         :llm-query-fn (rlm::make-llm-query-fn)))
    ;; Register tools
    (dolist (tool *tools*)
      (rlm/repl:register-tool *environment* tool)))
  *environment*)

(defun reset-environment ()
  "Clear the persistent environment."
  (setf *environment* nil)
  (format t "Session reset. Sandbox state cleared.~%"))

(defun show-variables ()
  "Show current sandbox variables."
  (let* ((env (ensure-environment))
         (vars (remove "context" (rlm/repl:environment-variables env)
                       :key #'car :test #'string=)))
    (if vars
        (progn
          (format t "Sandbox variables:~%")
          (dolist (pair vars)
            (let* ((val-str (princ-to-string (cdr pair)))
                   (preview (if (> (length val-str) 100)
                                (format nil "~A..." (subseq val-str 0 100))
                                val-str)))
              (format t "  ~A (~A): ~A~%" (car pair) (type-of (cdr pair)) preview))))
        (format t "No variables set.~%"))))

(defun handle-command (input)
  "Handle a /command. Returns T if handled, NIL if not a command."
  (unless (and (plusp (length input)) (char= (char input 0) #\/))
    (return-from handle-command nil))
  (let* ((space-pos (position #\Space input))
         (cmd (string-downcase (subseq input 0 (or space-pos (length input)))))
         (arg (when space-pos
                (string-trim '(#\Space #\Tab) (subseq input (1+ space-pos))))))
    (cond
      ((string= cmd "/quit")
       (throw 'quit-cli nil))
      ((string= cmd "/help")
       (print-help))
      ((string= cmd "/model")
       (if (and arg (plusp (length arg)))
           (progn
             (setf rlm/client:*model* arg)
             (format t "Model set to: ~A~%" arg))
           (format t "Current model: ~A~%Usage: /model NAME~%" rlm/client:*model*)))
      ((string= cmd "/context")
       (if (and arg (plusp (length arg)))
           (progn
             (setf *context* arg)
             (format t "Context set (~A chars)~%" (length arg)))
           (if *context*
               (format t "Context (~A chars): ~A~%"
                       (length *context*)
                       (if (> (length *context*) 200)
                           (format nil "~A..." (subseq *context* 0 200))
                           *context*))
               (format t "No context set. Use /context TEXT or /context-file PATH~%"))))
      ((string= cmd "/context-file")
       (if (and arg (plusp (length arg)))
           (handler-case
               (progn
                 (setf *context* (uiop:read-file-string arg))
                 (format t "Context loaded from ~A (~A chars)~%" arg (length *context*)))
             (error (e) (format t "Error reading file: ~A~%" e)))
           (format t "Usage: /context-file PATH~%")))
      ((string= cmd "/verbose")
       (setf rlm:*verbose* (not rlm:*verbose*))
       (format t "Verbose: ~A~%" (if rlm:*verbose* "on" "off")))
      ((string= cmd "/tools")
       (if (null *tools*)
           (format t "No tools registered.~%")
           (dolist (tool *tools*)
             (format t "  ~A — ~A~%" (rlm/repl:tool-name tool) (rlm/repl:tool-description tool)))))
      ((string= cmd "/vars")
       (show-variables))
      ((string= cmd "/reset")
       (reset-environment))
      ((string= cmd "/iterations")
       (if (and arg (plusp (length arg)))
           (let ((n (parse-integer arg :junk-allowed t)))
             (if (and n (plusp n))
                 (progn
                   (setf *default-max-iterations* n)
                   (format t "Max iterations set to ~A~%" n))
                 (format t "Invalid number: ~A~%" arg)))
           (format t "Max iterations: ~A~%Usage: /iterations N~%" *default-max-iterations*)))
      (t
       (format t "Unknown command: ~A. Try /help~%" cmd))))
  (force-output)
  t)

(defun run-query (input)
  "Run the agent loop on user input with persistent environment."
  (let ((context (or *context* "No specific context was set. Use your tools (list-directory, read-file, web-read, web-search) to gather information as needed."))
        (env (ensure-environment)))
    (format t "~%")
    (force-output)
    (handler-case
        (let ((result (rlm:query context input
                                 :max-iterations *default-max-iterations*
                                 :environment env)))
          ;; Update our environment reference from the result
          (setf *environment* (rlm:query-result-environment result))
          (format t "~%~%━━━ Answer ━━━~%~A~%" (rlm:query-result-answer result))
          (format t "~%(~A iterations, ~A prompt + ~A completion tokens)~%"
                  (rlm:query-result-iterations result)
                  (getf (rlm:query-result-token-usage result) :prompt-tokens 0)
                  (getf (rlm:query-result-token-usage result) :completion-tokens 0)))
      (error (e)
        (format t "~%Error: ~A~%" e))))
  (force-output))

(defun main (&key tools (verbose t))
  "Start the interactive CLI.
TOOLS is a list of tool instances to register.
VERBOSE controls whether agent iterations are printed."
  (setf *tools* (or tools '()))
  (setf *environment* nil)
  (setf rlm:*verbose* verbose)
  (print-banner)
  (catch 'quit-cli
    (loop
      (format t "~%~A" *prompt*)
      (force-output)
      (let ((input (read-multiline)))
        (unless input
          (format t "~%")
          (return))
        (when (plusp (length (string-trim '(#\Space #\Tab) input)))
          (let ((trimmed (string-trim '(#\Space #\Tab) input)))
            (unless (handle-command trimmed)
              (run-query trimmed)))))))
  (format t "Bye!~%"))

(defun default-tools ()
  "Return a standard set of tools for general use.
Omits web-search if no JINA_API_KEY is set."
  (let ((tools (list (rlm/tools:make-web-read-tool)
                     (rlm/tools:make-list-directory-tool)
                     (rlm/tools:make-read-file-tool))))
    (if rlm/tools:*jina-api-key*
        (cons (rlm/tools:make-web-search-tool) tools)
        (progn
          (format t "Note: web-search disabled (no JINA_API_KEY set)~%")
          tools))))
