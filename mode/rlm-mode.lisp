(in-package :lem-rlm-mode)

;;; RLM mode — interactive agent loop in a Lem buffer.
;;;
;;; Uses listener-mode for the chat buffer, runs the RLM query
;;; in a background thread, and streams iteration progress back
;;; via send-event.

;;; ============================================================
;;; Attributes for styling output
;;; ============================================================

(define-attribute rlm-thinking-attribute
  (t :foreground :base08 :bold nil))

(define-attribute rlm-code-attribute
  (t :foreground :base0B))

(define-attribute rlm-output-attribute
  (t :foreground :base04))

(define-attribute rlm-error-attribute
  (t :foreground :base08 :bold t))

(define-attribute rlm-final-attribute
  (t :foreground :base0D :bold t))

(define-attribute rlm-separator-attribute
  (t :foreground :base03))

;;; ============================================================
;;; Buffer state
;;; ============================================================

(defvar *rlm-environment* nil
  "Persistent RLM environment across queries. Holds sandbox state.")

(defvar *rlm-busy* nil
  "True when an RLM query is running.")

(defvar *rlm-spinner* nil
  "Current modeline spinner during a query.")

(defvar *rlm-buffer-name* "*RLM*"
  "Name of the RLM chat buffer.")

(defvar *rlm-task-mode* nil
  "When T, the current query is a task (editing) session.
Final answers are shown inline rather than in a separate buffer.")

(defvar *rlm-cancel* nil
  "When T, the running query will be cancelled at the next iteration.")

;;; ============================================================
;;; Major mode
;;; ============================================================

(define-major-mode rlm-mode ()
    (:name "RLM"
     :keymap *rlm-mode-keymap*))

;;; ============================================================
;;; Output helpers — append styled text to the RLM buffer
;;; ============================================================

(defvar *rlm-code-block-stripper*
  (cl-ppcre:create-scanner "```[a-z]*\\s*\\n.*?\\n\\s*```"
                           :single-line-mode t)
  "Scanner to strip fenced code blocks from LLM responses.")

(defun rlm-strip-code-blocks (text)
  "Remove ```...``` fenced code blocks from TEXT, returning just the reasoning."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (cl-ppcre:regex-replace-all *rlm-code-block-stripper* text "")))

(defun rlm-buffer ()
  "Get the RLM buffer, or nil if it doesn't exist."
  (get-buffer *rlm-buffer-name*))

(defun rlm-write-point (buffer)
  "Get or create the persistent write point for the RLM buffer.
This point is :left-inserting so it advances as text is inserted,
keeping output in chronological order."
  (or (buffer-value buffer 'rlm-write-point)
      (let ((isp (lem/listener-mode:input-start-point buffer)))
        (when isp
          (let ((p (copy-point isp :left-inserting)))
            (setf (buffer-value buffer 'rlm-write-point) p))))))

(defun rlm-reset-write-point (buffer)
  "Reset the write point to the current input-start-point.
Call this after refresh-prompt to prepare for the next query's output."
  (let ((old (buffer-value buffer 'rlm-write-point))
        (isp (lem/listener-mode:input-start-point buffer)))
    (when old (delete-point old))
    (when isp
      (setf (buffer-value buffer 'rlm-write-point)
            (copy-point isp :left-inserting)))))

(defun rlm-append (string &optional attribute)
  "Append STRING to the RLM buffer at the write point.
Must be called from the editor thread (via send-event)."
  (let ((buffer (rlm-buffer)))
    (when buffer
      (let ((p (rlm-write-point buffer)))
        (when p
          (let ((*inhibit-read-only* t))
            (if attribute
                (insert-string p string :sticky-attribute attribute)
                (insert-string p string))))))))

(defun rlm-append-line (string &optional attribute)
  "Append STRING followed by a newline."
  (rlm-append (format nil "~A~%" string) attribute))

(defvar *rlm-answer-counter* 0
  "Counter for generating unique answer buffer names.")

(defun rlm-present-answer (answer)
  "Open ANSWER in a markdown-mode buffer and note it in the chat buffer."
  (let* ((n (incf *rlm-answer-counter*))
         (buf-name (format nil "*RLM Answer ~A*" n))
         (buffer (make-buffer buf-name)))
    (erase-buffer buffer)
    (let ((point (buffer-point buffer)))
      (insert-string point answer))
    (change-buffer-mode buffer 'lem-markdown-mode:markdown-mode)
    (setf (buffer-read-only-p buffer) t)
    (switch-to-window (pop-to-buffer buffer))
    ;; Note in chat buffer
    (rlm-append-line (format nil "(see buffer ~A)" buf-name)
                     'rlm-separator-attribute)))

;;; ============================================================
;;; Event handler — called from the background thread via send-event
;;; ============================================================

(defun rlm-handle-event (event-type &rest args)
  "Handle an RLM event by appending styled output to the chat buffer.
Called from within send-event, so we're in the editor thread."
  (flet ((update-spinner (msg)
           (when *rlm-spinner*
             (setf (slot-value *rlm-spinner* 'lem/loading-spinner::loading-message) msg))))
  (case event-type
    (:thinking
     (let ((n (first args)))
       (update-spinner (format nil "Thinking (iteration ~A)..." n))
       (rlm-append-line (format nil "~%⏳ Thinking (iteration ~A)..." n)
                        'rlm-thinking-attribute)))
    (:response
     (let ((content (second args)))
       (when content
         ;; Show reasoning text with code blocks stripped out
         (let ((reasoning (rlm-strip-code-blocks content)))
           (when (plusp (length reasoning))
             (rlm-append-line reasoning 'rlm-thinking-attribute))))))
    (:code
     (let ((code (first args)))
       (update-spinner "Running code...")
       (rlm-append-line (format nil "▶ ~A"
                                (if (> (length code) 200)
                                    (format nil "~A..." (subseq code 0 200))
                                    code))
                        'rlm-code-attribute)))
    (:output
     (let ((output (second args)))
       (when (and output (plusp (length output)))
         (rlm-append-line (format nil "◀ ~A"
                                  (if (> (length output) 300)
                                      (format nil "~A..." (subseq output 0 300))
                                      output))
                          'rlm-output-attribute))))
    (:error
     (let ((msg (first args)))
       (rlm-append-line (format nil "ERROR: ~A" msg) 'rlm-error-attribute)))
    (:final
     (let ((answer (first args)))
       (if *rlm-task-mode*
           ;; Task mode: just note completion inline, no separate buffer
           (rlm-append-line (format nil "~%✎ ~A" answer) 'rlm-final-attribute)
           ;; Query mode: open answer in a markdown buffer
           (progn
             (rlm-append-line "" nil)
             (rlm-append-line "━━━ Answer ━━━" 'rlm-separator-attribute)
             (rlm-present-answer answer)
             (rlm-append-line "━━━━━━━━━━━━━━" 'rlm-separator-attribute))))))))

;;; ============================================================
;;; Query execution
;;; ============================================================

(defun ensure-rlm-environment ()
  "Get or create the persistent RLM environment with editor tools."
  (unless *rlm-environment*
    (setf *rlm-environment*
          (rlm/repl:make-environment :llm-query-fn (rlm:make-llm-query-fn)))
    ;; Register editor tools
    (dolist (tool (rlm-editor-tools))
      (rlm/repl:register-tool *rlm-environment* tool))
    ;; Register file tools — allow home directory tree for exploring related files
    (let ((dirs (list (user-homedir-pathname))))
      (rlm/repl:register-tool *rlm-environment*
                               (rlm/tools:make-read-file-tool
                                :allowed-dirs dirs))
      (rlm/repl:register-tool *rlm-environment*
                               (rlm/tools:make-list-directory-tool
                                :allowed-dirs dirs)))
    ;; Web tools if available
    (when rlm/tools:*jina-api-key*
      (rlm/repl:register-tool *rlm-environment*
                               (rlm/tools:make-web-read-tool))
      (rlm/repl:register-tool *rlm-environment*
                               (rlm/tools:make-web-search-tool))))
  *rlm-environment*)

(defun run-rlm-query (question &key context task-mode)
  "Run an RLM query in a background thread, streaming events to the buffer.
When TASK-MODE is T, final answers are shown inline instead of in a separate buffer."
  (when *rlm-busy*
    (editor-error "RLM is already running a query"))
  (setf *rlm-busy* t)
  (setf *rlm-task-mode* task-mode)
  (setf *rlm-cancel* nil)
  (let* ((env (ensure-rlm-environment))
         (ctx (or context
                  (format nil "No specific context. Use your tools (~{~A~^, ~}) to gather information."
                          (mapcar #'rlm/repl:tool-name
                                  (rlm/repl:environment-tools env)))))
         (buffer (rlm-buffer))
         (spinner (when buffer
                    (lem/loading-spinner:start-loading-spinner
                     :modeline :buffer buffer :loading-message "Starting..."))))
    (setf *rlm-spinner* spinner)
    (bt2:make-thread
     (lambda ()
       (let ((rlm:*verbose* nil))
         (handler-case
             (let ((result (rlm:query ctx question
                                      :max-iterations 15
                                      :environment env
                                      :cancel-flag (lambda () *rlm-cancel*)
                                      :on-event (lambda (event-type &rest args)
                                                  (send-event
                                                   (lambda ()
                                                     (apply #'rlm-handle-event
                                                            event-type args)
                                                     (redraw-display)))))))
               (send-event (lambda ()
                             (when spinner
                               (lem/loading-spinner:stop-loading-spinner spinner)
                               (setf *rlm-spinner* nil))
                             (rlm-append-line
                              (format nil "~%✓ Done (~A iterations)"
                                      (rlm:query-result-iterations result))
                              'rlm-separator-attribute)
                             (let ((buf (rlm-buffer)))
                               (when buf
                                 (lem/listener-mode:refresh-prompt buf)
                                 (rlm-reset-write-point buf))
                               (setf *rlm-busy* nil
                                     *rlm-task-mode* nil)
                               (redraw-display)))))
           (serious-condition (e)
             (send-event (lambda ()
                           (when spinner
                             (lem/loading-spinner:stop-loading-spinner spinner))
                           (rlm-append-line (format nil "~%Error: ~A" e)
                                            'rlm-error-attribute)
                           (let ((buf (rlm-buffer)))
                             (when buf
                               (lem/listener-mode:refresh-prompt buf)
                               (rlm-reset-write-point buf)))
                           (setf *rlm-busy* nil
                                 *rlm-task-mode* nil)
                           (redraw-display)))))))
     :name "rlm-query")))

;;; ============================================================
;;; Listener callbacks
;;; ============================================================

(defun rlm-set-prompt (point)
  "Write the RLM prompt."
  (insert-string point "rlm> "))

(defun rlm-check-input (point)
  "Always accept single-line input."
  (declare (ignore point))
  t)

(defun rlm-execute (point string)
  "Execute user input as an RLM query."
  (declare (ignore point))
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) string)))
    (cond
      ((string= trimmed "")
       nil)
      ((string= trimmed "/reset")
       (setf *rlm-environment* nil)
       (rlm-append-line "Session reset." 'rlm-thinking-attribute)
       (let ((buf (rlm-buffer)))
         (lem/listener-mode:refresh-prompt buf)
         (rlm-reset-write-point buf)))
      ((string= trimmed "/model")
       (rlm-append-line (format nil "Current model: ~A" rlm/client:*model*)
                        'rlm-thinking-attribute)
       (let ((buf (rlm-buffer)))
         (lem/listener-mode:refresh-prompt buf)
         (rlm-reset-write-point buf)))
      ((alexandria:starts-with-subseq "/model " trimmed)
       (let ((model (string-trim '(#\Space) (subseq trimmed 7))))
         (setf rlm/client:*model* model)
         (rlm-append-line (format nil "Model set to: ~A" model)
                          'rlm-thinking-attribute)
         (let ((buf (rlm-buffer)))
           (lem/listener-mode:refresh-prompt buf)
           (rlm-reset-write-point buf))))
      (t
       (run-rlm-query trimmed)))))

;;; ============================================================
;;; Modeline
;;; ============================================================

(define-attribute rlm-modeline-attribute
  (t :foreground :base0B :bold t))

(defun rlm-modeline-element (window)
  "Show current model in the modeline for RLM buffers."
  (when (eq (buffer-major-mode (window-buffer window)) 'rlm-mode)
    (values (format nil " [~A] " rlm/client:*model*)
            'rlm-modeline-attribute
            :right)))

;;; ============================================================
;;; Commands
;;; ============================================================

(define-command rlm-prompt () ()
  "Open the RLM interactive chat buffer."
  (let ((buffer (make-buffer *rlm-buffer-name*)))
    (switch-to-window (pop-to-buffer buffer))
    (unless (eq (buffer-major-mode buffer) 'rlm-mode)
      (change-buffer-mode buffer 'rlm-mode)
      (lem/listener-mode:start-listener-mode)
      (setf (variable-value 'lem/listener-mode:listener-set-prompt-function
                            :buffer buffer)
            'rlm-set-prompt)
      (setf (variable-value 'lem/listener-mode:listener-check-input-function
                            :buffer buffer)
            'rlm-check-input)
      (setf (variable-value 'lem/listener-mode:listener-execute-function
                            :buffer buffer)
            'rlm-execute)
      (modeline-add-status-list 'rlm-modeline-element buffer)
      (lem/listener-mode:refresh-prompt buffer)
      (rlm-reset-write-point buffer))))

(defun rlm-model-completion (input)
  "Return completion candidates for model names matching INPUT."
  (let ((models (rlm/client:list-models)))
    (when models
      (let ((input-lower (string-downcase input)))
        (loop :for id :in models
              :when (search input-lower (string-downcase id))
              :collect id)))))

(define-command rlm-switch-model () ()
  "Switch the RLM model with completion from available models."
  (let ((model (prompt-for-string "Model: "
                                  :initial-value rlm/client:*model*
                                  :completion-function #'rlm-model-completion)))
    (when (and model (plusp (length model)))
      (setf rlm/client:*model* model)
      (message "RLM model: ~A" model))))

(define-command rlm-toggle-yeet-mode () ()
  "Toggle yeet mode (auto-accept edits vs confirm each edit)."
  (setf *rlm-yeet-mode* (not *rlm-yeet-mode*))
  (message "RLM yeet mode: ~A" (if *rlm-yeet-mode* "ON (auto-accept)" "OFF (confirm each edit)")))

(define-command rlm-cancel () ()
  "Cancel the currently running RLM query."
  (if *rlm-busy*
      (progn
        (setf *rlm-cancel* t)
        (message "RLM: cancelling after current iteration..."))
      (message "RLM: no query running")))

(define-command rlm-ask-about-buffer () ()
  "Send the current buffer contents as context to the RLM agent."
  (let* ((buffer (current-buffer))
         (text (points-to-string (buffer-start-point buffer)
                                 (buffer-end-point buffer)))
         (question (prompt-for-string "Question about this buffer: ")))
    (when (and question (plusp (length question)))
      ;; Switch to RLM buffer
      (rlm-prompt)
      ;; Insert the question text so it appears in the listener
      (let ((rlm-buf (rlm-buffer)))
        (when rlm-buf
          (let ((point (buffer-point rlm-buf)))
            (insert-string point question)
            ;; Run with buffer contents as context
            (run-rlm-query question
                           :context (format nil "Buffer: ~A~%Filename: ~A~%Directory: ~A~%You can use list-directory and read-file to explore nearby files for additional context.~%~%~A"
                                   (buffer-name buffer)
                                   (or (buffer-filename buffer) "(no file)")
                                   (if (buffer-filename buffer)
                                       (directory-namestring (buffer-filename buffer))
                                       "(unknown)")
                                   text))))))))

(define-command rlm-ask-about-region () ()
  "Send the selected region as context to the RLM agent."
  (let* ((start (cursor-region-beginning (current-point)))
         (end (cursor-region-end (current-point)))
         (text (points-to-string start end))
         (question (prompt-for-string "Question about selection: ")))
    (when (and question (plusp (length question)))
      (rlm-prompt)
      (let ((rlm-buf (rlm-buffer)))
        (when rlm-buf
          (let ((point (buffer-point rlm-buf)))
            (insert-string point question)
            (run-rlm-query question
                           :context (format nil "Selected text from ~A:~%~%~A"
                                            (buffer-name (current-buffer))
                                            text))))))))

(define-command rlm-perform-task () ()
  "Start an agentic editing session with a task prompt for the current buffer."
  (let* ((buffer (current-buffer))
         (numbered (with-output-to-string (s)
                     (with-point ((p (buffer-start-point buffer)))
                       (loop :for n :from 1
                             :do (format s "~4D: ~A~%" n (line-string p))
                             :while (line-offset p 1)))))
         (task (prompt-for-string "Task: ")))
    (when (and task (plusp (length task)))
      (rlm-prompt)
      (let ((rlm-buf (rlm-buffer)))
        (when rlm-buf
          (let ((point (buffer-point rlm-buf)))
            (insert-string point task)
            (run-rlm-query task
                           :context (format nil "You are an editor agent. Your job is to make edits to the buffer shown below.

IMPORTANT RULES:
- Make edits ONE AT A TIME. Read the buffer, make ONE edit, then read again to verify and continue.
- Do NOT prepare all changes in variables first. Edit immediately after reading.
- Each iteration should contain exactly ONE call-tool edit.
- After all edits are done, call (final \"done\").

Tools:
- (call-tool \"read-buffer\" name) or (call-tool \"read-buffer\" name start-line end-line) — see contents with line numbers
- (call-tool \"edit-buffer\" name start-line end-line new-text) — replace lines start through end (inclusive)
- (call-tool \"insert-at\" name line new-text) — insert new text before a line
- (call-tool \"save-buffer\" name) — save to disk (only when asked or when fully done)
- (call-tool \"ask-user\" question) — ask the user a question and get their response

When you propose an edit, the user can:
- Accept it (applied to buffer)
- Skip it (undone, move on to next change)
- Request changes (undone, you receive their feedback — use it to revise your approach)

After each edit, line numbers shift. Always re-read before the next edit.

Buffer: ~A (~A lines)
Filename: ~A
Directory: ~A

~A"
                                   (buffer-name buffer)
                                   (buffer-nlines buffer)
                                   (or (buffer-filename buffer) "(no file)")
                                   (if (buffer-filename buffer)
                                       (directory-namestring (buffer-filename buffer))
                                       "(unknown)")
                                   numbered)
                           :task-mode t)))))))
