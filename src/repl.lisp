(in-package :rlm/repl)

;;; REPL Environment — wraps the sandbox with context, llm-query, and helpers.

;;; ============================================================
;;; Tool definition
;;; ============================================================

(defclass tool ()
  ((name :accessor tool-name :initarg :name)
   (description :accessor tool-description :initarg :description
                :documentation "Short description shown to the LLM.")
   (parameter-doc :accessor tool-parameter-doc :initarg :parameter-doc :initform ""
                  :documentation "Human-readable parameter documentation for the LLM.")
   (execute-fn :accessor tool-execute-fn :initarg :execute-fn
               :documentation "Function to call. Receives keyword args matching parameter names.")))

(defun make-tool (&key name description (parameter-doc "") execute-fn)
  "Create a tool definition."
  (make-instance 'tool
                 :name name
                 :description description
                 :parameter-doc parameter-doc
                 :execute-fn execute-fn))

;;; ============================================================
;;; Environment
;;; ============================================================

(defclass environment ()
  ((sandbox :accessor env-sandbox)
   (llm-query-fn :accessor env-llm-query-fn :initarg :llm-query-fn :initform nil
                 :documentation "Function (lambda (prompt) -> string) for sub-LLM calls.")
   (tools :accessor env-tools :initform (make-hash-table :test 'equal)
          :documentation "Registered tools keyed by name.")
   (history :accessor env-history :initform '()
            :documentation "List of (question . answer) pairs from prior queries, oldest first.")))

(defclass exec-result ()
  ((stdout :accessor exec-result-stdout :initarg :stdout :initform "")
   (stderr :accessor exec-result-stderr :initarg :stderr :initform "")
   (error :accessor exec-result-error :initarg :error :initform nil)
   (final-answer :accessor exec-result-final-answer :initarg :final-answer :initform nil)
   (variables :accessor exec-result-variables :initarg :variables :initform nil)))

(defun register-tool (env tool)
  "Register a TOOL in the environment. Can be called before or after make-environment init."
  (setf (gethash (tool-name tool) (env-tools env)) tool))

(defun environment-tools (env)
  "Return list of registered tools."
  (let ((tools '()))
    (maphash (lambda (k v) (declare (ignore k)) (push v tools))
             (env-tools env))
    (nreverse tools)))

(defun make-environment (&key llm-query-fn)
  "Create a new REPL environment with sandbox and injected functions."
  (let* ((env (make-instance 'environment :llm-query-fn llm-query-fn))
         (sb (rlm/sandbox:make-sandbox)))
    (setf (env-sandbox env) sb)
    ;; Inject helper functions
    (rlm/sandbox:inject-function sb "llm-query"
      (lambda (prompt)
        (if (env-llm-query-fn env)
            (funcall (env-llm-query-fn env) prompt)
            (error "No LLM query function configured"))))
    (rlm/sandbox:inject-function sb "show-vars"
      (lambda ()
        (let ((vars '()))
          (maphash (lambda (k v)
                     (push (format nil "~A: ~A" k (type-of v)) vars))
                   (rlm/sandbox:sandbox-variables sb))
          (format nil "Variables: ~{~A~^, ~}" (nreverse vars)))))
    (rlm/sandbox:inject-function sb "final"
      (lambda (value)
        (let ((pkg (rlm/sandbox:sandbox-user-package sb)))
          (when pkg
            (let ((sym (intern "FINAL-ANSWER" pkg)))
              (proclaim `(special ,sym))
              (setf (symbol-value sym) value))))
        value))
    (rlm/sandbox:inject-function sb "final-var"
      (lambda (var-name)
        (let* ((pkg (rlm/sandbox:sandbox-user-package sb))
               (sym (and pkg (find-symbol (string-upcase var-name) pkg))))
          (if (and sym (boundp sym))
              (let ((value (symbol-value sym)))
                (let ((fa-sym (intern "FINAL-ANSWER" pkg)))
                  (proclaim `(special ,fa-sym))
                  (setf (symbol-value fa-sym) value))
                value)
              (error "Variable '~A' not found. Available: ~{~A~^, ~}"
                     var-name
                     (let ((keys '()))
                       (maphash (lambda (k v) (declare (ignore v)) (push k keys))
                                (rlm/sandbox:sandbox-variables sb))
                       keys))))))
    ;; Inject call-tool dispatcher
    (rlm/sandbox:inject-function sb "call-tool"
      (lambda (tool-name &rest args)
        (let ((tool (gethash tool-name (env-tools env))))
          (unless tool
            (error "Unknown tool '~A'. Available tools: ~{~A~^, ~}"
                   tool-name
                   (let ((names '()))
                     (maphash (lambda (k v) (declare (ignore v)) (push k names))
                              (env-tools env))
                     names)))
          (let ((result (apply (tool-execute-fn tool) args)))
            (format t "~A~%" result)
            result))))
    ;; Inject text processing functions (cl-ppcre wrappers)
    (rlm/sandbox:inject-function sb "split-string"
      (lambda (str &optional (delimiter "\\s+"))
        (cl-ppcre:split delimiter str)))
    (rlm/sandbox:inject-function sb "regex-match"
      (lambda (pattern str)
        (multiple-value-bind (match groups)
            (cl-ppcre:scan-to-strings pattern str)
          (when match
            (cons match (coerce groups 'list))))))
    (rlm/sandbox:inject-function sb "regex-scan"
      (lambda (pattern str)
        (cl-ppcre:all-matches-as-strings pattern str)))
    (rlm/sandbox:inject-function sb "regex-replace"
      (lambda (pattern str replacement)
        (cl-ppcre:regex-replace-all pattern str replacement)))
    (rlm/sandbox:inject-function sb "words"
      (lambda (str)
        (remove "" (cl-ppcre:split "\\s+" str) :test #'string=)))
    (rlm/sandbox:inject-function sb "lines"
      (lambda (str)
        (cl-ppcre:split "\\n" str)))
    (rlm/sandbox:inject-function sb "join-strings"
      (lambda (list &optional (separator " "))
        (format nil (concatenate 'string "~{~A~^" separator "~}") list)))
    (rlm/sandbox:inject-function sb "word-frequency"
      (lambda (str)
        (let ((table (make-hash-table :test 'equal))
              (result '()))
          (dolist (w (remove "" (cl-ppcre:split "\\s+" str) :test #'string=))
            (let ((lower (string-downcase w)))
              (incf (gethash lower table 0))))
          (maphash (lambda (k v) (push (cons k v) result)) table)
          (sort result #'> :key #'cdr))))
    ;; Inject list-tools helper
    (rlm/sandbox:inject-function sb "list-tools"
      (lambda ()
        (let ((descriptions '()))
          (maphash (lambda (name tool)
                     (push (format nil "~A: ~A" name (tool-description tool))
                           descriptions))
                   (env-tools env))
          (format nil "Available tools:~%~{  ~A~%~}" (nreverse descriptions)))))
    env))

(defun environment-add-history (env question answer &key work-log)
  "Append a history entry to the environment's conversation history.
WORK-LOG is an optional string summarizing the reasoning and exploration done."
  (setf (env-history env)
        (append (env-history env)
                (list (list :question question
                            :answer answer
                            :work-log work-log)))))

(defun environment-history (env)
  "Return the conversation history as a list of (question . answer) pairs."
  (env-history env))

(defun environment-set-context (env context)
  "Load CONTEXT (a string or list) into the environment.
Sets both CONTEXT and *CONTEXT* so either naming convention works."
  (rlm/sandbox:set-variable (env-sandbox env) "context" context)
  (rlm/sandbox:set-variable (env-sandbox env) "*context*" context))

(defun environment-clear-final (env)
  "Move FINAL-ANSWER into *LAST-ANSWER*, sync *HISTORY*, and clear final so a new query starts fresh."
  (let* ((sb (env-sandbox env))
         (pkg (rlm/sandbox:sandbox-user-package sb)))
    (when pkg
      (let ((sym (find-symbol "FINAL-ANSWER" pkg)))
        (when (and sym (boundp sym))
          ;; Stash into *last-answer* for quick reference
          (rlm/sandbox:set-variable sb "*last-answer*" (symbol-value sym))
          (makunbound sym)))))
  ;; Remove from hash table so ensure-user-package doesn't re-bind it
  (remhash "final-answer" (rlm/sandbox:sandbox-variables (env-sandbox env)))
  ;; Sync full history into sandbox as *history*
  ;; Each entry is (question . answer) — the model can iterate, search, nth, etc.
  (let ((history (env-history env)))
    (when history
      (rlm/sandbox:set-variable (env-sandbox env) "*history*" history))))

(defun environment-variables (env)
  "Return the sandbox variables as an alist."
  (let ((result '()))
    (maphash (lambda (k v) (push (cons k v) result))
             (rlm/sandbox:sandbox-variables (env-sandbox env)))
    result))

(defun environment-execute (env code-string)
  "Execute CODE-STRING in the environment. Returns an EXEC-RESULT."
  (let ((sb-result (rlm/sandbox:sandbox-eval (env-sandbox env) code-string)))
    (make-instance 'exec-result
                   :stdout (rlm/sandbox:sandbox-result-stdout sb-result)
                   :stderr (rlm/sandbox:sandbox-result-stderr sb-result)
                   :error (rlm/sandbox:sandbox-result-error sb-result)
                   :final-answer (rlm/sandbox:sandbox-result-final-answer sb-result)
                   :variables (let ((vars '()))
                                (maphash (lambda (k v)
                                           (push (cons k (type-of v)) vars))
                                         (rlm/sandbox:sandbox-variables (env-sandbox env)))
                                vars))))
