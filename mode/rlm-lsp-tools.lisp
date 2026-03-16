(in-package :lem-rlm-mode)

;;; LSP tools — expose language server intelligence to the RLM agent.
;;; These require an active language server for the buffer's language.

;;; ============================================================
;;; Helpers
;;; ============================================================

(defun lsp-workspace-for-buffer (buffer)
  "Get the LSP workspace for BUFFER, or NIL if none active."
  (handler-case (lem-lsp-mode::buffer-workspace buffer nil)
    (error () nil)))

(defun lsp-available-p (buffer)
  "Return T if an LSP server is active for BUFFER."
  (not (null (lsp-workspace-for-buffer buffer))))

(defun resolve-buffer-and-point (buffer-name line column)
  "Look up BUFFER-NAME and create a point at LINE (1-indexed), COLUMN (0-indexed).
Returns (values buffer point) or signals an error."
  (let ((buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer not found: ~A" buffer-name))
    (unless (lsp-available-p buffer)
      (error "No language server active for buffer ~A" buffer-name))
    (let ((point (copy-point (buffer-point buffer) :temporary)))
      (move-to-line point (max 1 line))
      (line-start point)
      (character-offset point (max 0 column))
      (values buffer point))))

(defun format-lsp-location (location)
  "Format an LSP location as a readable string."
  (let* ((uri (lsp:location-uri location))
         (range (lsp:location-range location))
         (start (lsp:range-start range))
         (file (handler-case (lem-lsp-base/utils:uri-to-pathname uri)
                 (error () uri))))
    (format nil "~A:~A:~A"
            (namestring file)
            (1+ (lsp:position-line start))
            (lsp:position-character start))))

(defun format-lsp-location-with-content (location)
  "Format an LSP location with the source line content."
  (let* ((uri (lsp:location-uri location))
         (range (lsp:location-range location))
         (start (lsp:range-start range))
         (file (handler-case (lem-lsp-base/utils:uri-to-pathname uri)
                 (error () nil)))
         (line-num (1+ (lsp:position-line start)))
         (content (when (and file (probe-file file))
                    (handler-case
                        (with-open-file (in file :direction :input)
                          (loop :repeat (1- line-num) :do (read-line in nil))
                          (read-line in nil))
                      (error () nil)))))
    (format nil "~A:~A:~A~@[  ~A~]"
            (or (when file (namestring file)) uri)
            line-num
            (lsp:position-character start)
            content)))

;;; ============================================================
;;; Synchronous LSP call helper
;;; ============================================================

(defun call-on-editor-thread (fn)
  "Call FN on Lem's editor thread and wait for the result.
FN receives no arguments and should return a string."
  (let ((result nil)
        (lock (bt2:make-lock :name "rlm-lsp"))
        (cv (bt2:make-condition-variable :name "rlm-lsp")))
    (send-event
     (lambda ()
       (handler-case
           (progn
             (rlm-stop-spinner)
             (let ((val (funcall fn)))
               (bt2:with-lock-held (lock)
                 (setf result (or val ""))
                 (bt2:condition-notify cv))))
         (error (e)
           (bt2:with-lock-held (lock)
             (setf result (format nil "Error: ~A" e))
             (bt2:condition-notify cv))))))
    (bt2:with-lock-held (lock)
      (loop :until result
            :do (bt2:condition-wait cv lock)))
    result))

;;; ============================================================
;;; Tools
;;; ============================================================

(defun make-lsp-hover-tool ()
  "Tool that shows hover information (type, docs) for a symbol."
  (rlm/repl:make-tool
   :name "lsp-hover"
   :description "Get type information and documentation for the symbol at a position in a buffer. Requires an active language server. Line is 1-indexed, column is 0-indexed."
   :parameter-doc "(call-tool \"lsp-hover\" buffer-name line column)"
   :execute-fn
   (lambda (buffer-name line column)
     (call-on-editor-thread
      (lambda ()
        (multiple-value-bind (buffer point)
            (resolve-buffer-and-point buffer-name line column)
          (declare (ignore buffer))
          (let ((workspace (lem-lsp-mode::get-workspace-from-point point)))
            (unless (lem-lsp-mode::provide-hover-p workspace)
              (error "Server does not support hover"))
            (let ((result
                    (lem-language-client/request:request
                     (lem-lsp-mode::workspace-client workspace)
                     (make-instance 'lsp:text-document/hover)
                     (apply #'make-instance
                            'lsp:hover-params
                            (lem-lsp-mode::make-text-document-position-arguments point)))))
              (if (or (null result) (lem-lsp-base/type:lsp-null-p result))
                  "(no hover info)"
                  (lem-lsp-mode::contents-to-string
                   (lsp:hover-contents result)))))))))))

(defun make-lsp-definition-tool ()
  "Tool that finds the definition of a symbol."
  (rlm/repl:make-tool
   :name "lsp-definition"
   :description "Find the definition location(s) of the symbol at a position. Returns file path, line, and column for each definition. Line is 1-indexed, column is 0-indexed."
   :parameter-doc "(call-tool \"lsp-definition\" buffer-name line column)"
   :execute-fn
   (lambda (buffer-name line column)
     (call-on-editor-thread
      (lambda ()
        (multiple-value-bind (buffer point)
            (resolve-buffer-and-point buffer-name line column)
          (declare (ignore buffer))
          (let ((workspace (lem-lsp-mode::get-workspace-from-point point)))
            (unless (lem-lsp-mode::provide-definition-p workspace)
              (error "Server does not support go-to-definition"))
            (let ((result
                    (lem-language-client/request:request
                     (lem-lsp-mode::workspace-client workspace)
                     (make-instance 'lsp:text-document/definition)
                     (apply #'make-instance
                            'lsp:definition-params
                            (lem-lsp-mode::make-text-document-position-arguments point)))))
              (cond
                ((or (null result) (lem-lsp-base/type:lsp-null-p result))
                 "(no definition found)")
                ((typep result 'lsp:location)
                 (format-lsp-location-with-content result))
                ((lem-lsp-base/type:lsp-array-p result)
                 (with-output-to-string (s)
                   (map nil (lambda (loc)
                              (format s "~A~%"
                                      (format-lsp-location-with-content loc)))
                        result)))
                (t "(unexpected response type)"))))))))))

(defun make-lsp-references-tool ()
  "Tool that finds all references to a symbol."
  (rlm/repl:make-tool
   :name "lsp-references"
   :description "Find all references to the symbol at a position. Returns file path, line, and source content for each reference. Line is 1-indexed, column is 0-indexed."
   :parameter-doc "(call-tool \"lsp-references\" buffer-name line column)"
   :execute-fn
   (lambda (buffer-name line column)
     (call-on-editor-thread
      (lambda ()
        (multiple-value-bind (buffer point)
            (resolve-buffer-and-point buffer-name line column)
          (declare (ignore buffer))
          (let ((workspace (lem-lsp-mode::get-workspace-from-point point)))
            (unless (lem-lsp-mode::provide-references-p workspace)
              (error "Server does not support find-references"))
            (let ((result
                    (lem-language-client/request:request
                     (lem-lsp-mode::workspace-client workspace)
                     (make-instance 'lsp:text-document/references)
                     (apply #'make-instance
                            'lsp:reference-params
                            :context (make-instance 'lsp:reference-context
                                                    :include-declaration t)
                            (lem-lsp-mode::make-text-document-position-arguments point)))))
              (cond
                ((or (null result) (lem-lsp-base/type:lsp-null-p result))
                 "(no references found)")
                ((lem-lsp-base/type:lsp-array-p result)
                 (let ((count (length result)))
                   (with-output-to-string (s)
                     (format s "~A reference~:P:~%" count)
                     (map nil (lambda (loc)
                                (format s "  ~A~%"
                                        (format-lsp-location-with-content loc)))
                          result))))
                (t "(unexpected response type)"))))))))))

(defun make-lsp-document-symbols-tool ()
  "Tool that lists all symbols defined in a buffer."
  (rlm/repl:make-tool
   :name "lsp-symbols"
   :description "List all symbols (functions, classes, variables, etc.) defined in a buffer. Requires an active language server."
   :parameter-doc "(call-tool \"lsp-symbols\" buffer-name)"
   :execute-fn
   (lambda (buffer-name)
     (call-on-editor-thread
      (lambda ()
        (let ((buffer (get-buffer buffer-name)))
          (unless buffer
            (error "Buffer not found: ~A" buffer-name))
          (unless (lsp-available-p buffer)
            (error "No language server active for buffer ~A" buffer-name))
          (let ((workspace (lsp-workspace-for-buffer buffer)))
            (unless (lem-lsp-mode::provide-document-symbol-p workspace)
              (error "Server does not support document symbols"))
            (let ((result
                    (lem-language-client/request:request
                     (lem-lsp-mode::workspace-client workspace)
                     (make-instance 'lsp:text-document/document-symbol)
                     (make-instance
                      'lsp:document-symbol-params
                      :text-document (lem-lsp-mode::make-text-document-identifier buffer)))))
              (cond
                ((or (null result) (lem-lsp-base/type:lsp-null-p result))
                 "(no symbols found)")
                ((lem-lsp-base/type:lsp-array-p result)
                 (with-output-to-string (s)
                   (labels ((format-symbol (sym indent)
                              (let* ((name (lsp:document-symbol-name sym))
                                     (kind (lsp:document-symbol-kind sym))
                                     (range (lsp:document-symbol-range sym))
                                     (start (lsp:range-start range))
                                     (line (1+ (lsp:position-line start)))
                                     (kind-name (symbol-kind-name kind)))
                                (format s "~A~A ~A (line ~A)~%"
                                        (make-string indent :initial-element #\Space)
                                        kind-name name line)
                                ;; Recurse into children
                                (handler-case
                                    (let ((children (lsp:document-symbol-children sym)))
                                      (when (and children
                                                 (not (lem-lsp-base/type:lsp-null-p children)))
                                        (map nil (lambda (c) (format-symbol c (+ indent 2)))
                                             children)))
                                  (unbound-slot () nil)))))
                     (map nil (lambda (sym) (format-symbol sym 0)) result))))
                (t "(unexpected response type)"))))))))))

(defun symbol-kind-name (kind)
  "Convert LSP SymbolKind integer to a readable name."
  (case kind
    (1 "File") (2 "Module") (3 "Namespace") (4 "Package")
    (5 "Class") (6 "Method") (7 "Property") (8 "Field")
    (9 "Constructor") (10 "Enum") (11 "Interface") (12 "Function")
    (13 "Variable") (14 "Constant") (15 "String") (16 "Number")
    (17 "Boolean") (18 "Array") (19 "Object") (20 "Key")
    (21 "Null") (22 "EnumMember") (23 "Struct") (24 "Event")
    (25 "Operator") (26 "TypeParameter")
    (otherwise (format nil "Kind(~A)" kind))))

(defun make-lsp-diagnostics-tool ()
  "Tool that shows current diagnostics (errors, warnings) for a buffer."
  (rlm/repl:make-tool
   :name "lsp-diagnostics"
   :description "Show current diagnostics (errors, warnings, info) for a buffer from the language server."
   :parameter-doc "(call-tool \"lsp-diagnostics\" buffer-name)"
   :execute-fn
   (lambda (buffer-name)
     (call-on-editor-thread
      (lambda ()
        (let ((buffer (get-buffer buffer-name)))
          (unless buffer
            (error "Buffer not found: ~A" buffer-name))
          (unless (lsp-available-p buffer)
            (error "No language server active for buffer ~A" buffer-name))
          (let ((diagnostics (lem-lsp-mode::buffer-diagnostics buffer)))
            (if (or (null diagnostics) (zerop (length diagnostics)))
                "(no diagnostics)"
                (with-output-to-string (s)
                  (format s "~A diagnostic~:P:~%" (length diagnostics))
                  (map nil
                       (lambda (diag)
                         (let* ((range (lsp:diagnostic-range diag))
                                (start (lsp:range-start range))
                                (line (1+ (lsp:position-line start)))
                                (severity (handler-case (lsp:diagnostic-severity diag)
                                            (unbound-slot () nil)))
                                (message (lsp:diagnostic-message diag))
                                (sev-name (case severity
                                            (1 "ERROR") (2 "WARN")
                                            (3 "INFO") (4 "HINT")
                                            (otherwise "?"))))
                           (format s "  ~A line ~A: ~A~%" sev-name line message)))
                       diagnostics))))))))))

;;; ============================================================
;;; Registration
;;; ============================================================

(defun rlm-lsp-tools ()
  "Return list of all LSP tools for the RLM agent."
  (list (make-lsp-hover-tool)
        (make-lsp-definition-tool)
        (make-lsp-references-tool)
        (make-lsp-document-symbols-tool)
        (make-lsp-diagnostics-tool)))
