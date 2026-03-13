(in-package :lem-rlm-mode)

;;; Editor tools — these wrap Lem buffer/window operations
;;; for use by the RLM agent via call-tool.

;;; ============================================================
;;; Edit confirmation (yeet mode vs confirm mode)
;;; ============================================================

(defvar *rlm-yeet-mode* nil
  "When T, apply edits immediately without confirmation.
When NIL (default), highlight the edit and prompt the user to accept/reject.")

(define-attribute rlm-pending-edit-attribute
  (t :background :base0B :foreground :base00 :bold t))

;;; ============================================================
;;; Buffer text helpers
;;; ============================================================

(defun buffer-text-with-line-numbers (buffer)
  "Return the buffer contents as a string with line numbers prefixed."
  (with-output-to-string (s)
    (with-point ((point (buffer-start-point buffer)))
      (loop :for line-num :from 1
            :do (format s "~4D: ~A~%" line-num
                        (line-string point))
            :while (line-offset point 1)))))

;;; ============================================================
;;; Tools
;;; ============================================================

(defun make-read-buffer-tool ()
  "Tool that reads the contents of a Lem buffer with line numbers."
  (rlm/repl:make-tool
   :name "read-buffer"
   :description "Read the contents of a named buffer with line numbers. Optional start/end line numbers to read a specific range. If no name given, reads the most recent non-RLM buffer. Output format: '   1: first line' etc."
   :parameter-doc "(call-tool \"read-buffer\") or (call-tool \"read-buffer\" buffer-name) or (call-tool \"read-buffer\" buffer-name start-line end-line)"
   :execute-fn
   (lambda (&optional name start-line end-line)
     (let ((buffer (if name
                       (get-buffer name)
                       (find-if (lambda (b)
                                  (not (eql (buffer-major-mode b)
                                            'rlm-mode)))
                                (buffer-list)))))
       (unless buffer
         (error "Buffer not found: ~A" (or name "(no non-RLM buffer)")))
       (let ((start (if start-line (max 1 (truncate start-line)) 1))
             (end (if end-line (min (truncate end-line) (buffer-nlines buffer))
                      (buffer-nlines buffer))))
         (format nil "Buffer: ~A (~A lines)~%Filename: ~A~%Showing lines ~A-~A:~%~%~A"
                 (buffer-name buffer)
                 (buffer-nlines buffer)
                 (or (buffer-filename buffer) "(no file)")
                 start end
                 (with-output-to-string (s)
                   (with-point ((point (buffer-start-point buffer)))
                     (move-to-line point start)
                     (loop :for line-num :from start :to end
                           :do (format s "~4D: ~A~%" line-num
                                       (line-string point))
                           :while (line-offset point 1))))))))))

(defun make-list-buffers-tool ()
  "Tool that lists all open Lem buffers."
  (rlm/repl:make-tool
   :name "list-buffers"
   :description "List all open buffers with their names, filenames, and sizes."
   :parameter-doc "(call-tool \"list-buffers\")"
   :execute-fn
   (lambda ()
     (with-output-to-string (s)
       (dolist (buffer (buffer-list))
         (format s "~A~@[ (~A)~] ~A lines~%"
                 (buffer-name buffer)
                 (buffer-filename buffer)
                 (buffer-nlines buffer)))))))

(defun make-open-file-tool ()
  "Tool that opens a file in a new Lem buffer."
  (rlm/repl:make-tool
   :name "open-file"
   :description "Open a file in a new editor buffer. Returns the buffer name and line count."
   :parameter-doc "(call-tool \"open-file\" path)"
   :execute-fn
   (lambda (path)
     (let ((buffer (find-file-buffer path)))
       (unless buffer
         (error "Could not open file: ~A" path))
       (format nil "Opened buffer: ~A (~A lines)~%Filename: ~A"
               (buffer-name buffer)
               (buffer-nlines buffer)
               (or (buffer-filename buffer) path))))))

(defun make-present-tool ()
  "Tool that opens markdown content in a new buffer with markdown-mode."
  (rlm/repl:make-tool
   :name "present"
   :description "Present markdown content to the user in a new read-only buffer. Use for reports, summaries, or formatted output — NOT for edits."
   :parameter-doc "(call-tool \"present\" title content) — title is the buffer name, content is markdown text."
   :execute-fn
   (lambda (title content)
     (let* ((buf-name (format nil "*RLM: ~A*" title))
            (buffer (make-buffer buf-name)))
       (erase-buffer buffer)
       (let ((point (buffer-point buffer)))
         (insert-string point content))
       (change-buffer-mode buffer 'lem-markdown-mode:markdown-mode)
       (setf (buffer-read-only-p buffer) t)
       (send-event (lambda ()
                     (switch-to-window (pop-to-buffer buffer))))
       (format nil "Presented in buffer: ~A (~A chars)" buf-name (length content))))))

(defun apply-line-edit (buffer start end new-text)
  "Apply a line-range edit to BUFFER, replacing lines START through END with NEW-TEXT.
Returns (values start-point end-point) marking the new text region, for highlighting.
Caller must delete the returned points when done.
Caller must be in the editor thread or use send-event."
  (let ((nlines (buffer-nlines buffer)))
    (when (> start nlines)
      (error "Start line ~A is past end of buffer (~A lines)" start nlines))
    (setf end (min end nlines))
    (when (> start end)
      (error "Start line ~A is after end line ~A" start end))
    (with-point ((p1 (buffer-start-point buffer))
                 (p2 (buffer-start-point buffer)))
      (move-to-line p1 start)
      (line-start p1)
      (if (= end nlines)
          (buffer-end p2)
          (progn
            (move-to-line p2 (1+ end))
            (line-start p2)))
      (let ((*inhibit-read-only* t))
        (delete-between-points p1 p2)
        (let ((text (if (and (plusp (length new-text))
                             (char/= (char new-text (1- (length new-text))) #\Newline)
                             (/= end nlines))
                        (format nil "~A~%" new-text)
                        new-text))
              (region-start (copy-point p1 :left-inserting)))
          (insert-string p1 text)
          ;; region-start stayed put (left-inserting), p1 advanced (right-inserting)
          (values region-start (copy-point p1 :left-inserting)))))))

(defun apply-line-insert (buffer line-num new-text)
  "Insert NEW-TEXT before LINE-NUM in BUFFER.
Returns (values start-point end-point) marking the inserted region.
Caller must delete the returned points when done."
  (let ((nlines (buffer-nlines buffer)))
    (with-point ((p (buffer-start-point buffer)))
      (if (> line-num nlines)
          (buffer-end p)
          (progn
            (move-to-line p line-num)
            (line-start p)))
      (let ((*inhibit-read-only* t)
            (text (if (and (plusp (length new-text))
                           (char/= (char new-text (1- (length new-text))) #\Newline))
                      (format nil "~A~%" new-text)
                      new-text))
            (region-start (copy-point p :left-inserting)))
        (insert-string p text)
        (values region-start (copy-point p :left-inserting))))))

(defun execute-edit-with-confirmation (buffer apply-fn description-fn)
  "Execute an edit via APPLY-FN, then confirm with user unless in yeet mode.
APPLY-FN is called with no arguments from the editor thread and must return
\(values start-point end-point). DESCRIPTION-FN is a function called with no
arguments after a successful edit (on the editor thread) to produce the result
string. Returns the result string for the agent, or an error/rejection message."
  (if *rlm-yeet-mode*
      ;; Yeet mode: apply and focus, no confirmation
      (let ((result nil))
        (let ((lock (bt2:make-lock :name "rlm-edit"))
              (cv (bt2:make-condition-variable :name "rlm-edit")))
          (send-event
           (lambda ()
             (handler-case
                 (multiple-value-bind (start-pt end-pt) (funcall apply-fn)
                   ;; Focus the change
                   (let ((window (car (get-buffer-windows buffer))))
                     (unless window
                       (setf window (pop-to-buffer buffer)))
                     (switch-to-window window)
                     (move-to-line (buffer-point buffer)
                                   (line-number-at-point start-pt)))
                   (delete-point start-pt)
                   (delete-point end-pt)
                   (redraw-display)
                   (bt2:with-lock-held (lock)
                     (setf result (funcall description-fn))
                     (bt2:condition-notify cv)))
               (error (e)
                 (bt2:with-lock-held (lock)
                   (setf result (format nil "Error: ~A" e))
                   (bt2:condition-notify cv))))))
          (bt2:with-lock-held (lock)
            (loop :until result
                  :do (bt2:condition-wait cv lock)))
          result))
      ;; Confirm mode: apply, highlight, prompt user
      (let ((result nil))
        (let ((lock (bt2:make-lock :name "rlm-confirm"))
              (cv (bt2:make-condition-variable :name "rlm-confirm")))
          (send-event
           (lambda ()
             (handler-case
                 (progn
                   ;; Place an undo boundary before the edit
                   (buffer-undo-boundary buffer)
                   (multiple-value-bind (start-pt end-pt) (funcall apply-fn)
                     ;; Highlight the new text
                     (let ((overlay (make-overlay start-pt end-pt
                                                  'rlm-pending-edit-attribute)))
                       ;; Clean up our points (overlay made its own copies)
                       (delete-point start-pt)
                       (delete-point end-pt)
                       ;; Focus the buffer on the edit
                       (let ((window (car (get-buffer-windows buffer))))
                         (unless window
                           (setf window (pop-to-buffer buffer)))
                         (switch-to-window window)
                         (move-to-line (buffer-point buffer)
                                       (line-number-at-point (overlay-start overlay))))
                       (redraw-display)
                       ;; Prompt: single-keypress a/s/c/h
                       (let ((ch (loop :for c := (prompt-for-character
                                                  "[a]ccept [s]kip [c]hange [h]alt? ")
                                       :do (when (member c '(#\a #\s #\c #\h))
                                             (return c)))))
                         (delete-overlay overlay)
                         (cond
                             ((eql ch #\a)
                              (bt2:with-lock-held (lock)
                                (setf result (funcall description-fn))
                                (bt2:condition-notify cv)))
                             ((eql ch #\c)
                              ;; Undo, then get feedback from user
                              (buffer-undo (buffer-point buffer))
                              (redraw-display)
                              (let ((feedback (prompt-for-string "Feedback: ")))
                                (bt2:with-lock-held (lock)
                                  (setf result
                                        (format nil "REJECTED: User wants changes. Their feedback: ~A" feedback))
                                  (bt2:condition-notify cv))))
                             ((eql ch #\h)
                              ;; Halt — undo edit and cancel the whole query
                              (buffer-undo (buffer-point buffer))
                              (redraw-display)
                              (setf *rlm-cancel* t)
                              (bt2:with-lock-held (lock)
                                (setf result "HALTED: User cancelled the task.")
                                (bt2:condition-notify cv)))
                             (t
                              ;; Skip — undo and tell agent to move on
                              (buffer-undo (buffer-point buffer))
                              (redraw-display)
                              (bt2:with-lock-held (lock)
                                (setf result "SKIPPED: User skipped this edit. Move on to the next change.")
                                (bt2:condition-notify cv))))))))
               (error (e)
                 (bt2:with-lock-held (lock)
                   (setf result (format nil "Error: ~A" e))
                   (bt2:condition-notify cv))))))
          (bt2:with-lock-held (lock)
            (loop :until result
                  :do (bt2:condition-wait cv lock)))
          result))))

(defun make-edit-buffer-tool ()
  "Tool that replaces a range of lines in a buffer."
  (rlm/repl:make-tool
   :name "edit-buffer"
   :description "Replace lines START through END (inclusive, 1-indexed) in a buffer with NEW-TEXT. Use read-buffer first to see line numbers. To delete lines, pass empty string as new-text."
   :parameter-doc "(call-tool \"edit-buffer\" buffer-name start-line end-line new-text)"
   :execute-fn
   (lambda (buffer-name start-line end-line new-text)
     (let ((buffer (get-buffer buffer-name)))
       (unless buffer
         (error "Buffer not found: ~A" buffer-name))
       (let ((start (max 1 (truncate start-line)))
             (end (truncate end-line)))
         (execute-edit-with-confirmation
          buffer
          (lambda () (apply-line-edit buffer start end new-text))
          (lambda ()
            (format nil "Replaced lines ~A-~A in ~A (now ~A lines)"
                    start end buffer-name (buffer-nlines buffer)))))))))

(defun make-insert-lines-tool ()
  "Tool that inserts text at a specific line in a buffer."
  (rlm/repl:make-tool
   :name "insert-at"
   :description "Insert NEW-TEXT before line number LINE in a buffer. Use to add new code without removing existing lines. Line 1 inserts at the top."
   :parameter-doc "(call-tool \"insert-at\" buffer-name line new-text)"
   :execute-fn
   (lambda (buffer-name line-num new-text)
     (let ((buffer (get-buffer buffer-name)))
       (unless buffer
         (error "Buffer not found: ~A" buffer-name))
       (let ((line (max 1 (truncate line-num))))
         (execute-edit-with-confirmation
          buffer
          (lambda () (apply-line-insert buffer line new-text))
          (lambda ()
            (format nil "Inserted at line ~A in ~A (now ~A lines)"
                    line buffer-name (buffer-nlines buffer)))))))))

(defun make-save-buffer-tool ()
  "Tool that saves a buffer to disk."
  (rlm/repl:make-tool
   :name "save-buffer"
   :description "Save a buffer to its file. The buffer must have an associated filename. Only save when the user explicitly asks you to save, or when you are completely done with all edits."
   :parameter-doc "(call-tool \"save-buffer\" buffer-name)"
   :execute-fn
   (lambda (buffer-name)
     (let ((buffer (get-buffer buffer-name)))
       (unless buffer
         (error "Buffer not found: ~A" buffer-name))
       (unless (buffer-filename buffer)
         (error "Buffer ~A has no associated file" buffer-name))
       (if *rlm-yeet-mode*
           (progn
             (save-buffer buffer)
             (format nil "Saved ~A to ~A" buffer-name (buffer-filename buffer)))
           ;; Confirm mode: ask before saving
           (let ((result nil))
             (let ((lock (bt2:make-lock :name "rlm-save"))
                   (cv (bt2:make-condition-variable :name "rlm-save")))
               (send-event
                (lambda ()
                  (let ((accepted (prompt-for-y-or-n-p
                                   (format nil "Save ~A" buffer-name))))
                    (cond
                      (accepted
                       (save-buffer buffer)
                       (bt2:with-lock-held (lock)
                         (setf result (format nil "Saved ~A to ~A"
                                              buffer-name (buffer-filename buffer)))
                         (bt2:condition-notify cv)))
                      (t
                       (bt2:with-lock-held (lock)
                         (setf result "SKIPPED: User declined to save.")
                         (bt2:condition-notify cv)))))))
               (bt2:with-lock-held (lock)
                 (loop :until result
                       :do (bt2:condition-wait cv lock)))
               result)))))))

(defun make-ask-user-tool ()
  "Tool that prompts the user with a question and returns their response."
  (rlm/repl:make-tool
   :name "ask-user"
   :description "Ask the user a question and get their response. Use when you need clarification, want to confirm an approach before proceeding, or the user requested changes to an edit."
   :parameter-doc "(call-tool \"ask-user\" question)"
   :execute-fn
   (lambda (question)
     (let ((result nil))
       (let ((lock (bt2:make-lock :name "rlm-ask"))
             (cv (bt2:make-condition-variable :name "rlm-ask")))
         (send-event
          (lambda ()
            (handler-case
                (let ((answer (prompt-for-string
                               (format nil "~A " question))))
                  (bt2:with-lock-held (lock)
                    (setf result (or answer ""))
                    (bt2:condition-notify cv)))
              (error (e)
                (bt2:with-lock-held (lock)
                  (setf result (format nil "Error: ~A" e))
                  (bt2:condition-notify cv))))))
         (bt2:with-lock-held (lock)
           (loop :until result
                 :do (bt2:condition-wait cv lock)))
         result)))))

(defun rlm-editor-tools ()
  "Return list of all editor-integration tools for the RLM agent."
  (list (make-read-buffer-tool)
        (make-list-buffers-tool)
        (make-open-file-tool)
        (make-present-tool)
        (make-edit-buffer-tool)
        (make-insert-lines-tool)
        (make-save-buffer-tool)
        (make-ask-user-tool)))
