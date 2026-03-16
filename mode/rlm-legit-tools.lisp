(in-package :lem-rlm-mode)

;;; Git tools via Lem's legit/porcelain layer.
;;; These give the RLM agent native VCS operations without shelling out.

;;; ============================================================
;;; VCS helper — resolve project from working directory
;;; ============================================================

(defun call-with-vcs (fn)
  "Find the VCS project for *rlm-working-directory* and call FN with it.
Sets CWD to the project root, as the porcelain layer expects."
  (let ((dir (or *rlm-working-directory* (uiop:getcwd))))
    (let ((root (lem-core/commands/project:find-root (namestring dir))))
      (uiop:with-current-directory (root)
        (let ((vcs (lem/legit::vcs-project-p)))
          (unless vcs
            (error "No VCS found in ~A" root))
          (funcall fn vcs))))))

(defmacro with-vcs ((vcs) &body body)
  `(call-with-vcs (lambda (,vcs) ,@body)))

;;; ============================================================
;;; Tools
;;; ============================================================

(defun make-git-status-tool ()
  "Tool that shows VCS status: untracked, unstaged, and staged files."
  (rlm/repl:make-tool
   :name "git-status"
   :description "Show version control status: untracked files, unstaged changes, and staged changes. Also shows the current branch."
   :parameter-doc "(call-tool \"git-status\")"
   :execute-fn
   (lambda ()
     (with-vcs (vcs)
       (let ((branch (handler-case (lem/porcelain:current-branch vcs)
                       (error () "(unknown)"))))
         (multiple-value-bind (untracked unstaged staged)
             (lem/porcelain:components vcs)
           (with-output-to-string (s)
             (format s "Branch: ~A~%" branch)
             (format s "~%Staged (~A):~%" (length staged))
             (dolist (f staged)
               (format s "  ~A ~A~%" (getf f :type) (getf f :file)))
             (format s "~%Unstaged (~A):~%" (length unstaged))
             (dolist (f unstaged)
               (format s "  ~A ~A~%" (getf f :type) (getf f :file)))
             ;; Untracked files are plain strings, not plists
             (format s "~%Untracked (~A):~%" (length untracked))
             (dolist (f untracked)
               (format s "  ~A~%" f)))))))))

(defun make-git-diff-tool ()
  "Tool that shows the diff for a file."
  (rlm/repl:make-tool
   :name "git-diff"
   :description "Show the diff for a file. Use :cached t to see staged changes, or omit for unstaged changes."
   :parameter-doc "(call-tool \"git-diff\" file) or (call-tool \"git-diff\" file :cached t)"
   :execute-fn
   (lambda (file &key cached)
     (with-vcs (vcs)
       (let ((diff (lem/porcelain:file-diff vcs file :cached cached)))
         (if (and diff (plusp (length diff)))
             diff
             "(no changes)"))))))

(defun make-git-stage-tool ()
  "Tool that stages a file."
  (rlm/repl:make-tool
   :name "git-stage"
   :description "Stage a file for commit. Pass \".\" to stage all changes."
   :parameter-doc "(call-tool \"git-stage\" file)"
   :execute-fn
   (lambda (file)
     (with-vcs (vcs)
       (lem/porcelain:stage vcs file)
       (format nil "Staged: ~A" file)))))

(defun make-git-unstage-tool ()
  "Tool that unstages a file."
  (rlm/repl:make-tool
   :name "git-unstage"
   :description "Unstage a file (reverse of stage)."
   :parameter-doc "(call-tool \"git-unstage\" file)"
   :execute-fn
   (lambda (file)
     (with-vcs (vcs)
       (lem/porcelain:unstage vcs file)
       (format nil "Unstaged: ~A" file)))))

(defun make-git-commit-tool ()
  "Tool that commits staged changes."
  (rlm/repl:make-tool
   :name "git-commit"
   :description "Commit staged changes with the given message. Stage files first with git-stage."
   :parameter-doc "(call-tool \"git-commit\" message)"
   :execute-fn
   (lambda (message)
     (if *rlm-yeet-mode*
         (with-vcs (vcs)
           (lem/porcelain:commit vcs message)
           (format nil "Committed: ~A" message))
         ;; Confirm mode
         (let ((result nil))
           (let ((lock (bt2:make-lock :name "rlm-commit"))
                 (cv (bt2:make-condition-variable :name "rlm-commit")))
             (send-event
              (lambda ()
                (handler-case
                    (let ((saved-spinner (rlm-pause-spinner)))
                      (let ((accepted (prompt-for-y-or-n-p
                                       (format nil "Commit: ~A" message))))
                        (rlm-resume-spinner saved-spinner)
                        (cond
                          (accepted
                           (with-vcs (vcs)
                             (lem/porcelain:commit vcs message))
                           (bt2:with-lock-held (lock)
                             (setf result (format nil "Committed: ~A" message))
                             (bt2:condition-notify cv)))
                          (t
                           (bt2:with-lock-held (lock)
                             (setf result "SKIPPED: User declined to commit.")
                             (bt2:condition-notify cv))))))
                  (error (e)
                    (bt2:with-lock-held (lock)
                      (setf result (format nil "Error: ~A" e))
                      (bt2:condition-notify cv))))))
             (bt2:with-lock-held (lock)
               (loop :until result
                     :do (bt2:condition-wait cv lock)))
             result))))))

(defun make-git-log-tool ()
  "Tool that shows recent commits."
  (rlm/repl:make-tool
   :name "git-log"
   :description "Show recent commits. Returns commit hash and message for each."
   :parameter-doc "(call-tool \"git-log\") or (call-tool \"git-log\" :n 20)"
   :execute-fn
   (lambda (&key (n 15))
     (with-vcs (vcs)
       (let ((commits (lem/porcelain:latest-commits vcs :n n)))
         (with-output-to-string (s)
           (dolist (c commits)
             (format s "~A~%" (or (getf c :line)
                                  (format nil "~A ~A"
                                          (getf c :hash)
                                          (getf c :message)))))))))))

(defun make-git-show-tool ()
  "Tool that shows the diff for a specific commit."
  (rlm/repl:make-tool
   :name "git-show"
   :description "Show the diff for a specific commit by hash or ref."
   :parameter-doc "(call-tool \"git-show\" ref)"
   :execute-fn
   (lambda (ref)
     (with-vcs (vcs)
       (let ((diff (lem/porcelain:show-commit-diff vcs ref)))
         (if (and diff (plusp (length diff)))
             diff
             "(empty commit)"))))))

(defun make-git-branch-tool ()
  "Tool that lists branches or switches branch."
  (rlm/repl:make-tool
   :name "git-branches"
   :description "List all branches. The current branch is shown with an asterisk."
   :parameter-doc "(call-tool \"git-branches\")"
   :execute-fn
   (lambda ()
     (with-vcs (vcs)
       (let ((current (handler-case (lem/porcelain:current-branch vcs)
                        (error () nil)))
             (all (lem/porcelain:branches vcs)))
         (with-output-to-string (s)
           (dolist (b all)
             (format s "~A ~A~%" (if (string= b current) "*" " ") b))))))))

(defun make-git-checkout-tool ()
  "Tool that switches to a branch."
  (rlm/repl:make-tool
   :name "git-checkout"
   :description "Switch to an existing branch."
   :parameter-doc "(call-tool \"git-checkout\" branch)"
   :execute-fn
   (lambda (branch)
     (if *rlm-yeet-mode*
         (with-vcs (vcs)
           (lem/porcelain:checkout vcs branch)
           (format nil "Switched to: ~A" branch))
         (let ((result nil))
           (let ((lock (bt2:make-lock :name "rlm-checkout"))
                 (cv (bt2:make-condition-variable :name "rlm-checkout")))
             (send-event
              (lambda ()
                (handler-case
                    (let ((saved-spinner (rlm-pause-spinner)))
                      (let ((accepted (prompt-for-y-or-n-p
                                       (format nil "Checkout ~A" branch))))
                        (rlm-resume-spinner saved-spinner)
                        (cond
                          (accepted
                           (with-vcs (vcs)
                             (lem/porcelain:checkout vcs branch))
                           (bt2:with-lock-held (lock)
                             (setf result (format nil "Switched to: ~A" branch))
                             (bt2:condition-notify cv)))
                          (t
                           (bt2:with-lock-held (lock)
                             (setf result "SKIPPED: User declined checkout.")
                             (bt2:condition-notify cv))))))
                  (error (e)
                    (bt2:with-lock-held (lock)
                      (setf result (format nil "Error: ~A" e))
                      (bt2:condition-notify cv))))))
             (bt2:with-lock-held (lock)
               (loop :until result
                     :do (bt2:condition-wait cv lock)))
             result))))))

(defun make-git-push-tool ()
  "Tool that pushes to the default remote."
  (rlm/repl:make-tool
   :name "git-push"
   :description "Push commits to the default remote."
   :parameter-doc "(call-tool \"git-push\")"
   :execute-fn
   (lambda ()
     (if *rlm-yeet-mode*
         (with-vcs (vcs)
           (lem/porcelain:push-default vcs)
           "Pushed to remote.")
         (let ((result nil))
           (let ((lock (bt2:make-lock :name "rlm-push"))
                 (cv (bt2:make-condition-variable :name "rlm-push")))
             (send-event
              (lambda ()
                (handler-case
                    (let ((saved-spinner (rlm-pause-spinner)))
                      (let ((accepted (prompt-for-y-or-n-p "Push to remote")))
                        (rlm-resume-spinner saved-spinner)
                        (cond
                          (accepted
                           (with-vcs (vcs)
                             (lem/porcelain:push-default vcs))
                           (bt2:with-lock-held (lock)
                             (setf result "Pushed to remote.")
                             (bt2:condition-notify cv)))
                          (t
                           (bt2:with-lock-held (lock)
                             (setf result "SKIPPED: User declined push.")
                             (bt2:condition-notify cv))))))
                  (error (e)
                    (bt2:with-lock-held (lock)
                      (setf result (format nil "Error: ~A" e))
                      (bt2:condition-notify cv))))))
             (bt2:with-lock-held (lock)
               (loop :until result
                     :do (bt2:condition-wait cv lock)))
             result))))))

(defun make-git-pull-tool ()
  "Tool that pulls from the remote."
  (rlm/repl:make-tool
   :name "git-pull"
   :description "Pull changes from the remote."
   :parameter-doc "(call-tool \"git-pull\")"
   :execute-fn
   (lambda ()
     (if *rlm-yeet-mode*
         (with-vcs (vcs)
           (lem/porcelain:pull vcs)
           "Pulled from remote.")
         (let ((result nil))
           (let ((lock (bt2:make-lock :name "rlm-pull"))
                 (cv (bt2:make-condition-variable :name "rlm-pull")))
             (send-event
              (lambda ()
                (handler-case
                    (let ((saved-spinner (rlm-pause-spinner)))
                      (let ((accepted (prompt-for-y-or-n-p "Pull from remote")))
                        (rlm-resume-spinner saved-spinner)
                        (cond
                          (accepted
                           (with-vcs (vcs)
                             (lem/porcelain:pull vcs))
                           (bt2:with-lock-held (lock)
                             (setf result "Pulled from remote.")
                             (bt2:condition-notify cv)))
                          (t
                           (bt2:with-lock-held (lock)
                             (setf result "SKIPPED: User declined pull.")
                             (bt2:condition-notify cv))))))
                  (error (e)
                    (bt2:with-lock-held (lock)
                      (setf result (format nil "Error: ~A" e))
                      (bt2:condition-notify cv))))))
             (bt2:with-lock-held (lock)
               (loop :until result
                     :do (bt2:condition-wait cv lock)))
             result))))))

(defun make-git-discard-tool ()
  "Tool that discards changes to a file."
  (rlm/repl:make-tool
   :name "git-discard"
   :description "Discard all changes to a file. THIS IS DESTRUCTIVE — changes are permanently lost."
   :parameter-doc "(call-tool \"git-discard\" file)"
   :execute-fn
   (lambda (file)
     ;; Always confirm, even in yeet mode — too destructive
     (let ((result nil))
       (let ((lock (bt2:make-lock :name "rlm-discard"))
             (cv (bt2:make-condition-variable :name "rlm-discard")))
         (send-event
          (lambda ()
            (handler-case
                (let ((saved-spinner (rlm-pause-spinner)))
                  (let ((accepted (prompt-for-y-or-n-p
                                   (format nil "DISCARD all changes to ~A" file))))
                    (rlm-resume-spinner saved-spinner)
                    (cond
                      (accepted
                       (with-vcs (vcs)
                         (lem/porcelain:discard-file vcs file))
                       (bt2:with-lock-held (lock)
                         (setf result (format nil "Discarded changes to: ~A" file))
                         (bt2:condition-notify cv)))
                      (t
                       (bt2:with-lock-held (lock)
                         (setf result "SKIPPED: User declined discard.")
                         (bt2:condition-notify cv))))))
              (error (e)
                (bt2:with-lock-held (lock)
                  (setf result (format nil "Error: ~A" e))
                  (bt2:condition-notify cv))))))
         (bt2:with-lock-held (lock)
           (loop :until result
                 :do (bt2:condition-wait cv lock)))
         result)))))

;;; ============================================================
;;; Registration
;;; ============================================================

(defun rlm-legit-tools ()
  "Return list of all VCS tools for the RLM agent."
  (list (make-git-status-tool)
        (make-git-diff-tool)
        (make-git-stage-tool)
        (make-git-unstage-tool)
        (make-git-commit-tool)
        (make-git-log-tool)
        (make-git-show-tool)
        (make-git-branch-tool)
        (make-git-checkout-tool)
        (make-git-push-tool)
        (make-git-pull-tool)
        (make-git-discard-tool)))
