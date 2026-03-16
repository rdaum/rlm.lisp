(defpackage :rlm-tests/legit-tools
  (:use :cl :rove)
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :rlm-tests/legit-tools)

;;; Tests for the RLM legit (git) tools.
;;; These run against a real temp git repo but don't need the Lem UI —
;;; tool execute-fns are called directly.

;;; ============================================================
;;; Temp git repo helper
;;; ============================================================

(defun call-with-temp-git-repo (function)
  "Create a temp git repo, run FUNCTION with CWD inside it, then clean up."
  (let ((temp-dir (uiop:ensure-directory-pathname
                   (format nil "~Arlm-test-~A/"
                           (uiop:temporary-directory)
                           (get-universal-time)))))
    (unwind-protect
         (progn
           (ensure-directories-exist temp-dir)
           (uiop:with-current-directory (temp-dir)
             (uiop:run-program '("git" "init") :ignore-error-status t)
             (uiop:run-program '("git" "config" "user.email" "test@test.com")
                               :ignore-error-status t)
             (uiop:run-program '("git" "config" "user.name" "Test")
                               :ignore-error-status t)
             ;; Initial commit
             (with-open-file (s (merge-pathnames "README.md" temp-dir)
                                :direction :output :if-exists :supersede)
               (write-string "# Test Repo" s))
             (uiop:run-program '("git" "add" ".") :ignore-error-status t)
             (uiop:run-program '("git" "commit" "-m" "Initial commit")
                               :ignore-error-status t)
             (funcall function temp-dir)))
      (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore))))

(defmacro with-temp-git-repo ((dir-var) &body body)
  `(call-with-temp-git-repo (lambda (,dir-var) ,@body)))

;;; ============================================================
;;; Helper to call a tool's execute-fn
;;; ============================================================

(defun find-tool (tools name)
  "Find a tool by name in a list of tools."
  (find name tools :key #'rlm/repl:tool-name :test #'string=))

(defun call-tool (tool &rest args)
  "Call a tool's execute-fn with ARGS."
  (apply (rlm/repl:tool-execute-fn tool) args))

;;; ============================================================
;;; Tests
;;; ============================================================

(deftest git-status/shows-branch-and-files
  (testing "git-status returns branch name and file lists"
    (with-fake-interface ()
      (with-temp-git-repo (dir)
        ;; Set up RLM working directory
        (let ((lem-rlm-mode::*rlm-working-directory* dir))
          (let* ((tools (lem-rlm-mode::rlm-legit-tools))
                 (status-tool (find-tool tools "git-status"))
                 (result (call-tool status-tool)))
            (ok (search "Branch:" result))
            (ok (search "Staged" result))
            (ok (search "Unstaged" result))
            (ok (search "Untracked" result))))))))

(deftest git-status/detects-untracked-files
  (testing "git-status shows new untracked files"
    (with-fake-interface ()
      (with-temp-git-repo (dir)
        (let ((lem-rlm-mode::*rlm-working-directory* dir))
          ;; Create an untracked file
          (with-open-file (s (merge-pathnames "new-file.txt" dir)
                              :direction :output)
            (write-string "hello" s))
          (let* ((tools (lem-rlm-mode::rlm-legit-tools))
                 (result (call-tool (find-tool tools "git-status"))))
            (ok (search "new-file.txt" result))))))))

(deftest git-status/detects-modified-files
  (testing "git-status shows modified unstaged files"
    (with-fake-interface ()
      (with-temp-git-repo (dir)
        (let ((lem-rlm-mode::*rlm-working-directory* dir))
          ;; Modify a tracked file
          (with-open-file (s (merge-pathnames "README.md" dir)
                              :direction :output :if-exists :supersede)
            (write-string "# Modified" s))
          (let* ((tools (lem-rlm-mode::rlm-legit-tools))
                 (result (call-tool (find-tool tools "git-status"))))
            (ok (search "README.md" result))
            (ok (search "MODIFIED" (string-upcase result)))))))))

(deftest git-stage/stages-file
  (testing "git-stage stages a file and it appears in staged list"
    (with-fake-interface ()
      (with-temp-git-repo (dir)
        (let ((lem-rlm-mode::*rlm-working-directory* dir))
          ;; Modify a file
          (with-open-file (s (merge-pathnames "README.md" dir)
                              :direction :output :if-exists :supersede)
            (write-string "# Staged change" s))
          (let* ((tools (lem-rlm-mode::rlm-legit-tools))
                 (stage-tool (find-tool tools "git-stage"))
                 (status-tool (find-tool tools "git-status")))
            ;; Stage it
            (let ((result (call-tool stage-tool "README.md")))
              (ok (search "Staged" result)))
            ;; Verify it shows as staged
            (let ((status (call-tool status-tool)))
              (ok (search "MODIFIED" (string-upcase status))))))))))

(deftest git-unstage/unstages-file
  (testing "git-unstage reverses a stage operation"
    (with-fake-interface ()
      (with-temp-git-repo (dir)
        (let ((lem-rlm-mode::*rlm-working-directory* dir))
          (with-open-file (s (merge-pathnames "README.md" dir)
                              :direction :output :if-exists :supersede)
            (write-string "# Will unstage" s))
          (let* ((tools (lem-rlm-mode::rlm-legit-tools))
                 (stage-tool (find-tool tools "git-stage"))
                 (unstage-tool (find-tool tools "git-unstage")))
            (call-tool stage-tool "README.md")
            (let ((result (call-tool unstage-tool "README.md")))
              (ok (search "Unstaged" result)))))))))

(deftest git-diff/shows-changes
  (testing "git-diff returns diff output for modified file"
    (with-fake-interface ()
      (with-temp-git-repo (dir)
        (let ((lem-rlm-mode::*rlm-working-directory* dir))
          (with-open-file (s (merge-pathnames "README.md" dir)
                              :direction :output :if-exists :supersede)
            (write-string "# Changed content" s))
          (let* ((tools (lem-rlm-mode::rlm-legit-tools))
                 (diff-tool (find-tool tools "git-diff"))
                 (result (call-tool diff-tool "README.md")))
            (ok (search "Changed content" result))
            (ok (search "diff" result))))))))

(deftest git-diff/staged-changes
  (testing "git-diff with :cached shows staged changes"
    (with-fake-interface ()
      (with-temp-git-repo (dir)
        (let ((lem-rlm-mode::*rlm-working-directory* dir))
          (with-open-file (s (merge-pathnames "README.md" dir)
                              :direction :output :if-exists :supersede)
            (write-string "# Staged diff" s))
          (uiop:run-program '("git" "add" "README.md") :ignore-error-status t)
          (let* ((tools (lem-rlm-mode::rlm-legit-tools))
                 (diff-tool (find-tool tools "git-diff"))
                 (result (call-tool diff-tool "README.md" :cached t)))
            (ok (search "Staged diff" result))))))))

(deftest git-commit/creates-commit
  (testing "git-commit in yeet mode creates a commit"
    (with-fake-interface ()
      (with-temp-git-repo (dir)
        (let ((lem-rlm-mode::*rlm-working-directory* dir)
              (lem-rlm-mode::*rlm-yeet-mode* t))
          ;; Stage a change
          (with-open-file (s (merge-pathnames "README.md" dir)
                              :direction :output :if-exists :supersede)
            (write-string "# Committed" s))
          (uiop:run-program '("git" "add" "README.md") :ignore-error-status t)
          (let* ((tools (lem-rlm-mode::rlm-legit-tools))
                 (commit-tool (find-tool tools "git-commit"))
                 (result (call-tool commit-tool "Test commit message")))
            (ok (search "Committed" result))
            ;; Verify via git log
            (let ((log-tool (find-tool tools "git-log")))
              (let ((log (call-tool log-tool)))
                (ok (search "Test commit message" log))))))))))

(deftest git-log/shows-commits
  (testing "git-log returns commit history"
    (with-fake-interface ()
      (with-temp-git-repo (dir)
        (let ((lem-rlm-mode::*rlm-working-directory* dir))
          (let* ((tools (lem-rlm-mode::rlm-legit-tools))
                 (log-tool (find-tool tools "git-log"))
                 (result (call-tool log-tool)))
            (ok (search "Initial commit" result))))))))

(deftest git-branches/lists-branches
  (testing "git-branches shows current branch"
    (with-fake-interface ()
      (with-temp-git-repo (dir)
        (let ((lem-rlm-mode::*rlm-working-directory* dir))
          (let* ((tools (lem-rlm-mode::rlm-legit-tools))
                 (branch-tool (find-tool tools "git-branches"))
                 (result (call-tool branch-tool)))
            ;; Should show at least one branch with asterisk
            (ok (search "*" result))))))))

(deftest git-show/shows-commit-diff
  (testing "git-show returns diff for a commit ref"
    (with-fake-interface ()
      (with-temp-git-repo (dir)
        (let ((lem-rlm-mode::*rlm-working-directory* dir))
          (let* ((tools (lem-rlm-mode::rlm-legit-tools))
                 (show-tool (find-tool tools "git-show"))
                 (result (call-tool show-tool "HEAD")))
            (ok (search "README" result))))))))

(deftest git-checkout/switches-branch
  (testing "git-checkout switches to a new branch"
    (with-fake-interface ()
      (with-temp-git-repo (dir)
        (let ((lem-rlm-mode::*rlm-working-directory* dir)
              (lem-rlm-mode::*rlm-yeet-mode* t))
          ;; Create a branch first
          (uiop:run-program '("git" "branch" "test-branch") :ignore-error-status t)
          (let* ((tools (lem-rlm-mode::rlm-legit-tools))
                 (checkout-tool (find-tool tools "git-checkout"))
                 (result (call-tool checkout-tool "test-branch")))
            (ok (search "Switched" result))
            ;; Verify we're on the new branch
            (let ((status (call-tool (find-tool tools "git-status"))))
              (ok (search "test-branch" status)))))))))

(deftest git-stage-and-commit/full-workflow
  (testing "full workflow: modify, stage, commit, verify in log"
    (with-fake-interface ()
      (with-temp-git-repo (dir)
        (let ((lem-rlm-mode::*rlm-working-directory* dir)
              (lem-rlm-mode::*rlm-yeet-mode* t))
          (let* ((tools (lem-rlm-mode::rlm-legit-tools)))
            ;; Create new file
            (with-open-file (s (merge-pathnames "feature.lisp" dir)
                                :direction :output)
              (write-string "(defun hello () :world)" s))
            ;; Stage
            (call-tool (find-tool tools "git-stage") "feature.lisp")
            ;; Commit
            (call-tool (find-tool tools "git-commit") "Add feature.lisp")
            ;; Verify in log
            (let ((log (call-tool (find-tool tools "git-log"))))
              (ok (search "Add feature.lisp" log)))
            ;; Verify show works on new commit
            (let ((show (call-tool (find-tool tools "git-show") "HEAD")))
              (ok (search "hello" show)))))))))
