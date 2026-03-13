(in-package :rlm/sandbox)

;;; Sandbox: restricted CL evaluation with controlled symbol access.
;;;
;;; Strategy:
;;; 1. A restricted package (:rlm-user) imports only safe CL symbols
;;; 2. A custom readtable disables #. (read-time eval)
;;; 3. A form-walker post-READ rejects symbols from disallowed packages
;;; 4. Eval with output capture

;;; ============================================================
;;; Allowed symbols — curated subset of CL
;;; ============================================================

(defparameter *allowed-cl-symbols*
  '(;; Arithmetic
    + - * / mod rem floor ceiling truncate round
    abs max min expt sqrt log exp
    sin cos tan asin acos atan
    1+ 1- zerop plusp minusp evenp oddp
    float rational rationalize
    numerator denominator
    gcd lcm
    ;; Comparison
    = /= < > <= >=
    eq eql equal equalp
    ;; Logic
    not and or
    ;; Type predicates
    null atom consp listp numberp integerp floatp stringp symbolp
    characterp functionp hash-table-p arrayp vectorp
    typep subtypep
    ;; Cons / list
    cons car cdr caar cadr cdar cddr
    first second third fourth fifth sixth seventh eighth ninth tenth
    rest last butlast nthcdr
    list list* append nconc revappend nreconc
    copy-list copy-tree
    nth elt length
    member assoc rassoc getf get-properties
    find find-if find-if-not
    position position-if position-if-not
    count count-if count-if-not
    remove remove-if remove-if-not delete delete-if delete-if-not
    substitute substitute-if nsubstitute nsubstitute-if
    sort stable-sort merge
    reverse nreverse
    mapcar mapcan mapc mapl maplist
    reduce
    every some notevery notany
    acons pairlis
    subst subst-if nsubst nsubst-if
    intersection union set-difference set-exclusive-or
    subsetp
    push pop
    ;; Sequences
    subseq concatenate map map-into
    search mismatch
    fill replace
    remove-duplicates delete-duplicates
    coerce
    make-sequence
    ;; Strings
    string string= string/= string< string> string<= string>=
    string-equal string-not-equal
    string-upcase string-downcase string-capitalize
    string-trim string-left-trim string-right-trim
    char char-code code-char char-name char-upcase char-downcase
    char= char/= char< char> char<= char>=
    parse-integer
    ;; Format / output (to string only)
    format write-to-string prin1-to-string princ-to-string
    print prin1 princ terpri fresh-line
    write-char write-string write-line
    ;; Hash tables
    make-hash-table gethash remhash maphash hash-table-count
    ;; Arrays / vectors
    make-array aref array-dimensions array-dimension
    array-total-size array-rank
    vector vector-push vector-push-extend vector-pop
    make-string
    ;; Control flow
    if when unless cond case ecase typecase etypecase
    progn prog1 prog2
    block return return-from
    tagbody go
    loop do do* dotimes dolist
    ;; Binding
    let let* flet labels
    multiple-value-bind multiple-value-list values
    destructuring-bind
    setf setq incf decf
    ;; Functions
    function defun lambda funcall apply
    complement constantly identity
    ;; Local definitions
    defvar defparameter defconstant
    ;; Multiple values
    values multiple-value-bind multiple-value-list
    nth-value
    ;; Conditions (limited)
    error warn handler-case ignore-errors
    ;; Type conversion
    coerce
    parse-integer read-from-string
    ;; Type introspection
    type-of
    ;; Misc
    gensym quote
    with-output-to-string make-string-output-stream
    get-output-stream-string
    with-input-from-string make-string-input-stream
    ;; T and NIL
    t nil
    ;; String streams for output capture
    *standard-output* *error-output*
    ))

;;; ============================================================
;;; Restricted readtable
;;; ============================================================

(defun make-restricted-readtable ()
  "Create a readtable that disables #. (read-time eval)."
  (let ((rt (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\.
      (lambda (stream char arg)
        (declare (ignore stream char arg))
        (error "Read-time evaluation (#.) is disabled in sandbox"))
      rt)
    rt))

;;; ============================================================
;;; Form walker — reject disallowed symbols
;;; ============================================================

(defun symbol-allowed-p (sym allowed-packages)
  "Check if SYM is allowed in the sandbox."
  (let ((pkg (symbol-package sym)))
    (cond
      ((null pkg) t) ; uninterned symbols are fine
      ;; CL package: only allow specifically imported symbols
      ((eq pkg (find-package :cl))
       (member (symbol-name sym)
               *allowed-cl-symbols*
               :test #'string=
               :key #'string))
      ;; Keyword package: always allowed
      ((eq pkg (find-package :keyword)) t)
      ;; User package: allowed
      ((member pkg allowed-packages) t)
      ;; Everything else: blocked
      (t nil))))

(defun check-form-safety (form allowed-packages)
  "Walk FORM and error if any symbol belongs to a disallowed package or is a non-allowed CL symbol."
  (labels ((walk (x)
             (typecase x
               (symbol
                (unless (symbol-allowed-p x allowed-packages)
                  (error "Sandbox: symbol ~A from package ~A is not allowed"
                         x (package-name (symbol-package x)))))
               (cons
                (walk (car x))
                (walk (cdr x))))))
    (walk form)))

;;; ============================================================
;;; Sandbox
;;; ============================================================

(defclass sandbox ()
  ((user-package :accessor sandbox-user-package :initform nil)
   (allowed-packages :accessor sandbox-allowed-packages :initform '())
   (readtable :accessor sandbox-readtable)
   (variables :accessor sandbox-variables
              :initform (make-hash-table :test 'equal))
   (injected-functions :accessor sandbox-injected-functions
                       :initform (make-hash-table :test 'equal))
   (timeout :accessor sandbox-timeout :initarg :timeout :initform 30)))

(defclass sandbox-result ()
  ((stdout :accessor sandbox-result-stdout :initarg :stdout :initform "")
   (stderr :accessor sandbox-result-stderr :initarg :stderr :initform "")
   (values :accessor sandbox-result-values :initarg :values :initform nil)
   (error :accessor sandbox-result-error :initarg :error :initform nil)
   (final-answer :accessor sandbox-result-final-answer :initarg :final-answer :initform nil)))

(defun make-sandbox (&key (timeout 30))
  "Create a new sandbox with restricted readtable."
  (let ((sb (make-instance 'sandbox :timeout timeout)))
    (setf (sandbox-readtable sb) (make-restricted-readtable))
    sb))

(defun inject-function (sandbox name fn)
  "Register FN under NAME so it's available in sandboxed code."
  (setf (gethash name (sandbox-injected-functions sandbox)) fn))

(defun set-variable (sandbox name value)
  "Set a variable in the sandbox namespace."
  (setf (gethash name (sandbox-variables sandbox)) value))

(defun ensure-user-package (sandbox)
  "Create the :rlm-user package if needed, and sync variable bindings."
  (unless (sandbox-user-package sandbox)
    ;; First time — create package and import safe symbols
    (when (find-package :rlm-user)
      (delete-package :rlm-user))
    (let ((pkg (make-package :rlm-user :use nil)))
      ;; Import allowed CL symbols
      (dolist (sym-name *allowed-cl-symbols*)
        (multiple-value-bind (sym found-p) (find-symbol (string sym-name) :cl)
          (when found-p
            (import (list sym) pkg))))
      ;; Set up injected functions
      (maphash (lambda (name fn)
                 (let ((sym (intern (string-upcase name) pkg)))
                   (setf (symbol-function sym) fn)))
               (sandbox-injected-functions sandbox))
      (setf (sandbox-user-package sandbox) pkg)
      (setf (sandbox-allowed-packages sandbox)
            (list pkg (find-package :cl) (find-package :keyword)))))
  ;; Every time — sync variable bindings
  (let ((pkg (sandbox-user-package sandbox)))
    (maphash (lambda (name value)
               (let ((sym (intern (string-upcase name) pkg)))
                 (proclaim `(special ,sym))
                 (setf (symbol-value sym) value)))
             (sandbox-variables sandbox))
    pkg))

(defun sync-variables-back (sandbox)
  "After eval, sync user-defined variables back to the sandbox."
  (let ((pkg (sandbox-user-package sandbox)))
    (when pkg
      (do-symbols (sym pkg)
        (when (and (eq (symbol-package sym) pkg)
                   (boundp sym)
                   (not (fboundp sym)))
          (setf (gethash (string-downcase (symbol-name sym))
                         (sandbox-variables sandbox))
                (symbol-value sym)))))))

;;; ============================================================
;;; Eval with capture
;;; ============================================================

(defun sandbox-eval (sandbox code-string)
  "Read and evaluate CODE-STRING in the sandbox. Returns a SANDBOX-RESULT."
  (ensure-user-package sandbox)
  (let ((stdout-str "")
        (stderr-str "")
        (result-values nil)
        (error-msg nil)
        (final-answer nil))
    ;; Read all forms, tolerating trailing junk (e.g. extra close-parens from LLM)
    (let ((forms
            (handler-case
                (let ((*readtable* (sandbox-readtable sandbox))
                      (*package* (sandbox-user-package sandbox))
                      (*read-eval* nil))
                  (with-input-from-string (in code-string)
                    (let ((collected nil))
                      (loop :for form := (handler-case (read in nil :eof)
                                           (reader-error (e)
                                             ;; If we have forms already, ignore trailing junk
                                             (if collected
                                                 (return (nreverse collected))
                                                 (error e))))
                            :until (eq form :eof)
                            :do (check-form-safety form (sandbox-allowed-packages sandbox))
                            :do (push form collected)
                            :finally (return (nreverse collected))))))
              (error (e)
                (setf error-msg (format nil "Read error: ~A" e))
                nil))))
      ;; Eval all forms with output capture
      (when (and forms (not error-msg))
        (let ((stdout-stream (make-string-output-stream))
              (stderr-stream (make-string-output-stream)))
          (handler-case
              (let ((*standard-output* stdout-stream)
                    (*error-output* stderr-stream)
                    (*package* (sandbox-user-package sandbox)))
                (handler-bind ((warning #'muffle-warning))
                  (dolist (form forms)
                    (setf result-values
                          (multiple-value-list (eval form))))))
            (error (e)
              (setf error-msg (format nil "~A" e))))
          (setf stdout-str (get-output-stream-string stdout-stream))
          (setf stderr-str (get-output-stream-string stderr-stream))))
      ;; Check for final-answer
      (let ((fa-sym (find-symbol "FINAL-ANSWER"
                                 (sandbox-user-package sandbox))))
        (when (and fa-sym (boundp fa-sym))
          (setf final-answer (princ-to-string (symbol-value fa-sym))))))
    ;; Sync variables back
    (sync-variables-back sandbox)
    (make-instance 'sandbox-result
                   :stdout stdout-str
                   :stderr stderr-str
                   :values result-values
                   :error error-msg
                   :final-answer final-answer)))
