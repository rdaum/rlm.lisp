(in-package :rlm/client)

;;; Configuration

(defvar *api-key* (uiop:getenv "OPENROUTER_API_KEY")
  "API key for OpenRouter (or any OpenAI-compatible endpoint).")

(defvar *api-base-url* "https://openrouter.ai/api/v1"
  "Base URL for the API endpoint.")

(defvar *model* "qwen/qwen3.5-397b-a17b"
  "Default model to use.")

;;; Response accessors

(defun completion-content (response)
  "Extract the content string from a chat completion response.
Returns the content string, or signals an error with diagnostic info on unexpected structure."
  (let ((choices (gethash "choices" response)))
    (unless choices
      (error "No 'choices' in response. Keys: ~A"
             (let ((keys '())) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) response) keys)))
    (let ((first-choice (etypecase choices
                          (vector (when (plusp (length choices)) (aref choices 0)))
                          (list (first choices)))))
      (unless (hash-table-p first-choice)
        (error "choices[0] is ~A, not a hash-table" (type-of first-choice)))
      (let ((message (gethash "message" first-choice)))
        (unless (hash-table-p message)
          (error "choices[0].message is ~A (~S), not a hash-table. Keys: ~A"
                 (type-of message) message
                 (let ((keys '())) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) first-choice) keys)))
        (gethash "content" message)))))

(defun completion-usage (response)
  "Extract usage stats as (prompt-tokens completion-tokens total-tokens)."
  (let ((usage (gethash "usage" response)))
    (when usage
      (list :prompt-tokens (gethash "prompt_tokens" usage 0)
            :completion-tokens (gethash "completion_tokens" usage 0)
            :total-tokens (gethash "total_tokens" usage 0)))))

;;; Models

(defvar *models-cache* nil
  "Cached list of (id . name) pairs from the API.")

(defvar *models-cache-time* 0
  "Universal time when *models-cache* was last populated.")

(defun list-models (&key (base-url *api-base-url*) (max-age 300))
  "Fetch available models from the API. Returns a list of model ID strings.
Caches results for MAX-AGE seconds (default 5 minutes)."
  (when (and *models-cache*
             (< (- (get-universal-time) *models-cache-time*) max-age))
    (return-from list-models *models-cache*))
  (let* ((url (format nil "~A/models" base-url))
         (response-body
           (handler-case
               (dex:get url :want-stream nil
                        :headers (when *api-key*
                                   `(("Authorization" . ,(format nil "Bearer ~A" *api-key*)))))
             (error (e)
               (warn "Failed to fetch models: ~A" e)
               (return-from list-models *models-cache*))))
         (response-string (if (stringp response-body)
                              response-body
                              (flex:octets-to-string response-body :external-format :utf-8)))
         (parsed (yason:parse response-string))
         (data (gethash "data" parsed)))
    (when data
      (let ((models (loop :for m :across (if (vectorp data) data (coerce data 'vector))
                          :for id := (gethash "id" m)
                          :when id
                          :collect id)))
        (setf *models-cache* (sort models #'string<))
        (setf *models-cache-time* (get-universal-time))
        *models-cache*))))

;;; API call

(defun chat-completion (messages &key (model *model*) (api-key *api-key*)
                                      (base-url *api-base-url*))
  "Send a chat completion request. MESSAGES is a list of (:role \"...\" :content \"...\") plists.
Returns the parsed JSON response as a hash-table."
  (unless api-key
    (error "No API key configured. Set RLM/CLIENT:*API-KEY* or OPENROUTER_API_KEY env var."))
  (let* ((url (format nil "~A/chat/completions" base-url))
         (message-array
           (mapcar (lambda (msg)
                     (let ((ht (make-hash-table :test 'equal)))
                       (setf (gethash "role" ht) (getf msg :role))
                       (setf (gethash "content" ht) (getf msg :content))
                       ht))
                   messages))
         (body (let ((ht (make-hash-table :test 'equal)))
                 (setf (gethash "model" ht) model)
                 (setf (gethash "messages" ht) (coerce message-array 'vector))
                 ht))
         (body-string (yason:with-output-to-string* (:stream-symbol s)
                        (yason:encode body s))))
    (multiple-value-bind (response-body status-code)
        (dex:post url
                  :content body-string
                  :headers `(("Content-Type" . "application/json")
                             ("Authorization" . ,(format nil "Bearer ~A" api-key)))
                  :want-stream nil)
      (let ((response-string (if (stringp response-body)
                                 response-body
                                 (flex:octets-to-string response-body :external-format :utf-8))))
        (unless (<= 200 status-code 299)
          (error "API request failed with status ~A: ~A" status-code response-string))
        (let ((parsed (yason:parse response-string)))
          ;; Some providers return errors with HTTP 200
          (when (and (hash-table-p parsed)
                     (gethash "error" parsed)
                     (not (gethash "choices" parsed)))
            (let ((err (gethash "error" parsed)))
              (error "API error: ~A"
                     (if (hash-table-p err)
                         (or (gethash "message" err) err)
                         err))))
          parsed)))))
