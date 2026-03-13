(in-package :rlm/prompts)

(defvar *system-prompt-base*
  "You are tasked with answering a query with associated context. You interact with a Common Lisp REPL environment to explore, transform, and analyze the context. You will be queried iteratively until you provide a final answer.

The REPL environment provides:

1. A CONTEXT variable containing the data you need to analyze. Examine it to understand what you're working with.

1b. A *HISTORY* variable: list of prior exchanges. Each entry is a plist with :QUESTION, :ANSWER, and :WORK-LOG keys. Access with (GETF entry :QUESTION), (GETF entry :ANSWER), (GETF entry :WORK-LOG). The work-log contains the reasoning and exploration from that query. NIL on first query.

1c. A *LAST-ANSWER* variable: shortcut for the most recent answer. Reference it instead of re-deriving prior work.

2. (LLM-QUERY prompt) — makes a sub-LLM call for extraction, summarization, or Q&A over text chunks. The sub-LLM can handle ~500K characters. Returns a string.

3. (SHOW-VARS) — returns a description of all variables in the REPL environment.

4. (FINAL value) — call this when you have your answer. This signals completion.

5. (FINAL-VAR variable-name-string) — return an existing variable's value as the final answer.

6. (CALL-TOOL tool-name &rest args) — invoke a registered tool by name. Returns the tool's result as a string. Use PRINT or FORMAT to see the output, or capture it with DEFVAR.

7. (LIST-TOOLS) — show all available tools and their descriptions.

Text processing functions (available directly, no tool call needed):
- (SPLIT-STRING str delimiter) — split string on regex delimiter, returns list. Default delimiter: whitespace.
- (REGEX-MATCH pattern str) — first match + groups as (match group1 ...), or NIL.
- (REGEX-SCAN pattern str) — all matches as list of strings.
- (REGEX-REPLACE pattern str replacement) — global regex replace.
- (WORDS str) — split on whitespace, filter empties.
- (LINES str) — split on newlines.
- (JOIN-STRINGS list separator) — join list into string. Default separator: space.
- (WORD-FREQUENCY str) — alist of (word . count), sorted by frequency descending.

You write Common Lisp code in ```lisp fenced blocks. The code executes in a sandboxed environment with standard CL arithmetic, list/sequence operations, string operations, hash tables, and control flow. No file I/O, no FFI, no network access — but registered tools can perform external actions on your behalf.

**Strategy:** Break problems into digestible pieces. Use LLM-QUERY to analyze chunks of context that are too large to reason about directly. Use CALL-TOOL to interact with external systems. Build up intermediate results in variables, then combine them for your final answer.

**Example — analyzing a large text:**
```lisp
;; Check what we have
(format t \"Context type: ~A, length: ~A~%\" (type-of context) (length context))
```

Then in the next iteration:
```lisp
;; Chunk and analyze
(let* ((chunk (subseq context 0 (min 10000 (length context))))
       (analysis (llm-query (format nil \"Summarize the key points: ~A\" chunk))))
  (defvar *summary* analysis)
  (format t \"Summary: ~A~%\" analysis))
```

And when ready:
```lisp
(final (format nil \"Based on my analysis: ~A\" *summary*))
```

**Important rules:**
- Always explore the context before answering
- Use (FINAL value) when done — do NOT just state your answer in text
- Use PRINT/FORMAT to see intermediate results
- Variables persist across iterations
- Keep code blocks SHORT. Never embed large text as string literals — your output will be truncated. Instead, build answers from variables: (FINAL (format nil \"Summary: ~A\" *my-var*))
- LLM-QUERY is powerful — use it for semantic analysis of text chunks
- CALL-TOOL lets you interact with external systems via registered tools

Think step by step, write code, observe results, and iterate toward your answer."
  "Base system prompt for the RLM agent loop.")

(defun format-tools-section (tools)
  "Format a list of tool objects into a prompt section describing available tools.
TOOLS is a list of rlm/repl:tool instances."
  (if (null tools)
      ""
      (with-output-to-string (s)
        (format s "~%~%## Available Tools~%~%")
        (format s "Use (CALL-TOOL \"tool-name\" arg1 arg2 ...) to invoke these:~%~%")
        (dolist (tool tools)
          (format s "### ~A~%~A~%" (rlm/repl:tool-name tool) (rlm/repl:tool-description tool))
          (when (and (rlm/repl:tool-parameter-doc tool)
                     (plusp (length (rlm/repl:tool-parameter-doc tool))))
            (format s "Parameters: ~A~%" (rlm/repl:tool-parameter-doc tool)))
          (format s "~%")))))

(defun format-session-state (variables)
  "Format existing sandbox variables into a prompt section.
VARIABLES is an alist of (name . value)."
  (if (null variables)
      ""
      (with-output-to-string (s)
        (format s "~%~%## Session State~%~%")
        (format s "The REPL environment has variables from previous queries. You can use them directly:~%~%")
        (dolist (pair variables)
          (let* ((name (car pair))
                 (val (cdr pair))
                 (val-str (princ-to-string val))
                 (type-str (princ-to-string (type-of val))))
            ;; Show type and a preview of the value
            (if (> (length val-str) 200)
                (format s "- ~A (~A, ~A chars): ~A...~%"
                        name type-str (length val-str) (subseq val-str 0 200))
                (format s "- ~A (~A): ~A~%" name type-str val-str)))))))

(defun build-system-message (&optional tools session-variables)
  "Build the system message for the chat.
TOOLS is an optional list of tool objects.
SESSION-VARIABLES is an optional alist of (name . value) from prior queries."
  (list :role "system" :content (concatenate 'string
                                             *system-prompt-base*
                                             (format-tools-section tools)
                                             (format-session-state session-variables))))

(defun format-conversation-history (history)
  "Format prior history entries as user/assistant message pairs.
HISTORY is a list of plists with :question, :answer, and optional :work-log.
Also supports legacy (question . answer) conses.
Returns a list of message plists to splice into the conversation."
  (when history
    (loop :for entry :in history
          :for question := (if (consp entry)
                               (if (keywordp (car entry))
                                   (getf entry :question)
                                   (car entry))
                               entry)
          :for answer := (if (consp entry)
                             (if (keywordp (car entry))
                                 (getf entry :answer)
                                 (cdr entry))
                             "")
          :for work-log := (when (and (consp entry) (keywordp (car entry)))
                             (getf entry :work-log))
          :collect (list :role "user" :content question)
          :collect (list :role "assistant" :content
                         (if work-log
                             (format nil "~A~%~%Final answer:~%```lisp~%(final ~S)~%```"
                                     work-log answer)
                             (format nil "```lisp~%(final ~S)~%```" answer))))))

(defun build-context-metadata-message (context)
  "Build a message describing the context metadata."
  (let ((metadata
          (typecase context
            (string (format nil "Your context is a string with ~A characters."
                            (length context)))
            (list (format nil "Your context is a list with ~A elements, total ~A characters."
                          (length context)
                          (reduce #'+ context
                                  :key (lambda (x) (length (princ-to-string x))))))
            (t (format nil "Your context is a ~A." (type-of context))))))
    (list :role "assistant" :content metadata)))

(defun build-iteration-zero-message (question &key follow-up)
  "Build the first user message with the question.
When FOLLOW-UP is true, frames this as a new question in an ongoing conversation."
  (list :role "user"
        :content (if follow-up
                     (format nil "New question (answer THIS, not any previous question):

~A

Write Common Lisp code in ```lisp blocks. Use CALL-TOOL to interact with external resources, PRINT/FORMAT to see results." question)
                     (format nil "This is your first iteration. Use your tools and the REPL to work toward an answer.

~A

Write Common Lisp code in ```lisp blocks. Use CALL-TOOL to interact with external resources, PRINT/FORMAT to see results." question))))

(defun build-iteration-continue-message (question)
  "Build the continuation prompt for subsequent iterations."
  (list :role "user"
        :content (format nil "The above shows your previous REPL interactions. Continue exploring the context to answer: ~A

Write more ```lisp code, or call (FINAL answer) when ready." question)))

(defun build-fallback-message ()
  "Build the message for when max iterations are reached."
  (list :role "assistant"
        :content "I've reached the maximum number of iterations. Let me provide my best answer based on what I've gathered so far."))
