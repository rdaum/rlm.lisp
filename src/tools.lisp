(in-package :rlm/tools)

;;; Built-in tools for file system access and web reading.

(defun safe-path-p (path allowed-dirs)
  "Check that PATH is under one of ALLOWED-DIRS. Prevents directory traversal."
  (let ((truename (handler-case (truename (parse-namestring path))
                    (error () nil))))
    (when truename
      (let ((path-str (namestring truename)))
        (some (lambda (dir)
                (let ((dir-str (namestring (truename dir))))
                  (and (>= (length path-str) (length dir-str))
                       (string= dir-str path-str :end2 (length dir-str)))))
              allowed-dirs)))))

(defun make-list-directory-tool (&key (allowed-dirs (list (uiop:getcwd))))
  "Create a tool that lists directory contents.
ALLOWED-DIRS is a list of directory pathnames the tool may access.
Defaults to the current working directory."
  (rlm/repl:make-tool
   :name "list-directory"
   :description "List files and subdirectories in a directory. Returns one entry per line with type (FILE or DIR) and name."
   :parameter-doc "(call-tool \"list-directory\" path) — path is a directory path string."
   :execute-fn
   (lambda (path)
     (unless (safe-path-p path allowed-dirs)
       (error "Access denied: ~A is not within allowed directories" path))
     (let ((entries '()))
       (dolist (f (directory (merge-pathnames "*.*" (truename path))))
         (push (format nil "FILE ~A" (enough-namestring f (truename path)))
               entries))
       (dolist (d (directory (merge-pathnames "*/" (truename path))))
         (push (format nil "DIR  ~A" (enough-namestring d (truename path)))
               entries))
       (format nil "~{~A~%~}" (sort entries #'string<))))))

(defun make-read-file-tool (&key (allowed-dirs (list (uiop:getcwd))) (max-chars 100000))
  "Create a tool that reads file contents.
ALLOWED-DIRS is a list of directory pathnames the tool may access.
Defaults to the current working directory.
MAX-CHARS limits how much of the file to return."
  (rlm/repl:make-tool
   :name "read-file"
   :description "Read the contents of a text file. Returns the file content as a string."
   :parameter-doc "(call-tool \"read-file\" path) — path is a file path string. Optionally (call-tool \"read-file\" path :offset N :limit M) to read M characters starting at offset N."
   :execute-fn
   (lambda (path &key (offset 0) (limit max-chars))
     (unless (safe-path-p path allowed-dirs)
       (error "Access denied: ~A is not within allowed directories" path))
     (let ((content (handler-case
                        (with-open-file (in path :direction :input
                                                 :if-does-not-exist :error)
                          (let ((buf (make-string (min (file-length in) (+ offset limit)))))
                            (read-sequence buf in)
                            (subseq buf offset (min (length buf) (+ offset limit)))))
                      (error (e) (format nil "Error reading file: ~A" e)))))
       (if (> (length content) limit)
           (format nil "~A~%... [truncated at ~A chars, file has more]"
                   (subseq content 0 limit) limit)
           content)))))

;;; ============================================================
;;; Web reading via Jina Reader
;;; ============================================================

(defun make-web-read-tool (&key (max-chars 50000))
  "Create a tool that reads web pages as clean markdown via Jina Reader.
MAX-CHARS limits how much content to return."
  (rlm/repl:make-tool
   :name "web-read"
   :description "Read a web page and return its content as clean markdown. Works with any public URL — articles, docs, READMEs, etc."
   :parameter-doc "(call-tool \"web-read\" url) — url is a full URL string (e.g. \"https://example.com/page\")."
   :execute-fn
   (lambda (url)
     (unless (and (stringp url)
                  (or (alexandria:starts-with-subseq "http://" url)
                      (alexandria:starts-with-subseq "https://" url)))
       (error "Invalid URL: ~A — must start with http:// or https://" url))
     (let* ((jina-url (format nil "https://r.jina.ai/~A" url))
            (content
              (handler-case
                  (multiple-value-bind (body status-code)
                      (dex:get jina-url
                               :headers '(("Accept" . "text/markdown"))
                               :want-stream nil)
                    (let ((text (if (stringp body)
                                    body
                                    (flex:octets-to-string body :external-format :utf-8))))
                      (unless (<= 200 status-code 299)
                        (error "Jina Reader returned status ~A" status-code))
                      text))
                (error (e)
                  (format nil "Error reading URL: ~A" e)))))
       (if (> (length content) max-chars)
           (format nil "~A~%~%... [truncated at ~A chars, page has ~A total]"
                   (subseq content 0 max-chars) max-chars (length content))
           content)))))

;;; ============================================================
;;; URL encoding
;;; ============================================================

(defun url-encode (string)
  "Percent-encode STRING for use in URLs."
  (with-output-to-string (s)
    (loop :for byte :across (flex:string-to-octets string :external-format :utf-8)
          :do (if (or (<= (char-code #\A) byte (char-code #\Z))
                      (<= (char-code #\a) byte (char-code #\z))
                      (<= (char-code #\0) byte (char-code #\9))
                      (member byte (list (char-code #\-) (char-code #\_)
                                         (char-code #\.) (char-code #\~))))
                  (write-char (code-char byte) s)
                  (format s "%~2,'0X" byte)))))

;;; ============================================================
;;; Web search via Jina Search
;;; ============================================================

(defvar *jina-api-key* (uiop:getenv "JINA_API_KEY")
  "API key for Jina Search. Set via JINA_API_KEY env var or directly.")

(defun make-web-search-tool (&key (max-chars 20000) (api-key nil api-key-p))
  "Create a tool that searches the web via Jina Search.
MAX-CHARS limits how much of the results to return.
API-KEY overrides *jina-api-key* if provided."
  (rlm/repl:make-tool
   :name "web-search"
   :description "Search the web for a query. Returns results as markdown with titles, URLs, and snippets."
   :parameter-doc "(call-tool \"web-search\" query) — query is a search string (e.g. \"common lisp hash table performance\")."
   :execute-fn
   (lambda (query)
     (let ((key (if api-key-p api-key *jina-api-key*)))
       (unless key
         (error "No Jina API key. Set JINA_API_KEY env var or pass :api-key to make-web-search-tool"))
       (unless (and (stringp query) (plusp (length query)))
         (error "Search query must be a non-empty string"))
       (let* ((encoded-query (url-encode query))
              (search-url (format nil "https://s.jina.ai/~A" encoded-query))
              (content
                (handler-case
                    (multiple-value-bind (body status-code)
                        (dex:get search-url
                                 :headers `(("Accept" . "text/markdown")
                                            ("Authorization" . ,(format nil "Bearer ~A" key)))
                                 :want-stream nil)
                      (let ((text (if (stringp body)
                                      body
                                      (flex:octets-to-string body :external-format :utf-8))))
                        (unless (<= 200 status-code 299)
                          (error "Jina Search returned status ~A: ~A" status-code
                                 (subseq text 0 (min 200 (length text)))))
                        text))
                  (error (e)
                    (format nil "Error searching: ~A" e)))))
         (if (> (length content) max-chars)
             (format nil "~A~%~%... [truncated at ~A chars]"
                     (subseq content 0 max-chars) max-chars)
             content))))))
