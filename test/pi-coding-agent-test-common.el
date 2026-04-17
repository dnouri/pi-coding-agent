;;; pi-coding-agent-test-common.el --- Shared test utilities and configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Common definitions shared across pi-coding-agent test files.
;; Centralizes timeout values, fake-pi launch helpers, the mock-session
;; macro, and toolcall streaming helpers for easy adjustment (e.g., slow CI).

;;; Code:

(require 'cl-lib) ; for cl-letf in mock-session macro
(require 'ert)

;;; Timeout Configuration

(defvar pi-coding-agent-test-short-wait 0.5
  "Short wait in seconds for async operations to complete.")

(defvar pi-coding-agent-test-poll-interval 0.1
  "Polling interval in seconds for waiting loops.")

(defvar pi-coding-agent-test-rpc-timeout 10
  "Timeout in seconds for RPC calls in tests.")

(defvar pi-coding-agent-test-integration-timeout 600
  "Timeout in seconds for real-backend integration tests.
The shared contract still includes multi-turn model interactions, so the real
lane keeps a generous timeout for slower CI runners.")

(defvar pi-coding-agent-test-gui-timeout 180
  "Timeout in seconds for GUI tests.
The GUI suite is fake-backed, but it still waits for real Emacs window updates,
redisplay, and subprocess event delivery.")

;;;; Formatting Helpers

(defun pi-coding-agent-test-format-elapsed (seconds)
  "Format SECONDS as a human-readable duration with millisecond precision."
  (format "%.3fs" (float seconds)))

;;;; Fake-pi Helpers

(defconst pi-coding-agent-test-fake-pi-script
  (expand-file-name "support/fake_pi.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the fake-pi harness script.")

(defun pi-coding-agent-test-python-executable ()
  "Return a Python executable path, or skip if none is available."
  (or (executable-find "python3")
      (executable-find "python")
      (ert-skip "python3 or python not found")))

(defun pi-coding-agent-test-fake-pi-executable ()
  "Return the command list for launching the fake-pi harness.
Uses python3 in the test suite so local and CI runs do not need a
separate uv dependency, while the script itself still keeps uv metadata
and an executable shebang for manual runs."
  (list (pi-coding-agent-test-python-executable)
        pi-coding-agent-test-fake-pi-script))

(defun pi-coding-agent-test-fake-pi-extra-args (scenario &optional extra-args)
  "Return fake-pi CLI args for SCENARIO plus optional EXTRA-ARGS."
  (append (list "--scenario" scenario) extra-args))

(defun pi-coding-agent-test-backend-spec
    (backend default-fake-scenario &optional fake-scenario fake-extra-args)
  "Return shared backend launch data for BACKEND.
DEFAULT-FAKE-SCENARIO is used when BACKEND is `fake' and FAKE-SCENARIO is
nil.  FAKE-EXTRA-ARGS are appended after the generated fake-pi scenario args."
  (pcase backend
    ('fake
     (let* ((scenario (or fake-scenario default-fake-scenario))
            (extra-args (pi-coding-agent-test-fake-pi-extra-args
                         scenario fake-extra-args)))
       (list :name 'fake
             :label (format "fake:%s" scenario)
             :executable (pi-coding-agent-test-fake-pi-executable)
             :extra-args extra-args
             :scenario scenario)))
    ('real
     (list :name 'real
           :label "real"
           :executable pi-coding-agent-executable
           :extra-args pi-coding-agent-extra-args))
    (_
     (error "Unknown test backend: %S" backend))))

;;;; Batch Emacs Helpers

(defun pi-coding-agent-test--read-batch-emacs-result (expression)
  "Evaluate EXPRESSION in a fresh batch Emacs and read its printed result.
Initializes packages, then re-prepends the current project root to
`load-path' so the checkout under test wins over any installed copy."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (repo-root (file-name-directory (locate-library "pi-coding-agent")))
         (output-buffer (generate-new-buffer " *pi-coding-agent-batch-emacs*"))
         (exit-code (call-process emacs nil output-buffer nil
                                  "--batch" "-Q" "-L" repo-root
                                  "--eval" "(require 'package)"
                                  "--eval" "(package-initialize)"
                                  "--eval" "(setq load-prefer-newer t)"
                                  "--eval"
                                  (format "(setq load-path (cons %S load-path))"
                                          repo-root)
                                  "--eval" expression)))
    (unwind-protect
        (progn
          (unless (eq 0 exit-code)
            (error "Batch Emacs exited with %s" exit-code))
          (with-current-buffer output-buffer
            (goto-char (point-min))
            (read (current-buffer))))
      (kill-buffer output-buffer))))

(defun pi-coding-agent-test--markdown-load-state (library)
  "Return Markdown association state after requiring LIBRARY in batch Emacs."
  (pi-coding-agent-test--read-batch-emacs-result
   (format "(progn
  (defvar major-mode-remap-alist nil)
  (defvar treesit-major-mode-remap-alist nil)
  (let ((before-auto (copy-tree auto-mode-alist))
        (before-major-remap (copy-tree major-mode-remap-alist))
        (before-treesit-remap (copy-tree treesit-major-mode-remap-alist)))
    (require '%s)
    (prin1 (list
            :auto-unchanged (equal before-auto auto-mode-alist)
            :major-remap-unchanged (equal before-major-remap major-mode-remap-alist)
            :treesit-remap-unchanged (equal before-treesit-remap treesit-major-mode-remap-alist)
            :md-mode-defined (fboundp 'md-ts-mode)
            :md-mode-maybe-defined (fboundp 'md-ts-mode-maybe)
            :before-md-association (assoc \"\\.md\\'\" before-auto)
            :after-md-association (assoc \"\\.md\\'\" auto-mode-alist)
            :before-major-markdown-remap (alist-get 'markdown-mode before-major-remap)
            :after-major-markdown-remap (alist-get 'markdown-mode major-mode-remap-alist)
            :before-treesit-markdown-remap (alist-get 'markdown-mode before-treesit-remap)
            :after-treesit-markdown-remap (alist-get 'markdown-mode treesit-major-mode-remap-alist)))))"
           library)))

;;;; Waiting Helpers

(defun pi-coding-agent-test-wait-until (predicate &optional timeout poll-interval process)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse.
POLL-INTERVAL controls how often to check (default
`pi-coding-agent-test-poll-interval'). If PROCESS is non-nil, it is
passed to `accept-process-output' to allow process I/O.

Returns the predicate value, or nil on timeout."
  (let* ((timeout (or timeout pi-coding-agent-test-short-wait))
         (poll-interval (or poll-interval pi-coding-agent-test-poll-interval))
         (start (float-time))
         (result (funcall predicate)))
    (while (and (not result)
                (< (- (float-time) start) timeout))
      (accept-process-output process poll-interval)
      (setq result (funcall predicate)))
    result))

(defun pi-coding-agent-test-wait-for-process-exit (process &optional timeout)
  "Wait until PROCESS is no longer live, up to TIMEOUT seconds.
Returns non-nil if the process exits before the timeout."
  (pi-coding-agent-test-wait-until
   (lambda () (not (process-live-p process)))
   (or timeout pi-coding-agent-test-short-wait)
   pi-coding-agent-test-poll-interval
   process))

;;;; Toolcall Streaming Helpers

(defun pi-coding-agent-test--count-matches (regexp string)
  "Count non-overlapping occurrences of REGEXP in STRING."
  (let ((count 0) (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defmacro pi-coding-agent-test--with-streaming-assistant (&rest body)
  "Run BODY in a temp chat buffer with an active assistant stream."
  (declare (indent 0) (debug body))
  `(with-temp-buffer
     (pi-coding-agent-chat-mode)
     (pi-coding-agent--handle-display-event '(:type "agent_start"))
     (pi-coding-agent--handle-display-event '(:type "message_start"))
     ,@body))

(defun pi-coding-agent-test--toolcall (id tool-name args)
  "Return a toolCall content block for ID, TOOL-NAME, and ARGS."
  `(:type "toolCall" :id ,id :name ,tool-name :arguments ,args))

(defun pi-coding-agent-test--toolcall-message-update
    (event-type content-index toolcalls &optional delta)
  "Return a message_update event for EVENT-TYPE over TOOLCALLS.
CONTENT-INDEX is written into `assistantMessageEvent'.  DELTA is added
when non-nil.  TOOLCALLS is a list of toolCall content block plists."
  `(:type "message_update"
    :assistantMessageEvent ,(append (list :type event-type :contentIndex content-index)
                                    (when delta (list :delta delta)))
    :message (:role "assistant" :content ,(vconcat toolcalls))))

(defun pi-coding-agent-test--send-toolcall-message-update
    (event-type content-index toolcalls &optional delta)
  "Send a toolcall message update for EVENT-TYPE over TOOLCALLS.
CONTENT-INDEX and optional DELTA are forwarded to
`pi-coding-agent-test--toolcall-message-update'."
  (pi-coding-agent--handle-display-event
   (pi-coding-agent-test--toolcall-message-update
    event-type content-index toolcalls delta)))

(defmacro pi-coding-agent-test--with-toolcall (tool-name args &rest body)
  "Set up a chat buffer with a streaming tool call, then run BODY.
Creates a temp buffer in chat mode, fires agent_start, message_start,
and toolcall_start for TOOL-NAME with ARGS (a plist).  The tool call
ID is \"call_1\" and contentIndex is 0."
  (declare (indent 2) (debug (sexp sexp body)))
  `(pi-coding-agent-test--with-streaming-assistant
     (pi-coding-agent-test--send-toolcall-message-update
      "toolcall_start" 0
      (list (pi-coding-agent-test--toolcall "call_1" ,tool-name ,args)))
     ,@body))

(defun pi-coding-agent-test--send-delta (tool-name args)
  "Send a toolcall_delta event for TOOL-NAME with ARGS.
Uses tool call ID \"call_1\" and contentIndex 0."
  (pi-coding-agent-test--send-toolcall-message-update
   "toolcall_delta" 0
   (list (pi-coding-agent-test--toolcall "call_1" tool-name args))
   "x"))

;;;; Mock Session

(defmacro pi-coding-agent-test-with-mock-session (dir &rest body)
  "Execute BODY with a mocked pi session in DIR, cleaning up after.
DIR should be a unique directory path, typically created with
`pi-coding-agent-test--make-temp-directory'.
Mocks `project-current', `pi-coding-agent--start-process', and
`pi-coding-agent--display-buffers'.
Automatically cleans up chat and input buffers."
  (declare (indent 1) (debug t))
  `(let ((default-directory ,dir))
     (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
               ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
               ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
       (unwind-protect
           (progn (pi-coding-agent) ,@body)
         (pi-coding-agent-test--kill-session-buffers ,dir)))))

(defun pi-coding-agent-test--chat-buffer-name (dir &optional session)
  "Return the chat buffer name for DIR and optional SESSION."
  (pi-coding-agent--buffer-name :chat dir session))

(defun pi-coding-agent-test--input-buffer-name (dir &optional session)
  "Return the input buffer name for DIR and optional SESSION."
  (pi-coding-agent--buffer-name :input dir session))

(defun pi-coding-agent-test--kill-session-buffers (dir &optional session)
  "Kill chat and input buffers for DIR and optional SESSION."
  (ignore-errors (kill-buffer (pi-coding-agent-test--chat-buffer-name dir session)))
  (ignore-errors (kill-buffer (pi-coding-agent-test--input-buffer-name dir session))))

(defun pi-coding-agent-test--kill-live-buffers (&rest buffers)
  "Kill each live buffer in BUFFERS."
  (dolist (buf buffers)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun pi-coding-agent-test--make-temp-directory (prefix)
  "Create and return a fresh temporary directory for tests.
PREFIX is forwarded to `make-temp-file'.  The returned path always has a
trailing slash so it behaves like `default-directory'."
  (file-name-as-directory (make-temp-file prefix t)))

(defun pi-coding-agent-test--write-chat-buffer (chat prefix &optional appended-text)
  "Save CHAT to a temp markdown file and return the file name.
PREFIX is forwarded to `make-temp-file'.  When APPENDED-TEXT is non-nil,
append it to CHAT before saving.  The temp file is created without initial
contents so tests can verify the full write result explicitly."
  (let ((file (make-temp-file prefix nil ".md")))
    (delete-file file)
    (with-current-buffer chat
      (when appended-text
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert appended-text)))
      (write-file file))
    file))

;;;; Tree Fixtures

(defun pi-coding-agent-test--build-tree (&rest specs)
  "Build a conversation tree from flat node SPECS.
Each SPEC is (ID PARENT-OVERRIDE TYPE &rest PROPS) where:
- ID is the node identifier string
- PARENT-OVERRIDE is nil (auto-chain to previous node) or a parent ID
- TYPE is \"message\", \"compaction\", \"model_change\", etc.
- PROPS are keyword plist properties (:role, :preview, etc.)
First node with nil PARENT-OVERRIDE becomes the root.
Returns (:tree VECTOR :leafId LAST-ID)."
  (let ((nodes (make-hash-table :test 'equal))
        (child-ids (make-hash-table :test 'equal))
        (roots nil)
        (prev-id nil)
        (last-id nil))
    ;; Pass 1: create nodes, track parent-child relationships
    (dolist (spec specs)
      (let* ((id (nth 0 spec))
             (parent-override (nth 1 spec))
             (type (nth 2 spec))
             (props (nthcdr 3 spec))
             (parent-id (or parent-override prev-id))
             (node (append (list :id id :type type)
                           (when parent-id (list :parentId parent-id))
                           props)))
        (puthash id node nodes)
        (if parent-id
            (puthash parent-id
                     (append (gethash parent-id child-ids) (list id))
                     child-ids)
          (push id roots))
        (setq prev-id id
              last-id id)))
    ;; Pass 2: build nested structure with :children vectors
    (cl-labels ((build (id)
                  (let* ((node (gethash id nodes))
                         (kids (gethash id child-ids))
                         (child-vec (if kids
                                        (apply #'vector (mapcar #'build kids))
                                      [])))
                    (append node (list :children child-vec)))))
      (list :tree (apply #'vector (mapcar #'build (nreverse roots)))
            :leafId last-id))))

(defun pi-coding-agent-test--make-3turn-fork-messages ()
  "Return get_fork_messages payload for three user turns."
  [(:entryId "u1" :text "First question")
   (:entryId "u2" :text "Second question")
   (:entryId "u3" :text "Third question")])

;;;; Chat Buffer Fixtures

(defun pi-coding-agent-test--insert-chat-turns ()
  "Insert a 3-turn chat with setext headings into current buffer.
Returns the buffer with content ready for navigation tests."
  (insert "Pi 1.0.0\n========\nWelcome\n\n"
          "You · 10:00\n===========\nFirst question\n\n"
          "Assistant\n=========\nFirst answer\n\n"
          "You · 10:05\n===========\nSecond question\n\n"
          "Assistant\n=========\nSecond answer\n\n"
          "You · 10:10\n===========\nThird question\n\n"
          "Assistant\n=========\nThird answer\n"))

(provide 'pi-coding-agent-test-common)
;;; pi-coding-agent-test-common.el ends here
