;;; piem-core-test.el --- Tests for piem-core -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the core piem functionality: JSON parsing,
;; line accumulation, and command encoding.

;;; Code:

(require 'ert)
(require 'piem-core)
(require 'piem-test-common)

;;;; JSON Parsing Tests

(ert-deftest piem-test-parse-json-response ()
  "Parse a valid JSON response."
  (let ((result (piem--parse-json-line "{\"type\":\"response\",\"id\":\"req_1\",\"success\":true}")))
    (should (equal (plist-get result :type) "response"))
    (should (equal (plist-get result :id) "req_1"))
    (should (eq (plist-get result :success) t))))

(ert-deftest piem-test-parse-json-event ()
  "Parse a valid JSON event (no id field)."
  (let ((result (piem--parse-json-line "{\"type\":\"agent_start\"}")))
    (should (equal (plist-get result :type) "agent_start"))
    (should (null (plist-get result :id)))))

(ert-deftest piem-test-parse-json-with-nested-data ()
  "Parse JSON with nested objects."
  (let ((result (piem--parse-json-line "{\"type\":\"response\",\"data\":{\"model\":\"claude\",\"count\":42}}")))
    (should (equal (plist-get result :type) "response"))
    (let ((data (plist-get result :data)))
      (should (equal (plist-get data :model) "claude"))
      (should (equal (plist-get data :count) 42)))))

(ert-deftest piem-test-parse-json-malformed ()
  "Malformed JSON returns nil, not an error."
  (should (null (piem--parse-json-line "not valid json")))
  (should (null (piem--parse-json-line "")))
  (should (null (piem--parse-json-line "{")))
  (should (null (piem--parse-json-line "{\"unterminated"))))

(ert-deftest piem-test-parse-json-boolean-false ()
  "JSON false parses to :false, not nil."
  (let ((result (piem--parse-json-line "{\"isStreaming\":false}")))
    (should (eq (plist-get result :isStreaming) :false))))

(ert-deftest piem-test-parse-json-null ()
  "JSON null parses to :null, not nil."
  (let ((result (piem--parse-json-line "{\"result\":null}")))
    (should (eq (plist-get result :result) :null))))

(ert-deftest piem-test-json-false-p-accepts-both-sentinels ()
  "JSON false helper accepts both parser and encoder sentinels."
  (should (piem--json-false-p :false))
  (should (piem--json-false-p :json-false))
  (should-not (piem--json-false-p nil))
  (should-not (piem--json-false-p t)))

(ert-deftest piem-test-normalize-string-or-null-preserves-nonempty-strings ()
  "String normalization preserves ordinary and whitespace-only strings."
  (dolist (value '("ordinary text" " \t\n"))
    (should (equal (piem--normalize-string-or-null value)
                   value))))

(ert-deftest piem-test-normalize-string-or-null-rejects-empty-and-nonstrings ()
  "String normalization maps empty, null, false, and non-strings to nil."
  (let ((values (list "" nil :null :false :json-false
                      17 2.5 ["vector"] '(:key "value"))))
    (should (equal (mapcar #'piem--normalize-string-or-null values)
                   (make-list (length values) nil)))))

(ert-deftest piem-test-parse-json-unicode ()
  "Unicode content is preserved correctly."
  (let ((result (piem--parse-json-line "{\"msg\":\"Hello 世界 🌍\"}")))
    (should (equal (plist-get result :msg) "Hello 世界 🌍"))))

;;;; Line Accumulation Tests

(ert-deftest piem-test-accumulate-complete-line ()
  "A complete line (ending with newline) is extracted."
  (let ((result (piem--accumulate-lines "" "foo\n")))
    (should (equal (car result) '("foo")))
    (should (equal (cdr result) ""))))

(ert-deftest piem-test-accumulate-lines-treats-nil-as-empty ()
  "A nil accumulated fragment is treated as no previous input."
  (let ((result (piem--accumulate-lines nil "foo\nbar")))
    (should (equal (car result) '("foo")))
    (should (equal (cdr result) "bar"))))

(ert-deftest piem-test-accumulate-partial-line ()
  "A partial line (no newline) is saved as remainder."
  (let ((result (piem--accumulate-lines "" "foo")))
    (should (null (car result)))
    (should (equal (cdr result) "foo"))))

(ert-deftest piem-test-accumulate-multiple-lines ()
  "Multiple complete lines are all extracted."
  (let ((result (piem--accumulate-lines "" "foo\nbar\nbaz\n")))
    (should (equal (car result) '("foo" "bar" "baz")))
    (should (equal (cdr result) ""))))

(ert-deftest piem-test-accumulate-partial-then-complete ()
  "Partial line followed by completion works correctly."
  (let* ((result1 (piem--accumulate-lines "" "fo"))
         (result2 (piem--accumulate-lines (cdr result1) "o\nbar\n")))
    (should (null (car result1)))
    (should (equal (cdr result1) "fo"))
    (should (equal (car result2) '("foo" "bar")))
    (should (equal (cdr result2) ""))))

(ert-deftest piem-test-accumulate-mixed-complete-and-partial ()
  "Complete lines extracted, partial line saved."
  (let ((result (piem--accumulate-lines "" "foo\nbar")))
    (should (equal (car result) '("foo")))
    (should (equal (cdr result) "bar"))))

(ert-deftest piem-test-accumulate-with-existing-remainder ()
  "Existing remainder is prepended to new chunk."
  (let ((result (piem--accumulate-lines "hel" "lo\nworld\n")))
    (should (equal (car result) '("hello" "world")))
    (should (equal (cdr result) ""))))

(ert-deftest piem-test-accumulate-line-chunks-materializes-on-newline ()
  "Chunk accumulation keeps partial lines chunked until a newline arrives."
  (let* ((result1 (piem--accumulate-line-chunks nil "hel"))
         (result2 (piem--accumulate-line-chunks (cdr result1) "lo\nworld"))
         (result3 (piem--accumulate-line-chunks (cdr result2) "\n")))
    (should (null (car result1)))
    (should (equal (cdr result1) '("hel")))
    (should (equal (car result2) '("hello")))
    (should (equal (cdr result2) '("world")))
    (should (equal (car result3) '("world")))
    (should (null (cdr result3)))))

;;;; JSON Encoding Tests

(ert-deftest piem-test-encode-simple-command ()
  "Encode a simple command to JSON."
  (let ((result (piem--encode-command '(:type "prompt" :message "hello"))))
    (should (string-suffix-p "\n" result))
    (let ((parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
      (should (equal (plist-get parsed :type) "prompt"))
      (should (equal (plist-get parsed :message) "hello")))))

(ert-deftest piem-test-encode-command-with-id ()
  "Encoded command includes id field."
  (let* ((result (piem--encode-command '(:type "get_state" :id "req_1")))
         (parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
    (should (equal (plist-get parsed :type) "get_state"))
    (should (equal (plist-get parsed :id) "req_1"))))

(ert-deftest piem-test-encode-nested-command ()
  "Encode command with nested data."
  (let* ((result (piem--encode-command '(:type "set_model" :data (:provider "anthropic" :model "claude"))))
         (parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
    (should (equal (plist-get parsed :type) "set_model"))
    (let ((data (plist-get parsed :data)))
      (should (equal (plist-get data :provider) "anthropic")))))

(ert-deftest piem-test-encode-command-with-array ()
  "Encode command with array values."
  (let* ((result (piem--encode-command '(:type "prompt" :attachments [])))
         (parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
    (should (equal (plist-get parsed :attachments) []))))

;;;; Path Boundary Helper Tests

(ert-deftest piem-test-remote-prefix-detects-tramp-anchor ()
  "Remote prefix helper returns the TRAMP prefix without connecting."
  (let ((default-directory "/ssh:pi-host:/home/pi/project/"))
    (should (equal (piem--remote-prefix)
                   "/ssh:pi-host:"))
    (should (equal (piem--remote-prefix
                    "/ssh:other:/srv/project/")
                   "/ssh:other:"))
    (should (equal (piem--remote-prefix
                    "/ssh:bastion|sudo:root@pi-host:/srv/project/")
                   "/ssh:bastion|sudo:root@pi-host:"))
    (should-not (piem--remote-prefix "/tmp/project/"))))

(ert-deftest piem-test-emacs-path-normalizes-remote-inbound-paths ()
  "Inbound process-local paths become Emacs/TRAMP paths at the boundary."
  (let ((anchor "/ssh:pi-host:/home/pi/project/"))
    (should (equal (piem--emacs-path "/var/pi/session.jsonl" anchor)
                   "/ssh:pi-host:/var/pi/session.jsonl"))
    (should (equal (piem--emacs-path "~/pi/session.jsonl" anchor)
                   "/ssh:pi-host:~/pi/session.jsonl"))
    (should (equal (piem--emacs-path "~root/pi/session.jsonl" anchor)
                   "/ssh:pi-host:~root/pi/session.jsonl"))
    (should (equal (piem--emacs-path "sessions/current.jsonl" anchor)
                   "/ssh:pi-host:/home/pi/project/sessions/current.jsonl"))
    (should (equal (piem--emacs-path
                    "/ssh:pi-host:/tmp/remote.jsonl" anchor)
                   "/ssh:pi-host:/tmp/remote.jsonl"))
    (should-not (piem--emacs-path "" anchor))))

(ert-deftest piem-test-emacs-path-preserves-multi-hop-route ()
  "Inbound process-local paths keep the full multi-hop TRAMP anchor."
  (let ((anchor "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"))
    (should (equal (piem--emacs-path
                    "sessions/current.jsonl" anchor)
                   "/ssh:bastion|sudo:root@pi-host:/home/pi/project/sessions/current.jsonl"))
    (should (equal (piem--emacs-path
                    "/var/pi/session.jsonl" anchor)
                   "/ssh:bastion|sudo:root@pi-host:/var/pi/session.jsonl"))))

(ert-deftest piem-test-emacs-path-rejects-incompatible-tramp-inbound-paths ()
  "Inbound TRAMP paths must match the full session anchor route."
  (let ((anchor "/ssh:pi-host:/home/pi/project/"))
    (should-error (piem--emacs-path
                   "/ssh:other:/tmp/remote.jsonl" anchor)
                  :type 'user-error)
    (should-error (piem--emacs-path
                   "/ssh:pi-host:/tmp/remote.jsonl" "/tmp/project/")
                  :type 'user-error))
  (let ((anchor "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"))
    (should (equal (piem--emacs-path "/etc/hosts" anchor)
                   "/ssh:bastion|sudo:root@pi-host:/etc/hosts"))
    (should-error (piem--emacs-path
                   "/sudo:root@pi-host:/etc/hosts" anchor)
                  :type 'user-error)))

(ert-deftest piem-test-path-helpers-reject-nul-strings ()
  "NUL-containing path strings are not usable file names."
  (let ((anchor "/ssh:pi-host:/home/pi/project/")
        (bad (concat "/tmp/bad" (string ?\0) "name.el")))
    (let ((err (should-error (piem--emacs-path bad anchor)
                             :type 'user-error)))
      (should (string-match-p "NUL" (error-message-string err))))
    (let ((err (should-error (piem--process-local-path bad anchor)
                             :type 'user-error)))
      (should (string-match-p "NUL" (error-message-string err))))
    (let ((err (should-error (piem--shell-command-path bad anchor)
                             :type 'user-error)))
      (should (string-match-p "NUL" (error-message-string err))))
    (let ((err (should-error (piem--shell-quote-path bad anchor)
                             :type 'user-error)))
      (should (string-match-p "NUL" (error-message-string err))))))

(ert-deftest piem-test-passive-emacs-path-ignores-unsafe-metadata ()
  "Passive backend path metadata returns nil instead of signaling."
  (let ((anchor "/ssh:pi-host:/home/pi/project/")
        (bad (concat "/tmp/bad" (string ?\0) "name.el")))
    (should-not (piem--passive-emacs-path bad anchor))
    (should-not (piem--passive-emacs-path
                 "/ssh:other:/tmp/name.el" anchor))
    (should (equal (piem--passive-emacs-path
                    "prompts/fix.md" anchor)
                   "/ssh:pi-host:/home/pi/project/prompts/fix.md"))))

(ert-deftest piem-test-emacs-directory-normalizes-remote-inbound-directory ()
  "Inbound directories are normalized to Emacs paths with trailing slash."
  (should (equal (piem--emacs-directory
                  "/home/pi/project" "/ssh:pi-host:/tmp/session.jsonl")
                 "/ssh:pi-host:/home/pi/project/")))

(ert-deftest piem-test-emacs-directory-preserves-multi-hop-route ()
  "Directory normalization keeps full multi-hop TRAMP prefixes."
  (should (equal (piem--emacs-directory
                  "/home/pi/project"
                  "/ssh:bastion|sudo:root@pi-host:/tmp/session.jsonl")
                 "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")))

(ert-deftest piem-test-process-local-path-strips-tramp-outbound-paths ()
  "Outbound paths are converted into process-local paths for Pi."
  (let ((anchor "/ssh:pi-host:/home/pi/project/"))
    (should (equal (piem--process-local-path
                    "/ssh:pi-host:/var/pi/session.jsonl" anchor)
                   "/var/pi/session.jsonl"))
    (should (equal (piem--process-local-path
                    "/ssh:bastion|sudo:root@pi-host:/var/pi/session.jsonl"
                    "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
                   "/var/pi/session.jsonl"))
    (should (equal (piem--process-local-path
                    "/var/pi/session.jsonl" anchor)
                   "/var/pi/session.jsonl"))
    (should (equal (piem--process-local-path
                    "sessions/current.jsonl" anchor)
                   "/home/pi/project/sessions/current.jsonl"))
    (should (equal (piem--process-local-path
                    "~/pi/session.jsonl" anchor)
                   "~/pi/session.jsonl"))
    (should-error (piem--process-local-path
                   "~root/pi/session.jsonl" anchor)
                  :type 'user-error)
    (should-not (piem--process-local-path "" anchor))))

(ert-deftest piem-test-process-local-path-rejects-other-remote ()
  "Outbound remote paths must stay on the session remote."
  (let ((anchor "/ssh:pi-host:/home/pi/project/"))
    (should-error (piem--process-local-path
                   "/ssh:other:/var/pi/session.jsonl" anchor)
                  :type 'user-error)
    (should-error (piem--process-local-path
                   "/scp:pi-host:/var/pi/session.jsonl" anchor)
                  :type 'user-error)))

(ert-deftest piem-test-process-local-path-rejects-remote-for-local-anchor ()
  "Outbound TRAMP paths are not stripped for a local Pi process."
  (should-error (piem--process-local-path
                 "/ssh:pi-host:/var/pi/session.jsonl" "/tmp/project/")
                :type 'user-error))

(ert-deftest piem-test-ensure-compatible-remote-path-rejects-local-anchor ()
  "Remote compatibility helper rejects TRAMP paths for local anchors."
  (should-not (piem--ensure-compatible-remote-path
               "/tmp/session.jsonl" "/ssh:pi-host:/home/pi/project/"))
  (should-error (piem--ensure-compatible-remote-path
                 "/ssh:pi-host:/tmp/session.jsonl" "/tmp/project/")
                :type 'user-error)
  (should-not (piem--ensure-compatible-remote-path
               "/ssh:pi-host:/tmp/session.jsonl"
               "/ssh:pi-host:/home/pi/project/")))

(ert-deftest piem-test-process-local-path-preserves-remote-home-paths ()
  "Remote outbound home paths stay process-local without guessed expansion."
  (let ((anchor "/ssh:pi-host:/home/pi/project/"))
    (should (equal (piem--process-local-path "~" anchor) "~"))
    (should (equal (piem--process-local-path "~/out.html" anchor)
                   "~/out.html"))
    (should-error (piem--process-local-path "~root/out.html" anchor)
                  :type 'user-error)
    (should-error (piem--process-local-path
                   "/ssh:pi-host:~root/out.html" anchor)
                  :type 'user-error)))

(ert-deftest piem-test-process-local-path-preserves-home-without-tramp-handler ()
  "Remote outbound home paths never expand against Emacs's local home."
  (let ((anchor "/ssh:pi-host:/home/pi/project/")
        (file-name-handler-alist nil))
    (should (equal (piem--process-local-path "~/out.html" anchor)
                   "~/out.html"))
    (should (equal (piem--process-local-path
                    "/ssh:pi-host:/tmp/out.html" anchor)
                   "/tmp/out.html"))))

(ert-deftest piem-test-process-local-path-expands-local-home-paths ()
  "Local outbound home paths are expanded before Pi receives JSON."
  (should (equal (piem--process-local-path
                  "~/out.html" "/tmp/pi-project/")
                 (expand-file-name "~/out.html"))))

(ert-deftest piem-test-process-local-path-expands-local-relative-paths ()
  "Local outbound relative paths expand under the local anchor."
  (should (equal (piem--process-local-path
                  "sessions/current.jsonl" "/tmp/pi-project/")
                 "/tmp/pi-project/sessions/current.jsonl")))

(ert-deftest piem-test-shell-command-path-normalizes-for-emacs-shell-host ()
  "Shell paths are local to the host where Emacs starts `shell-command'."
  (let ((home-path (expand-file-name "~/a b.el")))
    (dolist (case
             `(("src/a b.el" "/tmp/pi project/"
                "/tmp/pi project/src/a b.el")
               ("/tmp/a b.el" "/tmp/pi project/" "/tmp/a b.el")
               ("~/a b.el" "/tmp/pi project/" ,home-path)
               ("/:/tmp/a b.el" "/tmp/pi project/" "/tmp/a b.el")
               ("/:relative" "/tmp/pi project/"
                "/tmp/pi project/relative")
               ("src/a b.el" "/ssh:pi-host:/home/pi/project/"
                "/home/pi/project/src/a b.el")
               ("/ssh:pi-host:/tmp/a b.el"
                "/ssh:pi-host:/home/pi/project/" "/tmp/a b.el")
               ("~/a b.el" "/ssh:pi-host:/home/pi/project/" "~/a b.el")
               ("~root/a b.el" "/ssh:pi-host:/home/pi/project/"
                "~root/a b.el")
               ("/ssh:pi-host:/:/tmp/a b.el"
                "/ssh:pi-host:/home/pi/project/" "/tmp/a b.el")
               ("/ssh:bastion|sudo:root@pi-host:/tmp/a b.el"
                "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"
                "/tmp/a b.el")
               ("/ssh:bastion|sudo:root@pi-host:~daemon/a b.el"
                "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"
                "~daemon/a b.el")
               ("/ssh:bastion|sudo:root@pi-host:/:/tmp/a b.el"
                "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"
                "/tmp/a b.el")))
      (should (equal (piem--shell-command-path
                      (nth 0 case) (nth 1 case))
                     (nth 2 case))))))

(ert-deftest piem-test-shell-command-path-rejects-incompatible-remotes ()
  "Shell conversion never strips a different TRAMP route."
  (should-error
   (piem--shell-command-path
    "/ssh:other:/tmp/a.el" "/ssh:pi-host:/home/pi/project/")
   :type 'user-error)
  (should-error
   (piem--shell-command-path
    "/sudo:root@pi-host:/tmp/a.el"
    "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
   :type 'user-error)
  (should-error
   (piem--shell-command-path
    "/ssh:pi-host:/tmp/a.el" "/tmp/project/")
   :type 'user-error))

(ert-deftest piem-test-shell-command-path-rejects-unrooted-tramp-local-names ()
  "Explicit TRAMP local names must not become unsafe shell operands."
  (dolist (case '(("/ssh:pi-host:-rf"
                   "/ssh:pi-host:/home/pi/project/")
                  ("/ssh:pi-host:" "/ssh:pi-host:/home/pi/project/")
                  ("/ssh:bastion|sudo:root@pi-host:-rf"
                   "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
                  ("/ssh:bastion|sudo:root@pi-host:"
                   "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
                  ("~root;printf PWNED/a.el"
                   "/ssh:pi-host:/home/pi/project/")
                  ("~+" "/ssh:pi-host:/home/pi/project/")
                  ("~-/a.el" "/ssh:pi-host:/home/pi/project/")
                  ("~0/a.el" "/ssh:pi-host:/home/pi/project/")))
    (should-error
     (piem--shell-command-path (nth 0 case) (nth 1 case))
     :type 'user-error)))

(ert-deftest piem-test-shell-quote-path-preserves-remote-home-expansion ()
  "Quoting leaves only a safe remote tilde prefix visible to the shell."
  (let ((remote "/ssh:pi-host:/home/pi/project/")
        (multi "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"))
    (should (equal (piem--shell-quote-path "~/a b.el" remote)
                   (concat "~/" (shell-quote-argument "a b.el" t))))
    (should (equal (piem--shell-quote-path
                    "~root/a b.el" remote)
                   (concat "~root/" (shell-quote-argument "a b.el" t))))
    (should (equal (piem--shell-quote-path
                    "~daemon/a b.el" multi)
                   (concat "~daemon/" (shell-quote-argument "a b.el" t))))
    (should (equal (piem--shell-quote-path
                    "~root/a\nb.el" remote)
                   (concat "~root/" (shell-quote-argument "a\nb.el" t))))
    (should (equal (piem--shell-quote-path
                    "/tmp/a b.el" remote)
                   (shell-quote-argument "/tmp/a b.el" t)))
    (should (equal (piem--shell-quote-path
                    "/tmp/a b.el" "/tmp/project/")
                   (shell-quote-argument "/tmp/a b.el")))))

(ert-deftest piem-test-shell-quote-path-rejects-unsafe-home-prefix ()
  "Shell-significant and reserved named-home prefixes are never exposed."
  (dolist (path '("~root;printf PWNED/a.el" "~+" "~-/a.el"
                  "~0/a.el" "~+0/a.el"))
    (should-error
     (piem--shell-quote-path
      path "/ssh:pi-host:/home/pi/project/")
     :type 'user-error)))

(ert-deftest piem-test-shell-quote-path-hides-terminal-ampersand-from-emacs ()
  "Quoted operands cannot trigger `shell-command' trailing-& detection."
  (dolist (case '(("/tmp/report&" "/tmp/project/" nil)
                  ("/tmp/report&" "/ssh:pi-host:/home/pi/project/" t)
                  ("~/report&" "/ssh:pi-host:/home/pi/project/" t)))
    (let* ((path (nth 0 case))
           (anchor (nth 1 case))
           (posix (nth 2 case))
           (quoted (piem--shell-quote-path path anchor))
           (ordinary (if (string-prefix-p "~/" path)
                         (concat "~/"
                                 (shell-quote-argument
                                  (substring path 2) t))
                       (shell-quote-argument path posix))))
      (should (equal quoted
                     (concat ordinary (shell-quote-argument "" posix))))
      (should-not (string-suffix-p "&" quoted)))))

;;;; Process Cleanup Tests

(ert-deftest piem-test-process-exit-clears-pending ()
  "Process exit clears pending request state."
  (let ((piem--request-id-counter 0)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (piem--get-pending-requests fake-proc))
              (pending-types (piem--get-pending-command-types fake-proc)))
          (puthash "req_1" #'ignore pending)
          (puthash "req_2" #'ignore pending)
          (puthash "req_1" "get_tree" pending-types)
          (puthash "req_2" "get_state" pending-types)
          (piem--handle-process-exit fake-proc "finished\n")
          (should (= (hash-table-count pending) 0))
          (should (= (hash-table-count pending-types) 0)))
      (ignore-errors (delete-process fake-proc)))))

(ert-deftest piem-test-process-exit-calls-callbacks-with-error ()
  "Process exit calls pending callbacks with error response."
  (let ((piem--request-id-counter 0)
        (received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (piem--get-pending-requests fake-proc)))
          (puthash "req_1" (lambda (r) (setq received r)) pending)
          (piem--handle-process-exit fake-proc "finished\n")
          (should received)
          (should (eq (plist-get received :success) :false))
          (should (plist-get received :error)))
      (ignore-errors (delete-process fake-proc)))))

(ert-deftest piem-test-process-exit-runs-exit-handler-after-callbacks ()
  "Process exit lets pending callbacks recover input before frontend cleanup."
  (let ((events nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (piem--get-pending-requests fake-proc)))
          (puthash "req_1" (lambda (_r) (push 'callback events)) pending)
          (process-put fake-proc 'piem-exit-handler
                       (lambda (_r) (push 'exit-handler events)))
          (piem--handle-process-exit fake-proc "finished\n")
          (should (equal (reverse events) '(callback exit-handler))))
      (ignore-errors (delete-process fake-proc)))))

;;;; Response Dispatch Tests

(ert-deftest piem-test-dispatch-response-calls-callback ()
  "Response with matching ID calls stored callback."
  (let ((received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (piem--get-pending-requests fake-proc)))
          (puthash "req_1" (lambda (r) (setq received r)) pending)
          (piem--dispatch-response fake-proc '(:type "response" :id "req_1" :success t))
          (should received)
          (should (eq (plist-get received :success) t)))
      (delete-process fake-proc))))

(ert-deftest piem-test-dispatch-response-removes-callback ()
  "Response removes callback from pending requests after calling."
  (let ((fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (piem--get-pending-requests fake-proc)))
          (puthash "req_1" #'ignore pending)
          (piem--dispatch-response fake-proc '(:type "response" :id "req_1" :success t))
          (should (null (gethash "req_1" pending))))
      (delete-process fake-proc))))

(ert-deftest piem-test-dispatch-idless-response-to-sole-pending ()
  "Id-less response routes to sole pending callback."
  (let ((received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (piem--get-pending-requests fake-proc)))
          (puthash "req_1" (lambda (r) (setq received r)) pending)
          (piem--dispatch-response
           fake-proc
           '(:type "response" :command "get_tree" :success nil :error "Unknown command: get_tree"))
          (should (equal (plist-get received :error) "Unknown command: get_tree"))
          (should (= (hash-table-count pending) 0)))
      (delete-process fake-proc))))

(ert-deftest piem-test-dispatch-idless-response-matches-command ()
  "Id-less response with :command routes to matching request."
  (let ((piem--request-id-counter 0)
        (received-tree nil)
        (received-state nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string) #'ignore))
          (piem--rpc-async
           fake-proc
           '(:type "get_tree")
           (lambda (response)
             (setq received-tree response)))
          (piem--rpc-async
           fake-proc
           '(:type "get_state")
           (lambda (response)
             (setq received-state response)))
          (piem--dispatch-response
           fake-proc
           '(:type "response" :command "get_tree" :success nil :error "Unknown command: get_tree"))
          (should (equal (plist-get received-tree :error) "Unknown command: get_tree"))
          (should-not received-state)
          (should (= (hash-table-count (piem--get-pending-requests fake-proc)) 1)))
      (delete-process fake-proc))))

(ert-deftest piem-test-dispatch-event-calls-handler ()
  "Events call the process handler."
  (let ((event-received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (process-put fake-proc 'piem-display-handler
                       (lambda (e) (setq event-received e)))
          (piem--dispatch-response fake-proc '(:type "agent_start"))
          (should event-received)
          (should (equal (plist-get event-received :type) "agent_start")))
      (delete-process fake-proc))))

(ert-deftest piem-test-dispatch-unknown-id-no-crash ()
  "Unknown response IDs do not crash."
  (let ((fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (should (null (piem--dispatch-response fake-proc '(:type "response" :id "unknown" :success t))))
      (delete-process fake-proc))))

;;;; RPC Send Tests

(ert-deftest piem-test-rpc-async-stores-callback ()
  "Sending a command stores the callback in process's pending requests."
  (let ((piem--request-id-counter 0)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (piem--rpc-async fake-proc '(:type "get_state") #'ignore)
          (let ((pending (piem--get-pending-requests fake-proc)))
            (should (gethash "req_1" pending))))
      (delete-process fake-proc))))

(ert-deftest piem-test-rpc-async-sends-json-with-id ()
  "Sending a command writes JSON with ID to process."
  (let ((piem--request-id-counter 0)
        (output-buffer (generate-new-buffer " *test-output*")))
    (unwind-protect
        (let ((fake-proc (start-process "cat" output-buffer "cat")))
          (unwind-protect
              (progn
                (piem--rpc-async fake-proc '(:type "get_state") #'ignore)
                (should
                 (piem-test-wait-until
                  (lambda ()
                    (with-current-buffer output-buffer
                      (> (buffer-size) 0)))
                  piem-test-short-wait
                  piem-test-poll-interval
                  fake-proc))
                (with-current-buffer output-buffer
                  (let* ((sent (buffer-string))
                         (json (json-parse-string (string-trim sent) :object-type 'plist)))
                    (should (equal (plist-get json :type) "get_state"))
                    (should (equal (plist-get json :id) "req_1")))))
            (delete-process fake-proc)))
      (kill-buffer output-buffer))))

(ert-deftest piem-test-rpc-async-scheduling-error-cleans-pending-state ()
  "Encoding and sending failures leave no orphaned pending request state."
  (let ((piem--request-id-counter 0)
        (real-encode (symbol-function 'piem--encode-command))
        (real-send (symbol-function 'piem--send-string))
        results)
    (dolist (failure '(encode send))
      (let ((fake-proc (start-process "cat" nil "cat"))
            attempted)
        (unwind-protect
            (cl-letf (((symbol-function 'piem--encode-command)
                       (lambda (command)
                         (if (eq failure 'encode)
                             (progn
                               (setq attempted t)
                               (error "synchronous encode failure"))
                           (funcall real-encode command))))
                      ((symbol-function 'piem--send-string)
                       (lambda (process string)
                         (if (eq failure 'send)
                             (progn
                               (setq attempted t)
                               (error "synchronous send failure"))
                           (funcall real-send process string)))))
              (condition-case nil
                  (piem--rpc-async
                   fake-proc '(:type "get_state") #'ignore)
                (error nil))
              (push
               (list failure attempted
                     (hash-table-count
                      (piem--get-pending-requests fake-proc))
                     (hash-table-count
                      (piem--get-pending-command-types fake-proc)))
               results))
          (ignore-errors (delete-process fake-proc)))))
    (should (equal (nreverse results)
                   '((encode t 0 0) (send t 0 0))))))

(ert-deftest piem-test-remote-rpc-queue-flushes-after-ready-marker ()
  "Remote RPC writes queue until the ready marker, then flush FIFO."
  (let ((piem--request-id-counter 0)
        (sent nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (_proc string) (push string sent))))
          (process-put fake-proc 'piem-awaiting-ready-marker t)
          (piem--rpc-async fake-proc '(:type "get_state") #'ignore)
          (piem--rpc-async fake-proc '(:type "get_commands") #'ignore)
          (should-not sent)
          (should (= (length (process-get fake-proc
                                          'piem-outbound-queue))
                     2))
          (piem--process-filter
           fake-proc (concat piem--remote-ready-marker "\n"))
          (should (process-get fake-proc 'piem-ready))
          (should-not (process-get fake-proc
                                   'piem-awaiting-ready-marker))
          (should-not (process-get fake-proc 'piem-outbound-queue))
          (should (equal (mapcar (lambda (string)
                                   (plist-get
                                    (json-parse-string (string-trim string)
                                                       :object-type 'plist)
                                    :type))
                                 (nreverse sent))
                         '("get_state" "get_commands"))))
      (delete-process fake-proc))))

(ert-deftest piem-test-extension-ui-response-queues-until-ready-marker ()
  "Extension UI responses share the remote ready queue."
  (let ((sent nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (_proc string) (push string sent))))
          (process-put fake-proc 'piem-awaiting-ready-marker t)
          (piem--send-extension-ui-response
           fake-proc '(:type "extension_ui_response" :id "ui_1" :value "ok"))
          (should-not sent)
          (should (= (length (process-get fake-proc
                                          'piem-outbound-queue))
                     1))
          (piem--process-filter
           fake-proc (concat piem--remote-ready-marker "\n"))
          (should-not (process-get fake-proc 'piem-outbound-queue))
          (let ((json (json-parse-string (string-trim (car sent))
                                         :object-type 'plist)))
            (should (equal (plist-get json :type) "extension_ui_response"))
            (should (equal (plist-get json :id) "ui_1"))))
      (delete-process fake-proc))))

;;;; Request ID Management Tests

(ert-deftest piem-test-request-id-increments ()
  "Request IDs increment with each call."
  (let ((piem--request-id-counter 0))
    (should (equal (piem--next-request-id) "req_1"))
    (should (equal (piem--next-request-id) "req_2"))
    (should (equal (piem--next-request-id) "req_3"))))

(ert-deftest piem-test-pending-requests-table ()
  "Each process gets its own pending requests table."
  (let ((proc1 (start-process "cat1" nil "cat"))
        (proc2 (start-process "cat2" nil "cat")))
    (unwind-protect
        (let ((pending1 (piem--get-pending-requests proc1))
              (pending2 (piem--get-pending-requests proc2)))
          ;; Tables should be separate
          (puthash "req_1" #'ignore pending1)
          (should (gethash "req_1" pending1))
          (should (null (gethash "req_1" pending2)))
          ;; Calling get again returns the same table
          (should (eq pending1 (piem--get-pending-requests proc1))))
      (ignore-errors (delete-process proc1))
      (ignore-errors (delete-process proc2)))))

;;;; State Event Handling Tests

(ert-deftest piem-test-event-agent-start-sets-streaming ()
  "agent_start event sets piem--status to streaming."
  (let ((piem--status 'idle)
        (piem--state nil))
    (piem--update-state-from-event '(:type "agent_start"))
    (should (eq piem--status 'streaming))))

(ert-deftest piem-test-event-agent-end-clears-streaming ()
  "agent_end event sets piem--status to idle."
  (let ((piem--status 'streaming)
        (piem--state nil))
    (piem--update-state-from-event '(:type "agent_end" :messages []))
    (should (eq piem--status 'idle))))

(ert-deftest piem-test-event-agent-end-will-retry-keeps-sending ()
  "agent_end with willRetry keeps the session busy for Pi's retry."
  (let ((piem--status 'streaming)
        (piem--state nil))
    (piem--update-state-from-event
     '(:type "agent_end" :messages [] :willRetry t))
    (should (eq piem--status 'sending))))

(ert-deftest piem-test-event-agent-end-stores-messages ()
  "agent_end event stores messages in state."
  (let ((piem--status 'streaming)
        (piem--state (list :messages nil))
        (msgs [(:role "user" :content "hi") (:role "assistant" :content "hello")]))
    (piem--update-state-from-event (list :type "agent_end" :messages msgs))
    (should (plist-get piem--state :messages))))

(ert-deftest piem-test-event-message-start-creates-current-message ()
  "message_start event creates current-message in state."
  (let ((piem--state (list :current-message nil))
        (msg '(:role "assistant" :content [])))
    (piem--update-state-from-event (list :type "message_start" :message msg))
    (should (plist-get piem--state :current-message))))

(ert-deftest piem-test-event-message-update-is-state-noop ()
  "message_update event does not modify state.
Display is handled by the display handler, not by state updates."
  (let ((piem--state (list :current-message '(:role "assistant" :content "Hello"))))
    (piem--update-state-from-event
     '(:type "message_update"
       :message (:role "assistant")
       :assistantMessageEvent (:type "text_delta" :delta " world")))
    (should (equal (plist-get (plist-get piem--state :current-message) :content)
                   "Hello"))))

(ert-deftest piem-test-event-message-end-clears-current-message ()
  "message_end event clears current-message."
  (let ((piem--state (list :current-message '(:role "assistant" :content "done"))))
    (piem--update-state-from-event '(:type "message_end" :message (:role "assistant")))
    (should (null (plist-get piem--state :current-message)))))

(ert-deftest piem-test-event-tool-start-tracks-active-tool ()
  "tool_execution_start adds tool to active-tools."
  (let ((piem--state (list :active-tools nil)))
    (piem--update-state-from-event
     '(:type "tool_execution_start"
       :toolCallId "call_123"
       :toolName "bash"
       :args (:command "ls")))
    (should (gethash "call_123" (plist-get piem--state :active-tools)))))

(ert-deftest piem-test-event-tool-update-stores-partial-result ()
  "tool_execution_update stores partial result."
  (let* ((tools (make-hash-table :test 'equal))
         (piem--state (list :active-tools tools)))
    (puthash "call_123" (list :name "bash") tools)
    (piem--update-state-from-event
     '(:type "tool_execution_update"
       :toolCallId "call_123"
       :partialResult (:content "output")))
    (let ((tool (gethash "call_123" tools)))
      (should (plist-get tool :partial-result)))))

(ert-deftest piem-test-event-tool-end-removes-active-tool ()
  "tool_execution_end removes tool from active-tools."
  (let* ((tools (make-hash-table :test 'equal))
         (piem--state (list :active-tools tools)))
    (puthash "call_123" (list :name "bash") tools)
    (piem--update-state-from-event
     '(:type "tool_execution_end"
       :toolCallId "call_123"
       :result (:content "done")
       :isError :false))
    (should (null (gethash "call_123" tools)))))

(ert-deftest piem-test-event-compaction-start-sets-compacting ()
  "compaction_start event sets piem--status to compacting."
  (let ((piem--status 'idle)
        (piem--state nil))
    (piem--update-state-from-event '(:type "compaction_start" :reason "threshold"))
    (should (eq piem--status 'compacting))))

(ert-deftest piem-test-event-compaction-end-sets-idle ()
  "compaction_end event sets piem--status to idle."
  (let ((piem--status 'compacting)
        (piem--state nil))
    (piem--update-state-from-event '(:type "compaction_end" :reason "threshold" :aborted :false))
    (should (eq piem--status 'idle))))

(ert-deftest piem-test-event-compaction-end-will-retry-sets-sending ()
  "Successful compaction_end with willRetry keeps the session busy."
  (let ((piem--status 'compacting)
        (piem--state nil))
    (piem--update-state-from-event
     '(:type "compaction_end"
       :reason "overflow"
       :aborted :false
       :willRetry t
       :result (:tokensBefore 1000 :summary "Summary")))
    (should (eq piem--status 'sending))))

(ert-deftest piem-test-event-compaction-end-will-retry-without-result-sets-idle ()
  "willRetry without a result is not a retrying success."
  (let ((piem--status 'compacting)
        (piem--pre-compaction-status nil)
        (piem--state nil))
    (piem--update-state-from-event
     '(:type "compaction_end"
       :reason "overflow"
       :aborted :false
       :willRetry t
       :result :null
       :errorMessage "recovery failed"))
    (should (eq piem--status 'idle))))

(ert-deftest piem-test-event-compaction-preserves-prompt-preflight-sending ()
  "Successful compaction during prompt preflight resumes that prompt."
  (let ((piem--status 'sending)
        (piem--pre-compaction-status nil)
        (piem--state nil))
    (piem--update-state-from-event
     '(:type "compaction_start" :reason "threshold"))
    (should (eq piem--status 'compacting))
    (should (eq piem--pre-compaction-status 'sending))
    (piem--update-state-from-event
     '(:type "compaction_end"
       :reason "threshold"
       :aborted :false
       :willRetry :false
       :result (:tokensBefore 1000 :summary "Summary")))
    (should (eq piem--status 'sending))
    (should (null piem--pre-compaction-status))))

(ert-deftest piem-test-event-failed-compaction-does-not-resume-preflight-sending ()
  "Failed compaction during prompt preflight settles instead of faking retry."
  (let ((piem--status 'sending)
        (piem--pre-compaction-status nil)
        (piem--state nil))
    (piem--update-state-from-event
     '(:type "compaction_start" :reason "threshold"))
    (piem--update-state-from-event
     '(:type "compaction_end"
       :reason "threshold"
       :aborted :false
       :willRetry :false
       :result :null
       :errorMessage "quota exceeded"))
    (should (eq piem--status 'idle))
    (should (null piem--pre-compaction-status))))

(ert-deftest piem-test-event-aborted-compaction-does-not-resume-preflight-sending ()
  "Aborted compaction during prompt preflight is stop-everything, not sending."
  (let ((piem--status 'sending)
        (piem--pre-compaction-status nil)
        (piem--state nil))
    (piem--update-state-from-event
     '(:type "compaction_start" :reason "threshold"))
    (piem--update-state-from-event
     '(:type "compaction_end"
       :reason "threshold"
       :aborted t
       :willRetry :false
       :result nil))
    (should (eq piem--status 'idle))
    (should (null piem--pre-compaction-status))))

(ert-deftest piem-test-ensure-active-tools-from-nil ()
  "piem--ensure-active-tools works when piem--state is nil."
  (let ((piem--state nil))
    (let ((tools (piem--ensure-active-tools)))
      (should (hash-table-p tools))
      (should (hash-table-p (plist-get piem--state :active-tools))))))

(ert-deftest piem-test-response-set-model-updates-state ()
  "set_model response updates model in state."
  (let ((piem--state (list :model nil)))
    (piem--update-state-from-response
     '(:type "response"
       :command "set_model"
       :success t
       :data (:id "claude" :name "Claude")))
    (should (plist-get piem--state :model))
    (should (equal (plist-get (plist-get piem--state :model) :id) "claude"))))

(ert-deftest piem-test-response-cycle-thinking-updates-state ()
  "cycle_thinking_level response updates thinking-level."
  (let ((piem--state (list :thinking-level "off")))
    (piem--update-state-from-response
     '(:type "response"
       :command "cycle_thinking_level"
       :success t
       :data (:level "high")))
    (should (equal (plist-get piem--state :thinking-level) "high"))))

(ert-deftest piem-test-response-failed-does-not-update ()
  "Failed responses do not update state."
  (let ((piem--state (list :model '(:id "original"))))
    (piem--update-state-from-response
     '(:type "response"
       :command "set_model"
       :success :false
       :error "Model not found"))
    (should (equal (plist-get (plist-get piem--state :model) :id) "original"))))

(ert-deftest piem-test-event-dispatch-updates-state ()
  "Events update buffer-local state via handler."
  (let ((piem--status 'idle)
        (piem--state nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          ;; Register a handler that updates state
          (process-put fake-proc 'piem-display-handler
                       (lambda (e)
                         (piem--update-state-from-event e)))
          (piem--handle-event fake-proc '(:type "agent_start"))
          (should (eq piem--status 'streaming)))
      (delete-process fake-proc))))

;;;; State Management Tests

(ert-deftest piem-test-state-from-get-state-response ()
  "State is initialized from get_state response data."
  (let ((response '(:type "response"
                    :command "get_state"
                    :success t
                    :data (:model (:id "claude" :name "Claude")
                           :thinkingLevel "medium"
                           :isStreaming :false
                           :sessionId "test-123"
                           :messageCount 0))))
    (let ((state (piem--extract-state-from-response response)))
      (should state)
      (should (equal (plist-get state :session-id) "test-123"))
      (should (equal (plist-get state :thinking-level) "medium"))
      (should (eq (plist-get state :status) 'idle))
      (should (plist-get state :model)))))

(ert-deftest piem-test-state-extract-normalizes-remote-session-file ()
  "State extraction converts inbound remote process-local session files."
  (let ((response '(:type "response"
                    :command "get_state"
                    :success t
                    :data (:isStreaming :false
                           :isCompacting :false
                           :sessionFile "/home/pi/.pi/sessions/current.jsonl"))))
    (let ((state (piem--extract-state-from-response
                  response
                  "/ssh:pi-host:/home/pi/project/")))
      (should (equal (plist-get state :session-file)
                     "/ssh:pi-host:/home/pi/.pi/sessions/current.jsonl")))))

(ert-deftest piem-test-state-extract-normalizes-relative-session-file ()
  "State extraction resolves inbound relative session files under the anchor."
  (let ((response '(:type "response"
                    :command "get_state"
                    :success t
                    :data (:isStreaming :false
                           :isCompacting :false
                           :sessionFile "sessions/current.jsonl"))))
    (let ((state (piem--extract-state-from-response
                  response
                  "/ssh:pi-host:/home/pi/project/")))
      (should (equal (plist-get state :session-file)
                     "/ssh:pi-host:/home/pi/project/sessions/current.jsonl")))))

(ert-deftest piem-test-state-extract-ignores-unsafe-session-file ()
  "Unsafe passive sessionFile metadata is not stored as a navigable path."
  (let* ((bad (concat "/tmp/a" (string ?\0) "b.jsonl"))
         (response (list :type "response"
                         :command "get_state"
                         :success t
                         :data (list :isStreaming :false
                                     :isCompacting :false
                                     :sessionId "bad-session"
                                     :sessionFile bad))))
    (let ((state (piem--extract-state-from-response response)))
      (should (equal (plist-get state :session-id) "bad-session"))
      (should-not (plist-get state :session-file)))))

(ert-deftest piem-test-state-extract-status-idle ()
  "Extracted state has status idle when not streaming or compacting."
  (let ((response '(:type "response"
                    :success t
                    :data (:isStreaming :false :isCompacting :false))))
    (let ((state (piem--extract-state-from-response response)))
      (should (eq (plist-get state :status) 'idle)))))

(ert-deftest piem-test-state-extract-status-streaming ()
  "Extracted state has status streaming when isStreaming is true."
  (let ((response '(:type "response"
                    :success t
                    :data (:isStreaming t :isCompacting :false))))
    (let ((state (piem--extract-state-from-response response)))
      (should (eq (plist-get state :status) 'streaming)))))

(ert-deftest piem-test-state-extract-status-compacting ()
  "Extracted state has status compacting when isCompacting is true."
  (let ((response '(:type "response"
                    :success t
                    :data (:isStreaming :false :isCompacting t))))
    (let ((state (piem--extract-state-from-response response)))
      (should (eq (plist-get state :status) 'compacting)))))

;;;; Auto-Retry Event State Tests

(ert-deftest piem-test-event-auto-retry-start-sets-retrying ()
  "auto_retry_start event sets is-retrying to t."
  (let ((piem--status 'streaming)
        (piem--state (list :is-retrying nil)))
    (piem--update-state-from-event
     '(:type "auto_retry_start"
       :attempt 1
       :maxAttempts 3
       :delayMs 2000
       :errorMessage "429 rate_limit_error"))
    (should (eq (plist-get piem--state :is-retrying) t))
    (should (equal (plist-get piem--state :retry-attempt) 1))
    (should (equal (plist-get piem--state :last-error) "429 rate_limit_error"))))

(ert-deftest piem-test-event-auto-retry-end-success-clears-retrying ()
  "auto_retry_end with success clears is-retrying."
  (let ((piem--status 'streaming)
        (piem--state (list :is-retrying t :retry-attempt 2)))
    (piem--update-state-from-event
     '(:type "auto_retry_end"
       :success t
       :attempt 2))
    (should (eq (plist-get piem--state :is-retrying) nil))))

(ert-deftest piem-test-event-auto-retry-end-failure-stores-error ()
  "auto_retry_end with failure stores final error."
  (let ((piem--status 'streaming)
        (piem--state (list :is-retrying t)))
    (piem--update-state-from-event
     '(:type "auto_retry_end"
       :success :false
       :attempt 3
       :finalError "529 overloaded_error: Overloaded"))
    (should (eq (plist-get piem--state :is-retrying) nil))
    (should (equal (plist-get piem--state :last-error) "529 overloaded_error: Overloaded"))))

(ert-deftest piem-test-event-extension-error-stores-error ()
  "extension_error event stores error message in state."
  (let ((piem--status 'streaming)
        (piem--state (list :last-error nil)))
    (piem--update-state-from-event
     '(:type "extension_error"
       :extensionPath "/path/to/extension.ts"
       :event "tool_call"
       :error "TypeError: undefined is not a function"))
    (should (equal (plist-get piem--state :last-error)
                   "TypeError: undefined is not a function"))))

(ert-deftest piem-test-event-agent-start-clears-error-state ()
  "agent_start event clears error and retry state."
  (let ((piem--status 'idle)
        (piem--state (list :is-retrying t
                         :last-error "Previous error")))
    (piem--update-state-from-event '(:type "agent_start"))
    (should (eq piem--status 'streaming))
    (should (eq (plist-get piem--state :is-retrying) nil))
    (should (eq (plist-get piem--state :last-error) nil))))

(ert-deftest piem-test-event-agent-end-clears-retry-state ()
  "agent_end event clears retry state."
  (let ((piem--status 'streaming)
        (piem--state (list :is-retrying t)))
    (piem--update-state-from-event '(:type "agent_end" :messages []))
    (should (eq piem--status 'idle))
    (should (eq (plist-get piem--state :is-retrying) nil))))

;;;; Test Utilities

(ert-deftest piem-test-wait-until-succeeds ()
  "wait-until returns the predicate value when it becomes true."
  (let ((flag nil))
    (run-at-time 0.05 nil (lambda () (setq flag t)))
    (let ((result (piem-test-wait-until (lambda () flag) 0.5 0.01)))
      (should (eq result t)))))

(ert-deftest piem-test-wait-until-times-out ()
  "wait-until returns nil when the predicate stays false."
  (let ((result (piem-test-wait-until (lambda () nil) 0.05 0.01)))
    (should (null result))))

(ert-deftest piem-test-format-elapsed-rounds-to-millis ()
  "Elapsed formatter rounds to milliseconds with suffix."
  (should (equal (piem-test-format-elapsed 1.23456) "1.235s")))

;;;; Executable Customization Tests

(defun piem-test--capture-process-launch (executable extra-args &optional trust-policy directory)
  "Return the launch plist that `--start-process' passes to make-process.
Mocks `make-process' to capture all arguments, binding
`piem-executable' to EXECUTABLE,
`piem-extra-args' to EXTRA-ARGS,
`piem-project-trust-policy' to TRUST-POLICY or `approve',
and starting in DIRECTORY or `/tmp/'."
  (let ((piem-executable executable)
        (piem-extra-args extra-args)
        (piem-project-trust-policy (or trust-policy 'approve))
        (captured nil)
        (dummy-proc (start-process "piem-capture" nil "cat")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured
                             (append args
                                     (list :captured-default-directory
                                           default-directory)))
                       dummy-proc)))
            (ignore-errors (piem--start-process (or directory "/tmp/"))))
          captured)
      (when-let* ((stderr-buf (plist-get captured :stderr)))
        (when (buffer-live-p stderr-buf)
          (kill-buffer stderr-buf)))
      (when (process-live-p dummy-proc)
        (delete-process dummy-proc)))))

(ert-deftest piem-test-start-process-uses-custom-executable ()
  "start-process builds command from `piem-executable'."
  (should (equal (plist-get (piem-test--capture-process-launch '("npx" "pi") nil)
                            :command)
                 '("npx" "pi" "--mode" "rpc" "--approve"))))

(ert-deftest piem-test-start-process-custom-executable-with-extra-args ()
  "start-process combines custom executable, trust flag, and extra-args."
  (should (equal (plist-get (piem-test--capture-process-launch
                             '("npx" "pi") '("-e" "/path/to/ext.ts"))
                            :command)
                 '("npx" "pi" "--mode" "rpc" "-e" "/path/to/ext.ts" "--approve"))))

(ert-deftest piem-test-start-process-project-trust-default-omits-flag ()
  "A `default' project trust policy leaves Pi's own trust default in control."
  (should (equal (plist-get (piem-test--capture-process-launch
                             '("pi") nil 'default)
                            :command)
                 '("pi" "--mode" "rpc"))))

(ert-deftest piem-test-start-process-project-trust-no-approve-flag ()
  "A `no-approve' project trust policy tells Pi to ignore project-local files."
  (should (equal (plist-get (piem-test--capture-process-launch
                             '("pi") nil 'no-approve)
                            :command)
                 '("pi" "--mode" "rpc" "--no-approve"))))

(ert-deftest piem-test-start-process-captures-stderr-separately ()
  "start-process routes stderr away from the JSON-RPC stdout pipe."
  (let ((launch (piem-test--capture-process-launch '("pi") nil)))
    (should (bufferp (plist-get launch :stderr)))))

(ert-deftest piem-test-start-process-uses-default-directory-file-handler ()
  "start-process lets `default-directory' file handlers create the process."
  (let ((launch (piem-test--capture-process-launch
                 '("pi") nil 'approve "/ssh:test:/home/me/proj/")))
    (should (eq (plist-get launch :file-handler) t))
    (should (equal (plist-get launch :captured-default-directory)
                   "/ssh:test:/home/me/proj/"))))

(ert-deftest piem-test-start-process-remote-wraps-command-with-ready-marker ()
  "Remote start command emits a ready marker and preserves original argv."
  (let* ((launch (piem-test--capture-process-launch
                  '("npx" "pi") '("-e" "/path/to/ext.ts")
                  'approve "/ssh:test:/home/me/proj/"))
         (command (plist-get launch :command))
         (script (nth 2 command)))
    (should (equal (nth 0 command) "sh"))
    (should (equal (nth 1 command) "-c"))
    (should (string-match-p (regexp-quote piem--remote-ready-marker)
                            script))
    (should (string-match-p (regexp-quote "exec \"$0\" \"$@\"") script))
    (should (equal (nthcdr 3 command)
                   '("npx" "pi" "--mode" "rpc"
                     "-e" "/path/to/ext.ts" "--approve")))))

(ert-deftest piem-test-start-process-disables-main-query ()
  "start-process disables kill prompts for Emacs's main Pi process."
  (let ((piem-executable '("sh" "-c" "sleep 5"))
        (piem-extra-args nil)
        (proc nil))
    (unwind-protect
        (progn
          (setq proc (piem--start-process "/tmp/"))
          (should (process-live-p proc))
          (should-not (process-query-on-exit-flag proc)))
      (when (processp proc)
        (piem--cleanup-process-stderr-buffer proc)
        (when (process-live-p proc)
          (delete-process proc))))))

(ert-deftest piem-test-start-process-disables-stderr-query ()
  "start-process disables kill prompts for Emacs's stderr pipe process."
  (let ((piem-executable '("sh" "-c" "sleep 5"))
        (piem-extra-args nil)
        (proc nil))
    (unwind-protect
        (progn
          (setq proc (piem--start-process "/tmp/"))
          (let* ((stderr-buf (process-get proc 'piem-stderr-buf))
                 (stderr-proc (and stderr-buf (get-buffer-process stderr-buf))))
            (should (process-live-p proc))
            (should (buffer-live-p stderr-buf))
            (should stderr-proc)
            (should-not (process-query-on-exit-flag stderr-proc))))
      (when (processp proc)
        (piem--cleanup-process-stderr-buffer proc)
        (when (process-live-p proc)
          (delete-process proc))))))

(ert-deftest piem-test-cleanup-stderr-buffer-kills-stderr-process ()
  "stderr cleanup kills Emacs's stderr pipe process before killing its buffer."
  (let* ((stderr-buf (generate-new-buffer " *piem-test-stderr-cleanup*"))
         (proc (make-process :name "piem-cleanup-stderr"
                             :command '("sh" "-c" "sleep 5")
                             :connection-type 'pipe
                             :stderr stderr-buf)))
    (unwind-protect
        (let ((stderr-proc (get-buffer-process stderr-buf))
              (asked nil))
          (should stderr-proc)
          (should (process-query-on-exit-flag stderr-proc))
          (process-put proc 'piem-stderr-buf stderr-buf)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _)
                       (setq asked t)
                       (error "stderr cleanup should not prompt"))))
            (piem--cleanup-process-stderr-buffer proc))
          (should-not asked)
          (should-not (buffer-live-p stderr-buf))
          (should-not (process-live-p stderr-proc)))
      (when (buffer-live-p stderr-buf)
        (kill-buffer stderr-buf))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest piem-test-process-exit-includes-exit-code ()
  "Process exit errors include the process exit code."
  (let ((fake-proc (start-process "piem-exit-code" nil
                                  "sh" "-c" "exit 127"))
        (response nil))
    (unwind-protect
        (progn
          (while (process-live-p fake-proc)
            (accept-process-output fake-proc 0.05))
          (puthash "req_1" (lambda (r) (setq response r))
                   (piem--get-pending-requests fake-proc))
          (piem--handle-process-exit
           fake-proc "exited abnormally with code 127")
          (should (eq (plist-get response :processExit) t))
          (should (equal (plist-get response :exitCode) 127)))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest piem-test-process-exit-includes-stderr-excerpt ()
  "Process exit errors include stderr when available."
  (let ((fake-proc (start-process "piem-exit" nil "cat"))
        (stderr-buf (generate-new-buffer " *piem-test-stderr*"))
        (response nil))
    (unwind-protect
        (progn
          (with-current-buffer stderr-buf
            (insert "/tmp/undici.js:245\n"
                    "InvalidArgumentError: Invalid URL protocol: the URL must start with `http:` or `https:`.\n"
                    "    at parseURL (/tmp/undici.js:245:11)\n"
                    "Node.js v24.9.0\n"))
          (process-put fake-proc 'piem-stderr-buf stderr-buf)
          (puthash "req_1" (lambda (r) (setq response r))
                   (piem--get-pending-requests fake-proc))
          (piem--handle-process-exit fake-proc "exited abnormally with code 1")
          (should (equal (plist-get response :error)
                         "Process exited: exited abnormally with code 1"))
          (should (string-match-p "Invalid URL protocol"
                                  (plist-get response :stderr)))
          (should-not (buffer-live-p stderr-buf)))
      (when (buffer-live-p stderr-buf)
        (kill-buffer stderr-buf))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest piem-test-process-exit-truncates-long-stderr ()
  "Long stderr excerpts keep the beginning and end without growing unbounded."
  (let ((fake-proc (start-process "piem-exit-long" nil "cat"))
        (stderr-buf (generate-new-buffer " *piem-test-stderr-long*"))
        (response nil))
    (unwind-protect
        (progn
          (with-current-buffer stderr-buf
            (insert "START InvalidArgumentError: Invalid URL protocol\n")
            (insert (make-string 5000 ?x))
            (insert "\nEND Node.js v24.9.0\n"))
          (process-put fake-proc 'piem-stderr-buf stderr-buf)
          (puthash "req_2" (lambda (r) (setq response r))
                   (piem--get-pending-requests fake-proc))
          (piem--handle-process-exit fake-proc "exited abnormally with code 1")
          (let ((stderr (plist-get response :stderr)))
            (should (string-match-p "START InvalidArgumentError" stderr))
            (should (string-match-p "stderr truncated" stderr))
            (should (string-match-p "END Node.js" stderr))
            (should (< (length stderr) 4100))))
      (when (buffer-live-p stderr-buf)
        (kill-buffer stderr-buf))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

;;;; Process Filter Tests

(ert-deftest piem-test-process-filter-inhibits-redisplay ()
  "Process filter binds `inhibit-redisplay' to t during dispatch.
This batches N JSON lines delivered in one read() into a single
redisplay cycle instead of triggering N separate redraws."
  (let ((captured-inhibit nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (process-put fake-proc 'piem-display-handler
                       (lambda (_e) (setq captured-inhibit inhibit-redisplay)))
          (piem--process-filter
           fake-proc "{\"type\":\"agent_start\"}\n")
          (should (eq captured-inhibit t)))
      (delete-process fake-proc))))

(ert-deftest piem-test-process-filter-continues-after-callback-error ()
  "A failing response callback does not drop later complete lines."
  (let ((second-called nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (piem--get-pending-requests fake-proc))
              (pending-types (piem--get-pending-command-types fake-proc)))
          (puthash "req_1" (lambda (_response) (error "boom")) pending)
          (puthash "req_2" (lambda (_response) (setq second-called t)) pending)
          (puthash "req_1" "get_state" pending-types)
          (puthash "req_2" "get_history" pending-types)
          (cl-letf (((symbol-function 'message) (lambda (&rest _args) nil)))
            (piem--process-filter
             fake-proc
             (concat "{\"type\":\"response\",\"id\":\"req_1\",\"success\":true}\n"
                     "{\"type\":\"response\",\"id\":\"req_2\",\"success\":true}\n")))
          (should second-called)
          (should-not (gethash "req_1" pending))
          (should-not (gethash "req_2" pending)))
      (delete-process fake-proc))))

(ert-deftest piem-test-process-filter-ready-marker-not-dispatched ()
  "The remote ready marker is consumed before JSON/event dispatch."
  (let ((events nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (process-put fake-proc 'piem-awaiting-ready-marker t)
          (process-put fake-proc 'piem-display-handler
                       (lambda (event) (push event events)))
          (piem--process-filter
           fake-proc (concat piem--remote-ready-marker "\n"))
          (should (process-get fake-proc 'piem-ready))
          (should-not events))
      (delete-process fake-proc))))

(ert-deftest piem-test-process-filter-chunked-ready-marker-flushes ()
  "A ready marker split across filter calls still flushes queued writes."
  (let ((sent nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (_proc string) (push string sent))))
          (process-put fake-proc 'piem-awaiting-ready-marker t)
          (piem--send-string fake-proc "first\n")
          (let* ((marker piem--remote-ready-marker)
                 (split (/ (length marker) 2)))
            (piem--process-filter fake-proc (substring marker 0 split))
            (should-not sent)
            (should-not (process-get fake-proc 'piem-ready))
            (piem--process-filter
             fake-proc (concat (substring marker split) "\n")))
          (should (process-get fake-proc 'piem-ready))
          (should (equal (nreverse sent) '("first\n"))))
      (delete-process fake-proc))))

(ert-deftest piem-test-process-filter-keeps-partial-output-chunked ()
  "Process filter dispatches only complete JSON lines and clears chunk state."
  (let ((events nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (process-put fake-proc 'piem-display-handler
                       (lambda (event) (push event events)))
          (process-put fake-proc 'piem-partial-output-chunks
                       '("{\"type\":\"agent"))
          (piem--process-filter fake-proc "_start\"")
          (should (null events))
          (should (equal (process-get fake-proc
                                      'piem-partial-output-chunks)
                         '("_start\"" "{\"type\":\"agent")))
          (piem--process-filter
           fake-proc "}\n{\"type\":\"agent_stop\"}\npartial")
          (should (equal (mapcar (lambda (event) (plist-get event :type))
                                 (nreverse events))
                         '("agent_start" "agent_stop")))
          (should (equal (process-get fake-proc
                                      'piem-partial-output-chunks)
                         '("partial")))
          (setq events nil)
          (piem--process-filter fake-proc "\n")
          (should (null events))
          (should (null (process-get fake-proc
                                     'piem-partial-output-chunks))))
      (delete-process fake-proc))))

(ert-deftest piem-test-process-filter-get-state-ignores-nul-session-file ()
  "A get_state response with NUL sessionFile does not escape the filter."
  (let ((piem--status 'idle)
        (piem--state nil)
        (callback-called nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (puthash "req_state"
                   (lambda (response)
                     (setq callback-called t)
                     (piem--update-state-from-response response))
                   (piem--get-pending-requests fake-proc))
          (puthash "req_state" "get_state"
                   (piem--get-pending-command-types fake-proc))
          (let ((err nil))
            (condition-case caught
                (piem--process-filter
                 fake-proc
                 "{\"type\":\"response\",\"id\":\"req_state\",\"command\":\"get_state\",\"success\":true,\"data\":{\"isStreaming\":false,\"isCompacting\":false,\"sessionId\":\"nul-session\",\"sessionFile\":\"/tmp/a\\u0000b.jsonl\"}}\n")
              (error (setq err caught)))
            (should-not err))
          (should callback-called)
          (should (equal (plist-get piem--state :session-id)
                         "nul-session"))
          (should-not (plist-get piem--state :session-file)))
      (delete-process fake-proc))))

(provide 'piem-core-test)
;;; piem-core-test.el ends here
