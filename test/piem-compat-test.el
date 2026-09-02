;;; piem-compat-test.el --- Tests for the pi-coding-agent compatibility stub -*- lexical-binding: t; -*-

;;; Commentary:

;; Contract tests for the deprecated `pi-coding-agent' aliases that ship
;; after the rename to `piem' (v3.0.0).  Old user configurations must keep
;; working: (require 'pi-coding-agent), M-x pi-coding-agent-* commands,
;; customize variables, faces, the customize group, and
;; `with-eval-after-load' forms.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pi-coding-agent)

(defconst piem-compat-test-command-aliases
  '(pi-coding-agent
    pi-coding-agent-toggle
    pi-coding-agent-open-session-file
    pi-coding-agent-new-session
    pi-coding-agent-reload
    pi-coding-agent-session-browser
    pi-coding-agent-tree-browser
    pi-coding-agent-install-grammars
    pi-coding-agent-attach-image
    pi-coding-agent-evil-setup)
  "Old command names that must remain available as deprecated aliases.")

(defconst piem-compat-test-variable-aliases
  '(pi-coding-agent-executable
    pi-coding-agent-project-trust-policy
    pi-coding-agent-rpc-timeout
    pi-coding-agent-input-window-height
    pi-coding-agent-quit-without-confirmation
    pi-coding-agent-evil-integration
    pi-coding-agent-evil-chat-state
    pi-coding-agent-evil-input-state)
  "Old variable names that must remain available as deprecated aliases.")

(defconst piem-compat-test-face-aliases
  '(pi-coding-agent-timestamp
    pi-coding-agent-tool-name
    pi-coding-agent-tool-command
    pi-coding-agent-tool-output
    pi-coding-agent-tool-block
    pi-coding-agent-tool-block-error
    pi-coding-agent-diff-line-added
    pi-coding-agent-diff-line-removed
    pi-coding-agent-collapsed-indicator
    pi-coding-agent-model-name
    pi-coding-agent-activity-phase
    pi-coding-agent-retry-notice
    pi-coding-agent-error-notice
    pi-coding-agent-session-name
    pi-coding-agent-session-message-count
    pi-coding-agent-session-age
    pi-coding-agent-session-thread-connector
    pi-coding-agent-session-group-header
    pi-coding-agent-tree-user
    pi-coding-agent-tree-assistant
    pi-coding-agent-tree-tool
    pi-coding-agent-tree-compaction
    pi-coding-agent-tree-summary
    pi-coding-agent-tree-active
    pi-coding-agent-tree-label
    pi-coding-agent-tree-connector)
  "Old face names that must remain available as deprecated aliases.")

(defun piem-compat-test--new-name (old-name)
  "Return the piem replacement for OLD-NAME."
  (intern (concat "piem" (substring (symbol-name old-name)
                                    (length "pi-coding-agent")))))

(defun piem-compat-test--stub-source ()
  "Return the source text of the pi-coding-agent stub file."
  (let ((file (locate-library "pi-coding-agent.el")))
    (should file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

;;;; The stub itself

(ert-deftest piem-compat-test-stub-provides-old-feature ()
  "The old feature name still loads and provides itself."
  (should (featurep 'pi-coding-agent)))

(ert-deftest piem-compat-test-stub-defines-deprecated-group ()
  "The old customize group is still defined and points at `piem'."
  (should (get 'pi-coding-agent 'group-documentation)))

;;;; Command aliases

(ert-deftest piem-compat-test-command-aliases-are-declared-obsolete ()
  "Every old command is a declared obsolete alias, not a silent copy.
The piem-evil aliases need `piem-evil' loaded so the alias resolves."
  (require 'piem-evil)
  (dolist (old piem-compat-test-command-aliases)
    (should (fboundp old))
    (should (get old 'byte-obsolete-info))))

(ert-deftest piem-compat-test-command-aliases-reach-piem-commands ()
  "Every old command resolves through to its piem replacement."
  (require 'piem-evil)
  (dolist (old piem-compat-test-command-aliases)
    (let ((new (piem-compat-test--new-name old)))
      (should (eq (indirect-function old) (indirect-function new))))))

(ert-deftest piem-compat-test-command-aliases-carry-autoload-cookies ()
  "The stub source puts an autoload cookie on each obsolete command alias.
This is what makes `M-x pi-coding-agent' work before the stub is loaded,
keeping the README's (defalias 'pi ...) tip alive for package users."
  (let ((source (piem-compat-test--stub-source)))
    (dolist (old piem-compat-test-command-aliases)
      (should (string-match-p
               (concat ";;;###autoload[ \t]*\n?[ \t]*(define-obsolete-function-alias"
                       "[\n\r\t ]+'" (regexp-quote (symbol-name old)) "[\n\r\t ]")
               source)))))

;;;; Variable aliases

(ert-deftest piem-compat-test-variable-aliases-read-through ()
  "Old variables read through to their piem replacements.
`piem-evil' is loaded first so the evil defcustoms exist."
  (require 'piem-evil)
  (dolist (old piem-compat-test-variable-aliases)
    (let ((new (piem-compat-test--new-name old)))
      (should (boundp old))
      (should (eq (default-value old) (default-value new))))))

(ert-deftest piem-compat-test-variable-aliases-are-declared-obsolete ()
  "Every old variable carries declared obsolescence."
  (require 'piem-evil)
  (dolist (old piem-compat-test-variable-aliases)
    (should (get old 'byte-obsolete-variable))))

;;;; Face aliases

(ert-deftest piem-compat-test-face-aliases ()
  "Every old face is a face alias for its piem replacement."
  (dolist (old piem-compat-test-face-aliases)
    (should (eq (get old 'face-alias)
                (piem-compat-test--new-name old)))))

;;;; Pre-load settings under old names

;; `defvaralias' overwrites — rather than transfers — an existing old-name
;; binding when the base variable is already bound, and Customize entries
;; queued on the old name before the stub loads never apply (smoke findings
;; B2 and B5).  These tests reproduce a user init file that sets old names
;; before the stub is loaded, which requires a fresh Emacs: by the time this
;; suite runs, the aliases already exist and reads/writes go through them.

(defun piem-compat-test--child-emacs (program)
  "Run a child Emacs executing PROGRAM, a string of Elisp.
The child inherits this session's `load-path' so the repository
sources and package dependencies resolve exactly as in the parent.
Return (OUTPUT . EXIT-CODE), where OUTPUT combines stdout and
stderr so load-time warnings are visible."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (logfile (make-temp-file "piem-compat-child" nil ".txt"))
         (exit-code
          (call-process emacs nil `((:file ,logfile) t) nil
                        "--batch" "-Q"
                        "--eval" "(setq load-prefer-newer t)"
                        "--eval" (format "(setq load-path '%S)" load-path)
                        "--eval" program)))
    (unwind-protect
        (cons (with-temp-buffer
                (insert-file-contents logfile)
                (buffer-string))
              exit-code)
      (delete-file logfile))))

(defconst piem-compat-test--result-regexp "\\n?RESULT \\(.*\\)\\n?"
  "Regexp extracting the RESULT line printed by child Emacs drivers.")

(defun piem-compat-test--child-result (output)
  "Return the RESULT message from child OUTPUT as a string."
  (unless (string-match piem-compat-test--result-regexp output)
    (error "No RESULT line in child output: %s" output))
  (match-string 1 output))

(ert-deftest piem-compat-test-preload-setq-migrates-to-piem ()
  "Values set under old names before the stub loads reach the piem vars.
Covers `setq' in a user init file that runs before the stub is
loaded (smoke finding B2): the old binding must transfer to the
piem variable instead of being dropped with a defvaralias warning.
The evil variable covers the case where the piem defcustom has not
even run when the stub loads."
  (pcase-let* ((program "(progn
  (setq pi-coding-agent-executable '(\"/opt/renamed/bin/pi\")
        pi-coding-agent-rpc-timeout 42
        pi-coding-agent-evil-input-state 'normal)
  (require 'pi-coding-agent)
  (require 'piem-evil)
  (message \"RESULT %S\"
           (list piem-executable piem-rpc-timeout
                 piem-evil-input-state
                 (eq (indirect-variable 'pi-coding-agent-executable)
                     'piem-executable))))")
              (`(,output . ,exit-code)
               (piem-compat-test--child-emacs program)))
    (should (zerop exit-code))
    (should (equal (read (piem-compat-test--child-result output))
                   '(("/opt/renamed/bin/pi") 42 normal t)))
    (should-not (string-match-p "Overwriting value" output))))

(ert-deftest piem-compat-test-preload-customize-entries-migrate ()
  "Customize entries queued on old names before load land on piem vars.
A `custom-set-variables' block saved under the old names queues
values on the old symbols when the variables are not defined yet;
those entries were dropped silently before (smoke finding B5)."
  (pcase-let* ((program "(progn
  (custom-set-variables
   '(pi-coding-agent-rpc-timeout 42)
   '(pi-coding-agent-input-window-height 9)
   '(pi-coding-agent-quit-without-confirmation t))
  (require 'pi-coding-agent)
  (let ((queued (list piem-rpc-timeout
                      piem-input-window-height
                      piem-quit-without-confirmation)))
    (custom-set-variables '(pi-coding-agent-input-window-height 7))
    (message \"RESULT %S\" (list queued piem-input-window-height))))")
              (`(,output . ,exit-code)
               (piem-compat-test--child-emacs program)))
    (should (zerop exit-code))
    (should (equal (read (piem-compat-test--child-result output))
                   '((42 9 t) 7)))))

(ert-deftest piem-compat-test-preload-new-name-setting-wins ()
  "A piem value set before the stub loads beats the old-name value.
When both names carry user values at stub-load time, the new-name
setting wins and the old one is discarded without a warning."
  (pcase-let* ((program "(progn
  (setq pi-coding-agent-rpc-timeout 42
        piem-rpc-timeout 55)
  (require 'pi-coding-agent)
  (message \"RESULT %S\"
           (list piem-rpc-timeout
                 (eq (indirect-variable 'pi-coding-agent-rpc-timeout)
                     'piem-rpc-timeout))))")
              (`(,output . ,exit-code)
               (piem-compat-test--child-emacs program)))
    (should (zerop exit-code))
    (should (equal (read (piem-compat-test--child-result output))
                   '(55 t)))
    (should-not (string-match-p "Overwriting value" output))))

(provide 'piem-compat-test)
;;; piem-compat-test.el ends here
