;;; piem-compat-test.el --- Tests for the pi-coding-agent compatibility stub -*- lexical-binding: t; -*-

;;; Commentary:

;; Contract tests for the deprecated `pi-coding-agent' aliases that ship
;; after the rename to `piem' (v3.0.0).  Old user configurations must keep
;; working: (require 'pi-coding-agent), M-x pi-coding-agent-* commands,
;; customize variables, faces, the customize group, and
;; `with-eval-after-load' forms.

;;; Code:

(require 'ert)
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

(provide 'piem-compat-test)
;;; piem-compat-test.el ends here
