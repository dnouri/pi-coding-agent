;;; piem-ui-test.el --- Tests for piem-ui -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;;; Commentary:

;; Tests for buffer naming, creation, major modes, session directory,
;; buffer linkage, and startup header — the UI foundation layer.

;;; Code:

(require 'ert)
(require 'warnings)  ; ensure display-warning is loaded (not autoloaded)
(require 'piem)
(require 'piem-test-common)

;;; Buffer Naming

(ert-deftest piem-test-buffer-name-chat ()
  "Buffer name for chat includes abbreviated directory."
  (let ((name (piem--buffer-name :chat "/home/user/project/")))
    (should (string-match-p "\\*piem-chat:" name))
    (should (string-match-p "project" name))))

(ert-deftest piem-test-buffer-name-input ()
  "Buffer name for input includes abbreviated directory."
  (let ((name (piem--buffer-name :input "/home/user/project/")))
    (should (string-match-p "\\*piem-input:" name))
    (should (string-match-p "project" name))))

(ert-deftest piem-test-buffer-name-abbreviates-home ()
  "Buffer name abbreviates home directory to ~."
  (let ((name (piem--buffer-name :chat (expand-file-name "~/myproject/"))))
    (should (string-match-p "~" name))))

(ert-deftest piem-test-buffer-name-preserves-multi-hop-route ()
  "Buffer names keep the full TRAMP route for remote sessions."
  (let* ((dir "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
         (name (piem--buffer-name :chat dir)))
    (should (string-match-p (regexp-quote dir) name))
    (should-not (string-match-p (regexp-quote "/sudo:root@pi-host:")
                                name))))

(ert-deftest piem-test-path-to-language-known-extension ()
  "path-to-language returns correct language for known extensions."
  (should (equal "python" (piem--path-to-language "/tmp/foo.py")))
  (should (equal "javascript" (piem--path-to-language "/tmp/bar.js")))
  (should (equal "emacs-lisp" (piem--path-to-language "/tmp/baz.el"))))

(ert-deftest piem-test-path-to-language-unknown-extension ()
  "path-to-language returns 'text' for unknown extensions.
This ensures all files get code fences for consistent display."
  (should (equal "text" (piem--path-to-language "/tmp/foo.txt")))
  (should (equal "text" (piem--path-to-language "/tmp/bar.xyz")))
  (should (equal "text" (piem--path-to-language "/tmp/noext"))))

(ert-deftest piem-test-path-to-language-ignores-non-string ()
  "path-to-language returns nil for malformed path metadata."
  (should-not (piem--path-to-language '(:not "a path")))
  (should-not (piem--path-to-language ["not" "a" "path"])))

;;; Buffer Creation

(ert-deftest piem-test-get-or-create-buffer-creates-new ()
  "get-or-create-buffer creates a new buffer if none exists."
  (let* ((dir "/tmp/piem-test-unique-12345/")
         (buf (piem--get-or-create-buffer :chat dir)))
    (unwind-protect
        (progn
          (should (bufferp buf))
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest piem-test-get-or-create-buffer-returns-existing ()
  "get-or-create-buffer returns existing buffer."
  (let* ((dir "/tmp/piem-test-unique-67890/")
         (buf1 (piem--get-or-create-buffer :chat dir))
         (buf2 (piem--get-or-create-buffer :chat dir)))
    (unwind-protect
        (should (eq buf1 buf2))
      (when (buffer-live-p buf1)
        (kill-buffer buf1)))))

;;; Major Modes

(ert-deftest piem-test-chat-mode-is-read-only ()
  "piem-chat-mode sets buffer to read-only."
  (with-temp-buffer
    (piem-chat-mode)
    (should buffer-read-only)))

(ert-deftest piem-test-chat-mode-disables-undo-history ()
  "Generated chat updates do not accumulate undo history."
  (with-temp-buffer
    (piem-chat-mode)
    (should (eq buffer-undo-list t))
    (let ((inhibit-read-only t))
      (insert "streamed response")
      (delete-region (point-min) (point-max)))
    (should (eq buffer-undo-list t))))

(ert-deftest piem-test-chat-mode-has-word-wrap ()
  "piem-chat-mode enables word wrap."
  (with-temp-buffer
    (piem-chat-mode)
    (should word-wrap)
    (should-not truncate-lines)))

(ert-deftest piem-test-chat-mode-disables-hl-line ()
  "piem-chat-mode disables hl-line to prevent scroll oscillation."
  (with-temp-buffer
    (piem-chat-mode)
    (should-not hl-line-mode)
    (should-not (buffer-local-value 'global-hl-line-mode (current-buffer)))))

(ert-deftest piem-test-chat-mode-is-special-buffer-mode ()
  "Chat mode advertises the standard special-buffer contract."
  (should (eq (get 'piem-chat-mode 'mode-class) 'special)))

(ert-deftest piem-test-chat-mode-adds-window-change-hook ()
  "piem-chat-mode installs the buffer-local width refresh hook."
  (with-temp-buffer
    (piem-chat-mode)
    (should (local-variable-p 'window-configuration-change-hook))
    (should (memq #'piem--maybe-refresh-hot-tail-tables
                  window-configuration-change-hook))))

(ert-deftest piem-test-chat-mode-initializes-with-theme-derived-diff-faces ()
  "Chat mode startup should not depend on diff-mode being loaded elsewhere."
  (with-temp-buffer
    (let ((debug-on-error t))
      (piem-chat-mode)
      (should (derived-mode-p 'piem-chat-mode)))))

(ert-deftest piem-test-thinking-display-default-is-visible ()
  "Package default keeps completed thinking expanded in new chat buffers."
  (should (eq (default-value 'piem-thinking-display) 'visible)))

(ert-deftest piem-test-chat-mode-initializes-thinking-display-from-default ()
  "New chat buffers inherit the configured completed-thinking display default."
  (let ((piem-thinking-display 'hidden))
    (with-temp-buffer
      (piem-chat-mode)
      (should (eq piem--thinking-display 'hidden)))))

(ert-deftest piem-test-thinking-display-override-is-buffer-local ()
  "Changing one chat buffer's thinking display leaves others and the default alone."
  (let ((piem-thinking-display 'visible)
        (buf-a (generate-new-buffer " *pi-thinking-display-a*"))
        (buf-b (generate-new-buffer " *pi-thinking-display-b*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (piem-chat-mode)
            (piem--set-thinking-display 'hidden))
          (with-current-buffer buf-b
            (piem-chat-mode))
          (should (eq piem-thinking-display 'visible))
          (should (eq (buffer-local-value 'piem--thinking-display buf-a) 'hidden))
          (should (eq (buffer-local-value 'piem--thinking-display buf-b) 'visible)))
      (when (buffer-live-p buf-a)
        (kill-buffer buf-a))
      (when (buffer-live-p buf-b)
        (kill-buffer buf-b)))))

(ert-deftest piem-test-theme-diff-background-prefers-diff-face-background ()
  "Theme-derived diff lines should reuse an existing diff background first."
  (cl-letf (((symbol-function 'face-background)
             (lambda (face &optional _frame _inherit)
               (pcase face
                 ('diff-added "#224422")
                 ('default "#111111")
                 (_ nil))))
            ((symbol-function 'color-defined-p)
             (lambda (color) (stringp color))))
    (should (equal (piem--theme-diff-background
                    'diff-added 'diff-indicator-added)
                   "#224422"))))

(ert-deftest piem-test-theme-diff-background-prefers-diff-face-foreground ()
  "Theme-derived diff lines should prefer the diff face foreground before the indicator."
  (cl-letf (((symbol-function 'face-background)
             (lambda (face &optional _frame _inherit)
               (pcase face
                 ('diff-added nil)
                 ('default "#111111")
                 (_ nil))))
            ((symbol-function 'face-foreground)
             (lambda (face &optional _frame _inherit)
               (pcase face
                 ('diff-added "#bb3333")
                 ('diff-indicator-added "#22aa22")
                 (_ nil))))
            ((symbol-function 'color-defined-p)
             (lambda (color) (stringp color))))
    (should (equal (piem--theme-diff-background
                    'diff-added 'diff-indicator-added)
                   (piem--blend-color "#111111" "#bb3333" 0.20)))))

(ert-deftest piem-test-theme-diff-background-falls-back-to-indicator-foreground ()
  "Theme-derived diff lines should fall back to the indicator color when needed."
  (cl-letf (((symbol-function 'face-background)
             (lambda (face &optional _frame _inherit)
               (pcase face
                 ('diff-added nil)
                 ('default "#fefefe")
                 (_ nil))))
            ((symbol-function 'face-foreground)
             (lambda (face &optional _frame _inherit)
               (pcase face
                 ('diff-added nil)
                 ('diff-indicator-added "#22aa22")
                 (_ nil))))
            ((symbol-function 'color-defined-p)
             (lambda (color) (stringp color))))
    (should (equal (piem--theme-diff-background
                    'diff-added 'diff-indicator-added)
                   (piem--blend-color "#fefefe" "#22aa22" 0.10)))))

(ert-deftest piem-test-update-theme-derived-faces-uses-background-only-overlays ()
  "Theme-derived overlay faces should only contribute background tint."
  (let (calls)
    (cl-letf (((symbol-function 'face-background)
               (lambda (face &optional _frame _inherit)
                 (pcase face
                   ('default "#111111")
                   ('diff-added "#224422")
                   ('diff-removed nil)
                   (_ nil))))
              ((symbol-function 'face-foreground)
               (lambda (face &optional _frame _inherit)
                 (pcase face
                   ('diff-removed "#bb3333")
                   ('diff-indicator-removed "#aa2222")
                   (_ nil))))
              ((symbol-function 'color-defined-p)
               (lambda (color) (stringp color)))
              ((symbol-function 'set-face-attribute)
               (lambda (face _frame &rest args)
                 (push (cons face args) calls))))
      (piem--update-theme-derived-faces)
      (dolist (face '(piem-diff-line-added
                      piem-diff-line-removed
                      piem-tool-block-error))
        (let ((args (cdr (assq face calls))))
          (should args)
          (should (eq (plist-get args :inherit) nil))
          (should (eq (plist-get args :foreground) 'unspecified))
          (should (stringp (plist-get args :background)))
          (should (eq (plist-get args :extend) t)))))))

(ert-deftest piem-test-chat-mode-write-file-preserves-chat-state ()
  "`write-file' keeps chat buffers in chat mode with file backing attached."
  (let ((file nil)
        (root (piem-test--make-temp-directory
               "piem-test-write-file-"))
        (make-backup-files nil))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (setq default-directory root)
          (setq piem--process 'mock-process)
          (let ((inhibit-read-only t))
            (insert "Assistant\n=========\n\nHello\n"))
          (setq file (piem-test--write-chat-buffer
                      (current-buffer) "piem-chat-write-"))
          (should (derived-mode-p 'piem-chat-mode))
          (should (eq piem--process 'mock-process))
          (should (equal buffer-file-name file))
          (should buffer-read-only))
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

(ert-deftest piem-test-chat-mode-save-buffer-keeps-writing-to-bound-file ()
  "Later `save-buffer' keeps writing to the same file-backed chat buffer."
  (let ((file nil)
        (make-backup-files nil))
    (unwind-protect
        (progn
          (with-temp-buffer
            (piem-chat-mode)
            (let ((inhibit-read-only t))
              (insert "Assistant\n=========\n\nHello\n"))
            (setq file (piem-test--write-chat-buffer
                        (current-buffer) "piem-chat-save-"))
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "More\n"))
            (save-buffer)
            (should (derived-mode-p 'piem-chat-mode))
            (should (equal buffer-file-name file))
            (should buffer-read-only))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string)
                           "Assistant\n=========\n\nHello\nMore\n"))))
      (ignore-errors (delete-file file)))))

(ert-deftest piem-test-session-chat-write-file-preserves-canonical-name-and-directory ()
  "Session chat buffers keep their canonical identity after `write-file'."
  (let ((root (piem-test--make-temp-directory
               "piem-test-write-file-session-"))
        (file nil)
        (chat nil)
        (input nil)
        (make-backup-files nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'piem--start-process) (lambda (_) nil)))
          (setq chat (piem--setup-session root nil)
                input (buffer-local-value 'piem--input-buffer chat))
          (setq file (piem-test--write-chat-buffer
                      chat "piem-chat-session-" "Saved copy\n"))
          (with-current-buffer chat
            (should (equal (piem--chat-session-buffer-name)
                           (piem-test--chat-buffer-name root)))
            (should (equal (piem--session-directory) root))
            (should (equal buffer-file-name file))
            (should buffer-read-only)))
      (piem-test--kill-live-buffers input chat)
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

(ert-deftest piem-test-input-mode-keeps-own-mode-with-markdown-default ()
  "piem-input-mode keeps its identity with markdown highlighting."
  (with-temp-buffer
    (piem-input-mode)
    (should (derived-mode-p 'piem-input-mode))
    (should (derived-mode-p 'text-mode))
    (should-not md-ts-hide-markup)))

(ert-deftest piem-test-input-mode-not-read-only ()
  "piem-input-mode allows editing."
  (with-temp-buffer
    (piem-input-mode)
    (should-not buffer-read-only)))

;;; Session Directory Detection

(ert-deftest piem-test-session-directory-uses-project-root ()
  "Session directory is project root when in a project."
  (let ((default-directory "/tmp/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) '(vc . "/home/user/myproject/")))
              ((symbol-function 'project-root)
               (lambda (_) "/home/user/myproject/")))
      (should (equal (piem--session-directory) "/home/user/myproject/")))))

(ert-deftest piem-test-session-directory-falls-back-to-default ()
  "Session directory is default-directory when not in a project."
  (let ((default-directory "/tmp/somedir/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) nil)))
      (should (equal (piem--session-directory) "/tmp/somedir/")))))

(ert-deftest piem-test-session-directory-preserves-multi-hop-root ()
  "Session directory detection keeps multi-hop TRAMP project roots intact."
  (let ((default-directory "/tmp/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _)
                 '(vc . "/ssh:bastion|sudo:root@pi-host:/srv/project/")))
              ((symbol-function 'project-root)
               (lambda (_)
                 "/ssh:bastion|sudo:root@pi-host:/srv/project/")))
      (should (equal (piem--session-directory)
                     "/ssh:bastion|sudo:root@pi-host:/srv/project/")))))

(ert-deftest piem-test-session-directory-recovers-projectile-root ()
  "Recovers root when a backend returns a cons cell with no `project-root'.
Older projectile returns (projectile . DIR) but defines no method, raising
`cl-no-applicable-method' (issue #234)."
  (let ((default-directory "/tmp/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) (cons 'projectile "/home/user/proj/")))
              ((symbol-function 'project-root)
               (lambda (_proj)
                 (signal 'cl-no-applicable-method
                         (list 'project-root
                               (cons 'projectile "/home/user/proj/"))))))
      (should (equal (piem--session-directory) "/home/user/proj/")))))

(ert-deftest piem-test-session-directory-survives-malformed-backend ()
  "Degrades to `default-directory' when a backend's root can't be found.
Closes the category from issue #234: any instance without a usable
`(SYMBOL . DIR)' shape must not crash session startup."
  (let ((default-directory "/tmp/somedir/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) (vector 'weird-backend)))
              ((symbol-function 'project-root)
               (lambda (_proj)
                 (signal 'cl-no-applicable-method
                         (list 'project-root
                               (vector 'weird-backend))))))
      (should (equal (piem--session-directory) "/tmp/somedir/")))))

;;; Buffer Linkage

(defvar-local piem-test--activity-marker nil
  "Buffer-local marker used by activity-phase hook tests.")

(ert-deftest piem-test-input-buffer-finds-chat ()
  "Input buffer can find associated chat buffer."
  (piem-test-with-mock-session "/tmp/piem-test-link1/"
    (with-current-buffer "*piem-input:/tmp/piem-test-link1/*"
      (should (eq (piem--get-chat-buffer)
                  (get-buffer "*piem-chat:/tmp/piem-test-link1/*"))))))

(ert-deftest piem-test-chat-buffer-finds-input ()
  "Chat buffer can find associated input buffer."
  (piem-test-with-mock-session "/tmp/piem-test-link2/"
    (with-current-buffer "*piem-chat:/tmp/piem-test-link2/*"
      (should (eq (piem--get-input-buffer)
                  (get-buffer "*piem-input:/tmp/piem-test-link2/*"))))))

(ert-deftest piem-test-activity-phase-functions-receive-session-buffers ()
  "Activity phase functions receive buffers, phases, and reason."
  (let ((calls nil)
        (dir "/tmp/piem-test-activity-hook/"))
    (piem-test-with-mock-session dir
      (let ((chat (get-buffer (piem--buffer-name :chat dir)))
            (input (get-buffer (piem--buffer-name :input dir)))
            (piem-activity-phase-functions
             (list (lambda (chat-buf input-buf old-phase new-phase reason)
                     (push (list chat-buf input-buf old-phase new-phase reason)
                           calls)))))
        (with-current-buffer chat
          (piem--set-activity-phase "thinking")
          (piem--set-activity-phase "thinking"))
        (should (= (length calls) 1))
        (pcase-let ((`(,seen-chat ,seen-input ,old-phase ,new-phase ,reason)
                     (car calls)))
          (should (eq seen-chat chat))
          (should (eq seen-input input))
          (should (equal old-phase "idle"))
          (should (equal new-phase "thinking"))
          (should (eq reason 'phase-change)))))))

(ert-deftest piem-test-reset-session-state-forces-idle-activity-phase ()
  "Session reset applies idle even when user display state needs resync."
  (let ((calls nil)
        (dir "/tmp/piem-test-activity-reset/"))
    (piem-test-with-mock-session dir
      (let ((chat (get-buffer (piem--buffer-name :chat dir)))
            (input (get-buffer (piem--buffer-name :input dir)))
            (piem-activity-phase-functions
             (list (lambda (chat-buf input-buf old-phase new-phase reason)
                     (push (list chat-buf input-buf old-phase new-phase reason)
                           calls)))))
        (with-current-buffer chat
          (piem--set-activity-phase "running")
          (setq calls nil)
          (piem--reset-session-state))
        (should (= (length calls) 1))
        (pcase-let ((`(,seen-chat ,seen-input ,old-phase ,new-phase ,reason)
                     (car calls)))
          (should (eq seen-chat chat))
          (should (eq seen-input input))
          (should (equal old-phase "running"))
          (should (equal new-phase "idle"))
          (should (eq reason 'reset)))))))

(ert-deftest piem-test-set-input-buffer-resyncs-activity-phase ()
  "Relinking an input buffer reapplies the current activity phase."
  (let ((calls nil)
        (dir "/tmp/piem-test-activity-relink/")
        (new-input (generate-new-buffer " *pi-activity-relink-input*")))
    (unwind-protect
        (piem-test-with-mock-session dir
          (let ((chat (get-buffer (piem--buffer-name :chat dir)))
                (piem-activity-phase-functions
                 (list (lambda (chat-buf input-buf old-phase new-phase reason)
                         (push (list chat-buf input-buf old-phase new-phase reason)
                               calls)))))
            (with-current-buffer chat
              (piem--set-activity-phase "running")
              (setq calls nil)
              (piem--set-input-buffer new-input))
            (pcase-let ((`(,seen-chat ,seen-input ,old-phase ,new-phase ,reason)
                         (cl-find-if (lambda (call)
                                       (eq (cadr call) new-input))
                                     calls)))
              (should (eq seen-chat chat))
              (should (eq seen-input new-input))
              (should (equal old-phase "running"))
              (should (equal new-phase "running"))
              (should (eq reason 'input-link)))))
      (when (buffer-live-p new-input)
        (kill-buffer new-input)))))

(ert-deftest piem-test-set-input-buffer-clears-old-input-activity ()
  "Relinking input buffers lets hooks clean state from the old input."
  (let ((dir "/tmp/piem-test-activity-relink-cleanup/")
        (new-input (generate-new-buffer " *pi-activity-relink-cleanup-input*")))
    (unwind-protect
        (piem-test-with-mock-session dir
          (let ((chat (get-buffer (piem--buffer-name :chat dir)))
                (old-input (get-buffer (piem--buffer-name :input dir)))
                (piem-activity-phase-functions
                 (list (lambda (_chat-buf input-buf _old-phase new-phase reason)
                         (when (buffer-live-p input-buf)
                           (with-current-buffer input-buf
                             (cond
                              ((eq reason 'input-unlink)
                               (setq piem-test--activity-marker nil))
                              ((not (equal new-phase "idle"))
                               (setq piem-test--activity-marker t)))))))))
            (with-current-buffer chat
              (piem--set-activity-phase "running"))
            (with-current-buffer old-input
              (should piem-test--activity-marker))
            (with-current-buffer chat
              (piem--set-input-buffer new-input))
            (with-current-buffer old-input
              (should-not piem-test--activity-marker))
            (with-current-buffer new-input
              (should piem-test--activity-marker))))
      (when (buffer-live-p new-input)
        (kill-buffer new-input)))))

(ert-deftest piem-test-activity-phase-reason-distinguishes-relink-from-idle ()
  "Relinking input buffers does not look like a real idle transition."
  (let ((finished-notifications 0)
        (dir "/tmp/piem-test-activity-relink-reason/")
        (new-input (generate-new-buffer " *pi-activity-relink-reason-input*")))
    (unwind-protect
        (piem-test-with-mock-session dir
          (let ((chat (get-buffer (piem--buffer-name :chat dir)))
                (piem-activity-phase-functions
                 (list (lambda (_chat-buf _input-buf old-phase new-phase reason)
                         (when (and (eq reason 'phase-change)
                                    (not (equal old-phase "idle"))
                                    (equal new-phase "idle"))
                           (setq finished-notifications
                                 (1+ finished-notifications)))))))
            (with-current-buffer chat
              (piem--set-activity-phase "running")
              (piem--set-input-buffer new-input))
            (should (= finished-notifications 0))
            (with-current-buffer chat
              (piem--set-activity-phase "idle"))
            (should (= finished-notifications 1))))
      (when (buffer-live-p new-input)
        (kill-buffer new-input)))))

(ert-deftest piem-test-chat-buffer-kill-forces-teardown-activity-phase ()
  "Killing a chat buffer applies idle with teardown as the reason."
  (let ((calls nil)
        (dir "/tmp/piem-test-activity-teardown/"))
    (piem-test-with-mock-session dir
      (let ((chat (get-buffer (piem--buffer-name :chat dir)))
            (input (get-buffer (piem--buffer-name :input dir)))
            (piem-activity-phase-functions
             (list (lambda (chat-buf input-buf old-phase new-phase reason)
                     (push (list chat-buf input-buf old-phase new-phase reason)
                           calls)))))
        (with-current-buffer chat
          (piem--set-activity-phase "running"))
        (setq calls nil)
        (kill-buffer chat)
        (pcase-let ((`(,seen-chat ,seen-input ,old-phase ,new-phase ,reason)
                     (cl-find-if (lambda (call)
                                   (and (eq (nth 4 call) 'teardown)
                                        (equal (nth 2 call) "running")
                                        (equal (nth 3 call) "idle")))
                                 calls)))
          (should (eq seen-chat chat))
          (should (or (null seen-input)
                      (eq seen-input input)))
          (should (equal old-phase "running"))
          (should (equal new-phase "idle"))
          (should (eq reason 'teardown)))))))

(ert-deftest piem-test-get-process-from-chat ()
  "Can get process from chat buffer."
  (let ((default-directory "/tmp/piem-test-proc1/")
        (fake-proc 'mock-process))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) fake-proc))
              ((symbol-function 'piem--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (piem)
            (with-current-buffer "*piem-chat:/tmp/piem-test-proc1/*"
              (should (eq (piem--get-process) fake-proc))))
        (ignore-errors (kill-buffer "*piem-chat:/tmp/piem-test-proc1/*"))
        (ignore-errors (kill-buffer "*piem-input:/tmp/piem-test-proc1/*"))))))

(ert-deftest piem-test-get-process-from-input ()
  "Can get process from input buffer via chat buffer."
  (let ((default-directory "/tmp/piem-test-proc2/")
        (fake-proc 'mock-process))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) fake-proc))
              ((symbol-function 'piem--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (piem)
            (with-current-buffer "*piem-input:/tmp/piem-test-proc2/*"
              (should (eq (piem--get-process) fake-proc))))
        (ignore-errors (kill-buffer "*piem-chat:/tmp/piem-test-proc2/*"))
        (ignore-errors (kill-buffer "*piem-input:/tmp/piem-test-proc2/*"))))))

(ert-deftest piem-test-display-buffers-uses-current-frame-window-list ()
  "`piem--display-buffers' should query windows in current frame only."
  (let ((root "/tmp/piem-test-display-frame-local/")
        (all-frames-args nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil)))
      (unwind-protect
          (let* ((chat (piem--setup-session root nil))
                 (input (buffer-local-value 'piem--input-buffer chat))
                 (orig-get-buffer-window-list (symbol-function 'get-buffer-window-list)))
            (delete-other-windows)
            (cl-letf (((symbol-function 'get-buffer-window-list)
                       (lambda (buffer minibuf &optional all-frames)
                         (push all-frames all-frames-args)
                         (funcall orig-get-buffer-window-list buffer minibuf all-frames))))
              (piem--display-buffers chat input))
            (should-not (memq t all-frames-args)))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-chat-mode-map-binds-commands ()
  "Chat mode map binds abort, session, context, model, and info chords."
  (dolist (expected '(("C-c C-k" . piem-abort)
                      ("C-c C-n" . piem-new-session)
                      ("C-c C-r" . piem-session-browser)
                      ("C-c C-e" . piem-export-html)
                      ("C-c C-c" . piem-compact)
                      ("C-c C-m" . piem-select-model)
                      ("C-c C-t" . piem-cycle-thinking)
                      ("C-c C-y" . piem-copy-last-message)))
    (should (eq (lookup-key piem-chat-mode-map
                            (kbd (car expected)))
                (cdr expected)))))

(ert-deftest piem-test-display-buffers-soft-dedicates-input-window ()
  "Input window should be soft-dedicated so `display-buffer' skips it."
  (let ((root "/tmp/piem-test-dedicated/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil)))
      (unwind-protect
          (let* ((chat (piem--setup-session root nil))
                 (input (buffer-local-value 'piem--input-buffer chat)))
            (delete-other-windows)
            (piem--display-buffers chat input)
            (should (eq 'side (window-dedicated-p
                               (get-buffer-window input)))))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-display-buffers-chat-only-when-show-input-nil ()
  "SHOW-INPUT nil displays only the chat window."
  (let ((root "/tmp/piem-test-display-chat-only/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil)))
      (unwind-protect
          (let* ((chat (piem--setup-session root nil))
                 (input (buffer-local-value 'piem--input-buffer chat)))
            (delete-other-windows)
            (piem--display-buffers chat input t)
            (should (get-buffer-window chat))
            (should-not (get-buffer-window input)))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-open-input-splits-below-chat ()
  "`piem-open-input' opens a soft-dedicated input window below chat."
  (let ((root "/tmp/piem-test-open-input-split/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil)))
      (unwind-protect
          (let* ((chat (piem--setup-session root nil))
                 (input (buffer-local-value 'piem--input-buffer chat)))
            (delete-other-windows)
            (piem--display-buffers chat input t)
            (select-window (get-buffer-window chat))
            (piem-open-input)
            (let ((input-win (get-buffer-window input)))
              (should input-win)
              (should (eq (selected-window) input-win))
              (should (eq 'side (window-dedicated-p input-win)))
              (should (eq (window-in-direction 'above input-win)
                          (get-buffer-window chat)))))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-open-input-focuses-visible-input ()
  "`piem-open-input' selects an already-visible input window."
  (let ((root "/tmp/piem-test-open-input-focus/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil)))
      (unwind-protect
          (let* ((chat (piem--setup-session root nil))
                 (input (buffer-local-value 'piem--input-buffer chat)))
            (delete-other-windows)
            (piem--display-buffers chat input)
            (select-window (get-buffer-window chat))
            (piem-open-input)
            (should (eq (selected-window) (get-buffer-window input)))
            (should (= 2 (length (window-list nil 'no-mini)))))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-send-hides-input-window-on-demand ()
  "Sending hides the input window when display is `on-demand'."
  (let ((root "/tmp/piem-test-send-hide/")
        (piem-input-window-display 'on-demand))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--prepare-and-send) #'ignore))
      (unwind-protect
          (let* ((chat (piem--setup-session root nil))
                 (input (buffer-local-value 'piem--input-buffer chat)))
            (delete-other-windows)
            (piem--display-buffers chat input)
            (with-current-buffer input
              (insert "Hello, pi!")
              (piem-send))
            (should-not (get-buffer-window input))
            (should (eq (selected-window) (get-buffer-window chat))))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-send-keeps-input-window-when-always ()
  "Sending keeps the input window when display is `always'."
  (let ((root "/tmp/piem-test-send-keep/")
        (piem-input-window-display 'always))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--prepare-and-send) #'ignore))
      (unwind-protect
          (let* ((chat (piem--setup-session root nil))
                 (input (buffer-local-value 'piem--input-buffer chat)))
            (delete-other-windows)
            (piem--display-buffers chat input)
            (with-current-buffer input
              (insert "Hello, pi!")
              (piem-send))
            (should (get-buffer-window input)))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-show-session-buffers-hidden-launches-chat-only ()
  "A fresh session launches chat-only when display is `hidden'.
`piem--show-session-buffers' honors
`piem-input-window-display', so a `hidden' session starts
without an input window."
  (let ((root "/tmp/piem-test-show-hidden/")
        (piem-input-window-display 'hidden))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil)))
      (unwind-protect
          (let* ((chat (piem--setup-session root nil))
                 (input (buffer-local-value 'piem--input-buffer chat)))
            (delete-other-windows)
            (piem--show-session-buffers chat input)
            (should (get-buffer-window chat))
            (should-not (get-buffer-window input)))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-send-hides-input-window-when-hidden ()
  "Sending hides the input window when display is `hidden'."
  (let ((root "/tmp/piem-test-send-hide-hidden/")
        (piem-input-window-display 'hidden))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--prepare-and-send) #'ignore))
      (unwind-protect
          (let* ((chat (piem--setup-session root nil))
                 (input (buffer-local-value 'piem--input-buffer chat)))
            (delete-other-windows)
            (piem--display-buffers chat input)
            (with-current-buffer input
              (insert "Hello, pi!")
              (piem-send))
            (should-not (get-buffer-window input))
            (should (eq (selected-window) (get-buffer-window chat))))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-hide-session-windows-uses-current-frame-window-list ()
  "`piem--hide-session-windows' should query current frame windows only."
  (let ((root "/tmp/piem-test-hide-frame-local/")
        (all-frames-args nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (setq default-directory root)
            (piem)
            (let ((chat (get-buffer (piem-test--chat-buffer-name root)))
                  (orig-get-buffer-window-list (symbol-function 'get-buffer-window-list)))
              (with-current-buffer chat
                (cl-letf (((symbol-function 'get-buffer-window-list)
                           (lambda (buffer minibuf &optional all-frames)
                             (push all-frames all-frames-args)
                             (funcall orig-get-buffer-window-list buffer minibuf all-frames))))
                  (piem--hide-session-windows)))
              (should-not (memq t all-frames-args))))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

;;; Chat Keymap

(ert-deftest piem-test-chat-mode-map-shell-command-at-point ()
  "The chat `!' key runs one file command without changing other actions."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--set-chat-session-identity "/tmp/project/")
    (let ((inhibit-read-only t))
      (insert "src/report.el"))
    (goto-char (+ (point-min) 2))
    (let (prompt command command-directory)
      (cl-letf (((symbol-function 'read-shell-command)
                 (lambda (value)
                   (setq prompt value)
                   "file *"))
                ((symbol-function 'shell-command)
                 (lambda (value)
                   (setq command value
                         command-directory default-directory))))
        (let ((binding (key-binding (kbd "!"))))
          (should (eq binding #'piem-shell-command-at-point))
          (call-interactively binding)))
      (should (equal "! on src/report.el: " prompt))
      (should (equal "file /tmp/project/src/report.el" command))
      (should (equal "/tmp/project/" command-directory)))
    (should (eq (lookup-key piem-chat-mode-map (kbd "RET"))
                #'piem-visit-file))
    (should (eq (lookup-key piem-chat-mode-map
                            [remap push-button])
                #'piem--dispatch-button))
    (dolist (key '("&" "E" "o"))
      (should-not (lookup-key piem-chat-mode-map (kbd key))))))

;;; Startup Header

(ert-deftest piem-test-startup-header-shows-keybindings ()
  "Startup header includes key keybindings."
  (let ((header (piem--format-startup-header)))
    (should (string-match-p "C-c C-c" header))
    (should (string-match-p "send" header))
    (should (string-match-p "C-c C-a   attach image (C-u clears)" header))
    (should (string-match-p "C-c C-r   sessions" header))))

(ert-deftest piem-test-startup-header-shows-label ()
  "Startup header labels the buffer with the package name."
  (let ((header (piem--format-startup-header)))
    (should (string-match-p "^piem$" header))))

(ert-deftest piem-test-extract-pi-version-from-clean-output ()
  "Extract the plain semantic version returned by pi."
  (should (equal (piem--extract-pi-version "0.79.1\n")
                 "0.79.1")))

(ert-deftest piem-test-extract-pi-version-from-stderr-style-output ()
  "Ignore npm warnings and extract the standalone pi version line."
  (should (equal (piem--extract-pi-version
                  "npm warn deprecated package@1.0.0: old\n0.79.1\n")
                 "0.79.1")))

(ert-deftest piem-test-extract-pi-version-returns-nil-for-unparseable-output ()
  "Unparseable version output should be harmless."
  (should-not (piem--extract-pi-version "npm warn only\n")))

(ert-deftest piem-test-pi-version-outdated-compares-segments-numerically ()
  "Compare pi versions numerically, not lexically."
  (should (piem--pi-version-outdated-p "0.79.0"))
  (should (piem--pi-version-outdated-p "0.80.99"))
  (should (piem--pi-version-outdated-p "0.84.1"))
  (should-not (piem--pi-version-outdated-p "0.84.2"))
  (should-not (piem--pi-version-outdated-p "0.84.3"))
  (should-not (piem--pi-version-outdated-p "1.0.0")))

(ert-deftest piem-test-finish-pi-version-process-parses-stderr ()
  "Version probing should accept pi versions printed to stderr."
  (let ((proc (start-process "piem-test-version" nil "cat"))
        (stdout-buf (generate-new-buffer " *pi-test-version-stdout*"))
        (stderr-buf (generate-new-buffer " *pi-test-version-stderr*"))
        (resolved-version nil))
    (unwind-protect
        (progn
          (with-current-buffer stderr-buf
            (insert "npm warn deprecated package@1.0.0: old\n0.79.1\n"))
          (process-put proc 'piem-version-callback
                       (lambda (version)
                         (setq resolved-version version)))
          (process-put proc 'piem-version-stdout-buf stdout-buf)
          (process-put proc 'piem-version-stderr-buf stderr-buf)
          (piem--finish-pi-version-process proc)
          (should (equal resolved-version "0.79.1"))
          (should-not (buffer-live-p stdout-buf))
          (should-not (buffer-live-p stderr-buf)))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p stdout-buf)
        (kill-buffer stdout-buf))
      (when (buffer-live-p stderr-buf)
        (kill-buffer stderr-buf)))))

(ert-deftest piem-test-request-pi-version-async-waits-before-probe ()
  "Version lookup waits briefly before starting the probe process."
  (let ((scheduled-delay nil)
        (scheduled-directory nil)
        (resolved-version nil))
    (cl-letf (((symbol-function 'piem--run-pi-version-once-async)
               (lambda (callback &optional directory)
                 (setq scheduled-directory directory)
                 (funcall callback "0.79.1")))
              ((symbol-function 'run-at-time)
               (lambda (secs _repeat fn &rest args)
                 (setq scheduled-delay secs)
                 (apply fn args)
                 'mock-timer)))
      (let ((default-directory "/ssh:pi-host:/home/pi/project/"))
        (piem--request-pi-version-async
         (lambda (version)
           (setq resolved-version version)))))
    (should (= scheduled-delay piem--version-probe-delay))
    (should (equal scheduled-directory "/ssh:pi-host:/home/pi/project/"))
    (should (equal resolved-version "0.79.1"))))

(ert-deftest piem-test-run-pi-version-uses-default-directory-file-handler ()
  "Version probes let `default-directory' file handlers create the process."
  (let ((piem-executable '("pi"))
        (captured nil)
        (captured-default-directory nil)
        (dummy-proc (start-process "piem-test-version-capture" nil "cat")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured args
                             captured-default-directory default-directory)
                       dummy-proc)))
            (piem--run-pi-version-once-async
             #'ignore "/ssh:pi-host:/home/pi/project/"))
          (should (eq (plist-get captured :file-handler) t))
          (should (equal captured-default-directory
                         "/ssh:pi-host:/home/pi/project/"))
          (should (bufferp (plist-get captured :buffer)))
          (should (bufferp (plist-get captured :stderr))))
      (when-let* ((stdout-buf (plist-get captured :buffer)))
        (when (buffer-live-p stdout-buf)
          (kill-buffer stdout-buf)))
      (when-let* ((stderr-buf (plist-get captured :stderr)))
        (when (buffer-live-p stderr-buf)
          (kill-buffer stderr-buf)))
      (when (process-live-p dummy-proc)
        (delete-process dummy-proc)))))

(ert-deftest piem-test-probe-process-version-uses-chat-session-directory ()
  "Version probing uses the stable chat session directory."
  (let ((captured-default-directory nil))
    (with-temp-buffer
      (piem-chat-mode)
      (setq default-directory "/tmp/transcript/"
            piem--canonical-session-directory
            "/ssh:pi-host:/home/pi/project/")
      (cl-letf (((symbol-function 'piem--request-pi-version-async)
                 (lambda (_callback)
                   (setq captured-default-directory default-directory))))
        (piem--probe-process-version-async (current-buffer)))
      (should (equal captured-default-directory
                     "/ssh:pi-host:/home/pi/project/")))))

(ert-deftest piem-test-process-replacement-invalidates-model-change ()
  "A model callback cannot mutate state after its target process is replaced."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--process 'old-process)
    (let ((token (piem--begin-model-change
                  'old-process (current-buffer))))
      (should (piem--model-change-current-p token))
      (piem--set-process 'new-process)
      (should-not (piem--model-change-current-p token))
      (should-not (piem--model-change-pending-p)))))

(ert-deftest piem-test-set-process-probes-version-for-current-process ()
  "Setting process starts version probe and stores result for current process."
  (let ((callback nil)
        (messages nil)
        (noninteractive nil)
        (proc (start-process "piem-test-proc" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (cl-letf (((symbol-function 'piem--request-pi-version-async)
                     (lambda (cb)
                       (setq callback cb)
                       nil))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (piem--set-process proc)
            (should callback)
            (funcall callback "0.79.1")
            (should (equal piem--process-version "0.79.1"))
            (should (equal (car messages) "Pi: version 0.79.1"))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest piem-test-set-process-version-callback-uses-chat-buffer-context ()
  "Version callback updates chat buffer even when current buffer changed."
  (let ((callback nil)
        (messages nil)
        (noninteractive nil)
        (proc (start-process "piem-test-proc-a" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (let ((chat-buf (current-buffer)))
            (cl-letf (((symbol-function 'piem--request-pi-version-async)
                       (lambda (cb)
                         (setq callback cb)
                         nil))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (piem--set-process proc)
              (with-temp-buffer
                (funcall callback "0.79.1"))
              (with-current-buffer chat-buf
                (should (equal piem--process-version "0.79.1")))
              (should (equal (car messages) "Pi: version 0.79.1")))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest piem-test-probe-process-version-warns-when-pi-too-old ()
  "Version probe warns clearly for unsupported pi versions."
  (let ((callback nil)
        (warning-text nil)
        (noninteractive nil)
        (proc (start-process "piem-test-proc-old" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (cl-letf (((symbol-function 'piem--request-pi-version-async)
                     (lambda (cb)
                       (setq callback cb)
                       nil))
                    ((symbol-function 'message) #'ignore)
                    ((symbol-function 'display-warning)
                     (lambda (_type message &rest _)
                       (setq warning-text message))))
            (piem--set-process proc)
            (should callback)
            (funcall callback "0.79.0")
            (should (equal piem--process-version "0.79.0"))
            (should (string-match-p "0.79.0" warning-text))
            (should (string-match-p "0.84.2" warning-text))
            (should (string-match-p
                     "npm install -g @earendil-works/pi-coding-agent"
                     warning-text))
            (should-not (string-match-p
                         "npm install -g @earendil-works/pi-coding-agent@"
                         warning-text))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest piem-test-probe-process-version-does-not-warn-when-supported ()
  "Version probe accepts the minimum supported pi version."
  (let ((callback nil)
        (warning-called nil)
        (noninteractive nil)
        (proc (start-process "piem-test-proc-supported" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (cl-letf (((symbol-function 'piem--request-pi-version-async)
                     (lambda (cb)
                       (setq callback cb)
                       nil))
                    ((symbol-function 'message) #'ignore)
                    ((symbol-function 'display-warning)
                     (lambda (&rest _)
                       (setq warning-called t))))
            (piem--set-process proc)
            (should callback)
            (funcall callback "0.84.2")
            (should (equal piem--process-version "0.84.2"))
            (should-not warning-called)))
      (when (process-live-p proc)
        (delete-process proc)))))

;;; Copy Visible Text

(defmacro piem-test--with-chat-markup (markdown &rest body)
  "Insert MARKDOWN into a chat-mode buffer, fontify, then run BODY.
Buffer is read-only with `inhibit-read-only' used for insertion.
`font-lock-ensure' runs before BODY to apply invisible/display properties."
  (declare (indent 1) (debug (stringp body)))
  `(with-temp-buffer
     (piem-chat-mode)
     (let ((inhibit-read-only t))
       (insert ,markdown))
     (font-lock-ensure)
     ,@body))

(ert-deftest piem-test-visible-text-strips-bold-markers ()
  "visible-text strips invisible bold markers (**)."
  (piem-test--with-chat-markup "Hello **bold** world"
    (should (equal (piem--visible-text (point-min) (point-max))
                   "Hello bold world"))))

(ert-deftest piem-test-visible-text-strips-inline-code-backticks ()
  "visible-text strips invisible backticks around inline code."
  (piem-test--with-chat-markup "Use `foo` here"
    (should (equal (piem--visible-text (point-min) (point-max))
                   "Use foo here"))))

(ert-deftest piem-test-visible-text-strips-code-fences ()
  "visible-text strips invisible code fences and language label."
  (piem-test--with-chat-markup "```python\ndef foo():\n    pass\n```\n"
    (let ((result (piem--visible-text (point-min) (point-max))))
      (should (string-match-p "def foo" result))
      (should-not (string-match-p "```" result))
      (should-not (string-match-p "python" result)))))

(ert-deftest piem-test-visible-text-strips-setext-underline ()
  "visible-text strips setext underlines (hidden by md-ts-hide-markup)."
  (piem-test--with-chat-markup "Assistant\n=========\n\nHello\n"
    (let ((result (piem--visible-text (point-min) (point-max))))
      (should (string-match-p "Assistant" result))
      (should-not (string-match-p "=====" result))
      (should (string-match-p "Hello" result)))))

(ert-deftest piem-test-visible-text-strips-atx-heading-prefix ()
  "visible-text strips invisible ATX heading prefix characters."
  (piem-test--with-chat-markup "## Code Example\n\nSome text\n"
    (let ((result (piem--visible-text (point-min) (point-max))))
      (should (string-match-p "Code Example" result))
      (should (string-match-p "Some text" result))
      (should-not (string-match-p "^##" result)))))

(ert-deftest piem-test-visible-text-preserves-plain-text ()
  "visible-text preserves text that has no hidden markup."
  (piem-test--with-chat-markup "Just plain text with no markup"
    (should (equal (piem--visible-text (point-min) (point-max))
                   "Just plain text with no markup"))))

(ert-deftest piem-test-visible-text-position-map-preserves-source-envelope ()
  "Visible character indices map exactly across omitted property spans."
  (with-temp-buffer
    (insert "aXXbcYYd")
    (put-text-property 2 4 'invisible 'md-ts--markup)
    (put-text-property 6 8 'invisible 'md-ts--markup)
    (let ((at-boundary
           (piem--visible-text-with-position-map 1 9 6))
          (inside-hidden
           (piem--visible-text-with-position-map 1 9 7)))
      (should (equal "abcd" (plist-get at-boundary :text)))
      (should (equal [1 4 5 8] (plist-get at-boundary :positions)))
      (should (= 3 (plist-get at-boundary :index)))
      (should (= 3 (plist-get inside-hidden :index)))
      ;; Visible [1,3) is "bc" and maps to the real half-open envelope 4..6.
      (let ((positions (plist-get at-boundary :positions)))
        (should (equal (cons (aref positions 1)
                             (1+ (aref positions 2)))
                       '(4 . 6)))))))

(ert-deftest piem-test-copy-raw-markdown-defcustom-default ()
  "piem-copy-raw-markdown defcustom defaults to nil."
  (should (eq piem-copy-raw-markdown nil)))

(ert-deftest piem-test-project-trust-policy-default ()
  "Project trust policy defaults to approving project-local Pi inputs."
  (should (eq piem-project-trust-policy 'approve)))

(ert-deftest piem-test-hot-tail-turn-count-defcustom-defaults ()
  "Hot-tail turn count defaults to 3 headed turns."
  (should (= 3 piem-hot-tail-turn-count)))

(ert-deftest piem-test-extension-status-properties-apply-in-header-line ()
  "Extension status properties are applied by status key in the header line."
  (let ((piem-extension-status-faces
         '(("sub-status:usage" . (:foreground "#c6a0f6")))))
    (with-temp-buffer
      (piem-chat-mode)
      (setq piem--state '(:model "claude-sonnet-4")
            piem--extension-status
            '(("solveit-mode" . "⚡ concise")
              ("sub-status:usage" . "4h51m 1% · 9h9m 41%")))
      (let ((header (piem--header-line-string)))
        (should (string-match-p "⚡ concise · 4h51m 1%% · 9h9m 41%%"
                                (substring-no-properties header)))
        (should-not (get-text-property (string-match-p "⚡" header) 'face header))
        (should (equal (get-text-property (string-match-p "⚡" header)
                                          'help-echo header)
                       "solveit-mode"))
        (should (eq (get-text-property (string-match-p "⚡" header)
                                       'mouse-face header)
                    'highlight))
        (should (equal (get-text-property (string-match-p "4h" header) 'face header)
                       '(:foreground "#c6a0f6")))
        (should (equal (get-text-property (string-match-p "4h" header)
                                          'help-echo header)
                       "sub-status:usage"))
        (should (eq (get-text-property (string-match-p "4h" header)
                                       'mouse-face header)
                    'highlight))))))

(ert-deftest piem-test-kill-ring-save-strips-by-default ()
  "kill-ring-save strips hidden markup by default."
  (piem-test--with-chat-markup "Hello **bold** world"
    (kill-ring-save (point-min) (point-max))
    (should (equal (car kill-ring) "Hello bold world"))))

(ert-deftest piem-test-kill-ring-save-keeps-raw-when-enabled ()
  "When copy-raw-markdown is t, kill-ring-save keeps raw markdown."
  (piem-test--with-chat-markup "Hello **bold** world"
    (let ((piem-copy-raw-markdown t))
      (kill-ring-save (point-min) (point-max))
      (should (equal (car kill-ring) "Hello **bold** world")))))

;;; Chat Navigation Behavior

(ert-deftest piem-test-next-message-from-top ()
  "n from point-min reaches first You heading."
  (with-temp-buffer
    (piem-test--insert-chat-turns)
    (goto-char (point-min))
    (piem-next-message)
    (should (looking-at "You · 10:00"))))

(ert-deftest piem-test-next-message-successive ()
  "Successive n reaches each You heading in order."
  (with-temp-buffer
    (piem-test--insert-chat-turns)
    (goto-char (point-min))
    (piem-next-message)
    (should (looking-at "You · 10:00"))
    (piem-next-message)
    (should (looking-at "You · 10:05"))
    (piem-next-message)
    (should (looking-at "You · 10:10"))))

(ert-deftest piem-test-next-message-recognizes-full-date-heading ()
  "Message navigation recognizes full-date You headings."
  (with-temp-buffer
    (insert "Intro\n\nYou · 2026-06-13 10:05\n========================\nQuestion\n")
    (goto-char (point-min))
    (piem-next-message)
    (should (looking-at "You · 2026-06-13 10:05"))))

(ert-deftest piem-test-next-message-at-last ()
  "n at last You heading keeps point and shows message."
  (with-temp-buffer
    (piem-test--insert-chat-turns)
    (goto-char (point-min))
    (piem-next-message)
    (piem-next-message)
    (piem-next-message)
    (should (looking-at "You · 10:10"))
    (let ((pos (point))
          (shown-message nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (piem-next-message))
      ;; Point stays on the last heading
      (should (= (point) pos))
      (should (equal shown-message "No more messages")))))

(ert-deftest piem-test-previous-message-from-last ()
  "p from last You heading reaches previous."
  (with-temp-buffer
    (piem-test--insert-chat-turns)
    (goto-char (point-min))
    ;; Navigate to last heading first
    (piem-next-message)
    (piem-next-message)
    (piem-next-message)
    (should (looking-at "You · 10:10"))
    (piem-previous-message)
    (should (looking-at "You · 10:05"))))

(ert-deftest piem-test-previous-message-at-first ()
  "p at first You heading keeps point and shows message."
  (with-temp-buffer
    (piem-test--insert-chat-turns)
    (goto-char (point-min))
    (piem-next-message)
    (should (looking-at "You · 10:00"))
    (let ((pos (point))
          (shown-message nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (piem-previous-message))
      ;; Point stays on the first heading
      (should (= (point) pos))
      (should (equal shown-message "No previous message")))))

(ert-deftest piem-test-other-window-scroll-buffer-set-locally ()
  "Session setup stores `other-window-scroll-buffer' as input-local state."
  (let ((root "/tmp/piem-test-scroll-other/")
        (original-default (default-value 'other-window-scroll-buffer)))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (let ((default-directory root))
              (piem))
            (let ((chat (get-buffer (piem-test--chat-buffer-name root)))
                  (input (get-buffer (piem-test--input-buffer-name root))))
              (with-current-buffer input
                (should (local-variable-p 'other-window-scroll-buffer))
                (should (eq other-window-scroll-buffer chat)))
              (should (eq (default-value 'other-window-scroll-buffer)
                          original-default))))
        (set-default 'other-window-scroll-buffer original-default)
        (piem-test--kill-session-buffers root)))))

(ert-deftest piem-test-other-window-for-scrolling-tracks-each-input-session ()
  "Each input buffer scrolls its own chat buffer."
  (let ((root-a "/tmp/piem-test-scroll-a/")
        (root-b "/tmp/piem-test-scroll-b/")
        (original-default (default-value 'other-window-scroll-buffer)))
    (make-directory root-a t)
    (make-directory root-b t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (let ((default-directory root-a))
              (piem))
            (let ((default-directory root-b))
              (piem))
            (let* ((chat-a (get-buffer (piem-test--chat-buffer-name root-a)))
                   (input-a (get-buffer (piem-test--input-buffer-name root-a)))
                   (chat-b (get-buffer (piem-test--chat-buffer-name root-b)))
                   (input-b (get-buffer (piem-test--input-buffer-name root-b))))
              (delete-other-windows)
              (switch-to-buffer chat-a)
              (let* ((chat-win-a (selected-window))
                     (input-win-a (split-window chat-win-a -10 'below))
                     (chat-win-b (split-window chat-win-a nil 'right))
                     input-win-b)
                (set-window-buffer input-win-a input-a)
                (set-window-buffer chat-win-b chat-b)
                (setq input-win-b (split-window chat-win-b -10 'below))
                (set-window-buffer input-win-b input-b)
                (select-window input-win-a)
                (should (eq (window-buffer (other-window-for-scrolling)) chat-a))
                (select-window input-win-b)
                (should (eq (window-buffer (other-window-for-scrolling)) chat-b)))))
        (set-default 'other-window-scroll-buffer original-default)
        (piem-test--kill-session-buffers root-a)
        (piem-test--kill-session-buffers root-b)))))

;;; Turn Detection

(ert-deftest piem-test-turn-index-on-first-heading ()
  "Turn index is 0 when point is on first You heading."
  (with-temp-buffer
    (piem-test--insert-chat-turns)
    (goto-char (point-min))
    (piem-next-message)
    (should (= (piem--user-turn-index-at-point) 0))))

(ert-deftest piem-test-turn-index-in-first-body ()
  "Turn index is 0 when point is in first user message body."
  (with-temp-buffer
    (piem-test--insert-chat-turns)
    (goto-char (point-min))
    (piem-next-message)
    (forward-line 2) ; skip heading + underline into body
    (should (= (piem--user-turn-index-at-point) 0))))

(ert-deftest piem-test-turn-index-on-underline ()
  "Turn index is 0 when point is on === underline of first You."
  (with-temp-buffer
    (piem-test--insert-chat-turns)
    (goto-char (point-min))
    (piem-next-message)
    (forward-line 1) ; on ===
    (should (= (piem--user-turn-index-at-point) 0))))

(ert-deftest piem-test-turn-index-on-second-heading ()
  "Turn index is 1 on second You heading."
  (with-temp-buffer
    (piem-test--insert-chat-turns)
    (goto-char (point-min))
    (piem-next-message)
    (piem-next-message)
    (should (= (piem--user-turn-index-at-point) 1))))

(ert-deftest piem-test-turn-index-on-assistant-heading ()
  "Turn index is index of preceding You when point is on Assistant heading."
  (with-temp-buffer
    (piem-test--insert-chat-turns)
    (goto-char (point-min))
    ;; Navigate to first You, then move into assistant section
    (piem-next-message)
    (forward-line 4) ; past heading + underline + body + blank → "Assistant"
    (should (looking-at "Assistant"))
    (should (= (piem--user-turn-index-at-point) 0))))

(ert-deftest piem-test-turn-index-in-assistant-body ()
  "Turn index is index of preceding You when point is in assistant response."
  (with-temp-buffer
    (piem-test--insert-chat-turns)
    (goto-char (point-min))
    (piem-next-message)
    (forward-line 6) ; heading + underline + body + blank + Assistant + underline → response
    (should (looking-at "First answer"))
    (should (= (piem--user-turn-index-at-point) 0))))

(ert-deftest piem-test-turn-index-before-first-you ()
  "Turn index is nil before first You heading."
  (with-temp-buffer
    (piem-test--insert-chat-turns)
    (goto-char (point-min))
    (should-not (piem--user-turn-index-at-point))))

(ert-deftest piem-test-turn-index-empty-buffer ()
  "Turn index is nil in empty buffer."
  (with-temp-buffer
    (should-not (piem--user-turn-index-at-point))))

(ert-deftest piem-test-turn-index-no-false-match ()
  "Turn index ignores text starting with You without setext underline."
  (with-temp-buffer
    (insert "You mentioned something\nRegular text\n\n"
            "You · 10:00\n===========\nFirst question\n")
    (goto-char (point-min))
    ;; Point is on "You mentioned" which has no === underline
    (should-not (piem--user-turn-index-at-point))
    ;; Move to the real heading
    (goto-char (point-max))
    (should (= (piem--user-turn-index-at-point) 0))))

;;; You Heading Detection

(ert-deftest piem-test-heading-re-matches-plain-you ()
  "Heading regex matches bare `You' at start of line."
  (should (string-match-p piem--you-heading-re "You")))

(ert-deftest piem-test-heading-re-matches-you-with-timestamp ()
  "Heading regex matches `You · 22:10' at start of line."
  (should (string-match-p piem--you-heading-re "You · 22:10")))

(ert-deftest piem-test-heading-re-rejects-you-colon ()
  "Heading regex does not match `You:' (old broken pattern)."
  (should-not (string-match-p piem--you-heading-re "You: hello")))

(ert-deftest piem-test-heading-re-rejects-mid-line ()
  "Heading regex does not match `You' mid-line."
  (should-not (string-match-p piem--you-heading-re "  You · 22:10")))

(ert-deftest piem-test-heading-re-rejects-you-prefix ()
  "Heading regex does not match words starting with You like `Your'."
  (should-not (string-match-p piem--you-heading-re "Your code is fine")))

(ert-deftest piem-test-at-you-heading-p-true ()
  "Predicate returns t when on a valid You setext heading."
  (with-temp-buffer
    (insert "You · 22:10\n===========\n")
    (goto-char (point-min))
    (should (piem--at-you-heading-p))))

(ert-deftest piem-test-at-you-heading-p-no-underline ()
  "Predicate returns nil when You line lacks setext underline."
  (with-temp-buffer
    (insert "You · 22:10\nSome text\n")
    (goto-char (point-min))
    (should-not (piem--at-you-heading-p))))

(ert-deftest piem-test-at-you-heading-p-short-underline ()
  "Predicate returns t with minimum 3-char underline."
  (with-temp-buffer
    (insert "You\n===\n")
    (goto-char (point-min))
    (should (piem--at-you-heading-p))))

(ert-deftest piem-test-at-you-heading-p-wrong-line ()
  "Predicate returns nil when not on the heading line."
  (with-temp-buffer
    (insert "You · 22:10\n===========\nBody text\n")
    (goto-char (point-max))
    (forward-line -1)  ; on "Body text"
    (should-not (piem--at-you-heading-p))))

;;; Hot Tail

(ert-deftest piem-test-hot-tail-boundary-keeps-buffer-hot-when-few-turns ()
  "Buffers with at most N headed turns stay entirely hot."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((inhibit-read-only t))
      (insert "You · 10:00\n===========\nFirst question\n\n"
              "Assistant\n=========\nFirst answer\n\n"
              "You · 10:05\n===========\nSecond question\n"))
    (let ((piem-hot-tail-turn-count 3))
      (piem--update-hot-tail-boundary)
      (should (= (marker-position piem--hot-tail-start)
                 (point-min))))))

(ert-deftest piem-test-hot-tail-boundary-moves-to-nth-newest-heading ()
  "Hot tail starts at the Nth newest headed turn boundary."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((inhibit-read-only t))
      (insert "You · 10:00\n===========\nFirst question\n\n"
              "Assistant\n=========\nFirst answer\n\n"
              "You · 10:05\n===========\nSecond question\n\n"
              "Assistant\n=========\nSecond answer\n\n"
              "You · 10:10\n===========\nThird question\n"))
    (let ((piem-hot-tail-turn-count 3))
      (piem--update-hot-tail-boundary)
      (goto-char (marker-position piem--hot-tail-start))
      (should (looking-at "You · 10:05")))))

(ert-deftest piem-test-in-hot-tail-p-respects-boundary ()
  "Positions before the hot-tail marker are cold; marker and later are hot."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((inhibit-read-only t))
      (insert "You · 10:00\n===========\nFirst question\n\n"
              "Assistant\n=========\nFirst answer\n\n"
              "You · 10:05\n===========\nSecond question\n\n"
              "Assistant\n=========\nSecond answer\n\n"
              "You · 10:10\n===========\nThird question\n"))
    (let ((piem-hot-tail-turn-count 3))
      (piem--update-hot-tail-boundary)
      (should-not (piem--in-hot-tail-p (point-min)))
      (should (piem--in-hot-tail-p
               (marker-position piem--hot-tail-start))))))

;;; Executable Customization

(ert-deftest piem-test-check-pi-uses-executable ()
  "check-pi uses car of `piem-executable' for lookup."
  (let ((piem-executable '("npx" "pi"))
        looked-up-command
        remote-flag)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (cmd &optional remote)
                 (setq looked-up-command cmd
                       remote-flag remote)
                 (when (equal cmd "npx")
                   "/usr/bin/npx"))))
      (should (piem--check-pi))
      (should (equal looked-up-command "npx"))
      (should (eq remote-flag t)))))

(ert-deftest piem-test-check-pi-returns-nil-when-missing ()
  "check-pi returns nil when executable is not found."
  (let ((piem-executable '("nonexistent-binary")))
    (cl-letf (((symbol-function 'executable-find) (lambda (_cmd &optional _remote) nil)))
      (should-not (piem--check-pi)))))

(ert-deftest piem-test-check-pi-uses-remote-executable-find ()
  "Remote sessions should look for pi on the remote host."
  (let ((piem-executable '("pi"))
        (default-directory "/ssh:pi-host:/home/pi/project/")
        (calls nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (cmd &optional remote)
                 (push (list cmd remote default-directory) calls)
                 (and remote "/ssh:pi-host:/usr/bin/pi"))))
      (should (piem--check-pi))
      (should (member '("pi" t "/ssh:pi-host:/home/pi/project/") calls))
      (should-not (cl-find-if (lambda (call)
                                (and (equal (car call) "pi")
                                     (null (cadr call))))
                              calls)))))

(ert-deftest piem-test-check-pi-multi-hop-uses-full-prefix-candidates ()
  "Multi-hop remote dependency lookup builds candidates with the full route."
  (let ((piem-executable '("pi"))
        (exec-path '("/usr/local/bin" "/usr/bin" nil))
        (exec-suffixes '(""))
        (default-directory "/ssh:bastion|sudo:root@pi-host:/srv/project/")
        (checked nil))
    (cl-letf (((symbol-function 'exec-path)
               (lambda () exec-path))
              ((symbol-function 'executable-find)
               (lambda (&rest _)
                 (ert-fail "multi-hop lookup should not call executable-find")))
              ((symbol-function 'file-executable-p)
               (lambda (path)
                 (push path checked)
                 (equal path
                        "/ssh:bastion|sudo:root@pi-host:/usr/bin/pi"))))
      (should (piem--check-pi))
      (should (member "/ssh:bastion|sudo:root@pi-host:/usr/local/bin/pi"
                      checked))
      (should (member "/ssh:bastion|sudo:root@pi-host:/usr/bin/pi"
                      checked))
      (should-not (cl-some (lambda (path)
                             (string-prefix-p "/sudo:root@pi-host:" path))
                           checked)))))

(ert-deftest piem-test-check-pi-multi-hop-uses-remote-exec-path ()
  "Multi-hop dependency lookup asks TRAMP for the remote PATH entries."
  (let ((piem-executable '("pi"))
        (exec-path '("/local-only/bin"))
        (exec-suffixes '(""))
        (remote-dir "/ssh:bastion|sudo:root@pi-host:/srv/project/")
        (checked nil))
    (cl-letf (((symbol-function 'exec-path)
               (lambda ()
                 (should (equal default-directory remote-dir))
                 '("/opt/remote/bin")))
              ((symbol-function 'executable-find)
               (lambda (&rest _)
                 (ert-fail "multi-hop lookup should not call executable-find")))
              ((symbol-function 'file-executable-p)
               (lambda (path)
                 (push path checked)
                 (equal path
                        "/ssh:bastion|sudo:root@pi-host:/opt/remote/bin/pi"))))
      (let ((default-directory remote-dir))
        (should (piem--check-pi)))
      (should (equal checked
                     '("/ssh:bastion|sudo:root@pi-host:/opt/remote/bin/pi"))))))

(ert-deftest piem-test-check-dependencies-no-local-warning-for-remote-pi ()
  "Remote dependency checks should not warn just because local PATH lacks pi."
  (let ((piem-executable '("pi"))
        (default-directory "/ssh:pi-host:/home/pi/project/")
        (warning-text nil)
        (remote-lookup nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_cmd &optional remote)
                 (setq remote-lookup remote)
                 (and remote "/ssh:pi-host:/usr/bin/pi")))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _)
                 (setq warning-text msg)))
              ((symbol-function 'piem--maybe-install-essential-grammars)
               #'ignore)
              ((symbol-function 'piem--maybe-warn-incompatible-markdown-grammar)
               #'ignore)
              ((symbol-function 'piem--maybe-install-optional-grammars)
               #'ignore))
      (piem--check-dependencies)
      (should remote-lookup)
      (should-not warning-text))))

(ert-deftest piem-test-check-dependencies-warning-names-remote-path ()
  "Remote missing-pi warnings explain that the remote PATH was checked."
  (let ((piem-executable '("pi"))
        (warning-text nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_cmd &optional _remote) nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg)))
              ((symbol-function 'piem--maybe-install-essential-grammars)
               #'ignore)
              ((symbol-function 'piem--maybe-warn-incompatible-markdown-grammar)
               #'ignore)
              ((symbol-function 'piem--maybe-install-optional-grammars)
               #'ignore))
      (piem--check-dependencies "/ssh:pi-host:/home/pi/project/")
      (should (string-match-p "remote PATH (/ssh:pi-host:)" warning-text))
      (should (string-match-p "npm install -g @earendil-works/pi-coding-agent" warning-text))
      (should-not (string-match-p "@earendil-works/pi-coding-agent@" warning-text)))))

(ert-deftest piem-test-check-dependencies-warning-names-multi-hop-remote-path ()
  "Remote missing-pi warnings preserve the full TRAMP route."
  (let ((piem-executable '("pi"))
        (exec-path '("/usr/bin"))
        (warning-text nil))
    (cl-letf (((symbol-function 'exec-path)
               (lambda () exec-path))
              ((symbol-function 'piem--remote-executable-file-p)
               (lambda (_path) nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg)))
              ((symbol-function 'piem--maybe-install-essential-grammars)
               #'ignore)
              ((symbol-function 'piem--maybe-warn-incompatible-markdown-grammar)
               #'ignore)
              ((symbol-function 'piem--maybe-install-optional-grammars)
               #'ignore))
      (piem--check-dependencies
       "/ssh:bastion|sudo:root@pi-host:/srv/project/")
      (should (string-match-p
               (regexp-quote "remote PATH (/ssh:bastion|sudo:root@pi-host:)")
               warning-text)))))

(ert-deftest piem-test-check-dependencies-uses-explicit-directory ()
  "An explicit dependency directory overrides the caller buffer context."
  (let ((checked-directory nil))
    (cl-letf (((symbol-function 'piem--check-pi)
               (lambda (&optional directory)
                 (setq checked-directory directory)
                 t))
              ((symbol-function 'piem--maybe-install-essential-grammars)
               #'ignore)
              ((symbol-function 'piem--maybe-warn-incompatible-markdown-grammar)
               #'ignore)
              ((symbol-function 'piem--maybe-install-optional-grammars)
               #'ignore))
      (piem--check-dependencies "/ssh:pi-host:/home/pi/project/")
      (should (equal checked-directory "/ssh:pi-host:/home/pi/project/")))))

(ert-deftest piem-test-executable-default-value ()
  "Default value of piem-executable is (\"pi\")."
  (should (equal (default-value 'piem-executable) '("pi"))))

(ert-deftest piem-test-check-dependencies-names-executable ()
  "Warning message includes the actual executable name."
  (let ((piem-executable '("my-custom-pi"))
        (warning-text nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_cmd &optional _remote) nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg))))
      (piem--check-dependencies)
      (should (string-match-p "my-custom-pi" warning-text))
      (should (string-match-p
               "npm install -g @earendil-works/pi-coding-agent"
               warning-text))
      (should-not (string-match-p
                   "npm install -g @earendil-works/pi-coding-agent@"
                   warning-text)))))

;;; Essential Grammar Install Prompt (markdown + markdown-inline)

(ert-deftest piem-test-essential-grammars-ignore-optional-gaps ()
  "Only Markdown grammars should count as essential for chat rendering."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (lang &rest _)
               (memq lang '(markdown markdown-inline)))))
    (should-not (piem--missing-essential-grammars))))

(ert-deftest piem-test-missing-essential-grammars-detected ()
  "Detect when markdown or markdown-inline grammars are missing."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (lang &rest _)
               (not (memq lang '(markdown markdown-inline))))))
    (should (equal '(markdown markdown-inline)
                   (piem--missing-essential-grammars)))))

(ert-deftest piem-test-no-missing-essential-grammars ()
  "Return nil when both essential grammars are installed."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_lang &rest _) t)))
    (should-not (piem--missing-essential-grammars))))

(ert-deftest piem-test-essential-grammars-auto-install ()
  "Auto-install essential grammars without prompting when action is `auto'."
  (let ((installed-langs nil)
        (noninteractive nil)
        (piem-essential-grammar-action 'auto))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (not (memq lang '(markdown markdown-inline)))))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (lang &optional _out-dir)
                 (push lang installed-langs)))
              ((symbol-function 'message) #'ignore))
      (piem--maybe-install-essential-grammars)
      (should (memq 'markdown installed-langs))
      (should (memq 'markdown-inline installed-langs)))))

(ert-deftest piem-test-essential-grammars-prompt-accept ()
  "Install essential grammars when action is `prompt' and user accepts."
  (let ((installed-langs nil)
        (noninteractive nil)
        (piem-essential-grammar-action 'prompt))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (not (memq lang '(markdown markdown-inline)))))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (lang &optional _out-dir)
                 (push lang installed-langs)))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'message) #'ignore))
      (piem--maybe-install-essential-grammars)
      (should (memq 'markdown installed-langs))
      (should (memq 'markdown-inline installed-langs)))))

(ert-deftest piem-test-essential-grammars-prompt-decline ()
  "Warn without installing when action is `prompt' and user declines."
  (let ((installed nil)
        (warning-message nil)
        (noninteractive nil)
        (piem-essential-grammar-action 'prompt))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (not (memq lang '(markdown markdown-inline)))))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir)
                 (setq installed t)))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-message msg)))
              ((symbol-function 'message) #'ignore))
      (piem--maybe-install-essential-grammars)
      (should-not installed)
      (should (stringp warning-message))
      (should (string-match-p "not installed" warning-message)))))

(ert-deftest piem-test-essential-grammars-warn-only ()
  "Only warn when action is `warn' — never attempt installation."
  (let ((installed nil)
        (warning-message nil)
        (noninteractive nil)
        (piem-essential-grammar-action 'warn))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (not (memq lang '(markdown markdown-inline)))))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir)
                 (setq installed t)))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-message msg)))
              ((symbol-function 'message) #'ignore))
      (piem--maybe-install-essential-grammars)
      (should-not installed)
      (should (stringp warning-message))
      (should (string-match-p "not installed" warning-message)))))

(ert-deftest piem-test-essential-grammars-error-without-cc ()
  "Show clear error when C compiler is not available."
  (let ((noninteractive nil)
        (error-message nil)
        (piem-essential-grammar-action 'auto))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (not (memq lang '(markdown markdown-inline)))))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir)
                 (error "Cannot find suitable compiler")))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq error-message msg)))
              ((symbol-function 'message) #'ignore))
      (piem--maybe-install-essential-grammars)
      (should (stringp error-message))
      (should (string-match-p "C compiler" error-message)))))

(ert-deftest piem-test-essential-grammars-no-install-in-batch ()
  "Never install essential grammars in batch mode."
  (let ((noninteractive t)
        (installed nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (not (memq lang '(markdown markdown-inline)))))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir)
                 (setq installed t))))
      (piem--maybe-install-essential-grammars)
      (should-not installed))))

;;; Markdown Grammar Compatibility

(ert-deftest piem-test-incompatible-markdown-grammar-detected ()
  "Detect an installed Markdown grammar that lacks required table nodes."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_lang &rest _) t))
            ((symbol-function 'piem--markdown-grammar-compatible-p)
             (lambda () nil)))
    (should (piem--markdown-grammar-incompatible-p))))

(ert-deftest piem-test-missing-markdown-grammar-not-incompatible ()
  "Missing Markdown grammar is handled by the missing-essential path."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_lang &rest _) nil))
            ((symbol-function 'piem--markdown-grammar-compatible-p)
             (lambda () nil)))
    (should-not (piem--markdown-grammar-incompatible-p))))

(ert-deftest piem-test-incompatible-markdown-grammar-warns-once ()
  "Warn once when the loaded Markdown grammar is incompatible."
  (let ((noninteractive nil)
        (piem--markdown-grammar-warning-done nil)
        (warnings nil))
    (cl-letf (((symbol-function 'piem--markdown-grammar-incompatible-p)
               (lambda () t))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (push msg warnings))))
      (piem--maybe-warn-incompatible-markdown-grammar)
      (piem--maybe-warn-incompatible-markdown-grammar)
      (should (= 1 (length warnings)))
      (should (string-match-p "Incompatible Markdown" (car warnings)))
      (should (string-match-p "treesit-extra-load-path" (car warnings))))))

;;; Grammar Recipe Validation

(ert-deftest piem-test-grammar-recipes-all-registered ()
  "All grammar recipes are registered in `treesit-language-source-alist'.
Catches accidentally dropped or malformed entries."
  (dolist (recipe piem-grammar-recipes)
    (let ((lang (car recipe)))
      (should (assq lang treesit-language-source-alist)))))

(ert-deftest piem-test-grammar-recipes-have-required-fields ()
  "Every recipe has LANG, URL, and REVISION.  SOURCE-DIR is optional."
  (dolist (recipe piem-grammar-recipes)
    (should (symbolp (nth 0 recipe)))      ; LANG
    (should (stringp (nth 1 recipe)))      ; URL
    (should (string-prefix-p "https://" (nth 1 recipe)))
    (should (stringp (nth 2 recipe)))))    ; REVISION

(ert-deftest piem-test-grammar-recipes-source-dir-entries ()
  "Recipes needing SOURCE-DIR have it set (monorepos with subdirectories)."
  (let ((ts-recipe (assq 'typescript treesit-language-source-alist))
        (tsx-recipe (assq 'tsx treesit-language-source-alist))
        (php-recipe (assq 'php treesit-language-source-alist)))
    ;; These share repos with other parsers — SOURCE-DIR is required
    (should (equal (nth 3 ts-recipe) "typescript/src"))
    (should (equal (nth 3 tsx-recipe) "tsx/src"))
    (should (equal (nth 3 php-recipe) "php/src"))))

;;; Optional Grammar Install Prompt (embedded languages)

(ert-deftest piem-test-missing-optional-grammars-detected ()
  "Detect missing optional grammars from recipe list."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (lang &rest _)
               (memq lang '(python bash)))))
    (let ((missing (piem--missing-optional-grammars)))
      ;; python and bash are installed, rest should be missing
      (should-not (memq 'python missing))
      (should-not (memq 'bash missing))
      (should-not (memq 'markdown missing))
      (should-not (memq 'markdown-inline missing))
      (should (memq 'javascript missing))
      (should (memq 'rust missing)))))

(ert-deftest piem-test-optional-grammars-offer-install ()
  "Offer to install optional grammars when missing."
  (let ((piem--grammar-prompt-done nil)
        (piem-grammar-declined-set nil)
        (noninteractive nil)
        (installed-langs nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline python))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (lang &optional _out-dir)
                 (push lang installed-langs)))
              ((symbol-function 'message) #'ignore))
      (piem--maybe-install-optional-grammars)
      ;; Should have installed some grammars (not python, already present)
      (should installed-langs)
      (should-not (memq 'python installed-langs))
      (should (memq 'javascript installed-langs)))))

(ert-deftest piem-test-optional-grammars-decline-persists ()
  "Declining optional grammars saves the missing set via customize."
  (let ((piem--grammar-prompt-done nil)
        (piem-grammar-declined-set nil)
        (noninteractive nil)
        (saved-var nil)
        (saved-val nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'customize-save-variable)
               (lambda (var val)
                 (setq saved-var var saved-val val)
                 (set var val)))
              ((symbol-function 'message) #'ignore))
      (piem--maybe-install-optional-grammars)
      (should (eq saved-var 'piem-grammar-declined-set))
      ;; Saved the full set of missing grammars
      (should (memq 'javascript saved-val))
      (should (memq 'rust saved-val)))))

(ert-deftest piem-test-optional-grammars-no-repeat-in-session ()
  "No re-prompt after already prompted this session."
  (let ((piem--grammar-prompt-done t)
        (piem-grammar-declined-set nil)
        (noninteractive nil)
        (prompted nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) (setq prompted t))))
      (piem--maybe-install-optional-grammars)
      (should-not prompted))))

(ert-deftest piem-test-optional-grammars-no-prompt-when-all-installed ()
  "No prompt when all optional grammars are already installed."
  (let ((piem--grammar-prompt-done nil)
        (piem-grammar-declined-set nil)
        (noninteractive nil)
        (prompted nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (_lang &rest _) t))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) (setq prompted t))))
      (piem--maybe-install-optional-grammars)
      (should-not prompted))))

(ert-deftest piem-test-optional-grammars-no-prompt-in-batch ()
  "Never prompt for optional grammars in batch mode."
  (let ((piem--grammar-prompt-done nil)
        (piem-grammar-declined-set nil)
        (noninteractive t)
        (prompted nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) (setq prompted t))))
      (piem--maybe-install-optional-grammars)
      (should-not prompted))))

(ert-deftest piem-test-optional-grammars-cc-failure-reports ()
  "Report failure with actionable error when compiler is missing."
  (let ((piem--grammar-prompt-done nil)
        (piem-grammar-declined-set nil)
        (noninteractive nil)
        (install-attempts 0)
        (warning-text nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir)
                 (cl-incf install-attempts)
                 (error "Cannot find suitable compiler")))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg)))
              ((symbol-function 'message) #'ignore))
      (piem--maybe-install-optional-grammars)
      (should (= install-attempts 1))
      (should (stringp warning-text))
      (should (string-match-p "C compiler" warning-text)))))

(ert-deftest piem-test-optional-grammars-prompt-mentions-command ()
  "The prompt mentions M-x piem-install-grammars."
  (let ((piem--grammar-prompt-done nil)
        (piem-grammar-declined-set nil)
        (noninteractive nil)
        (prompt-text nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline python))))
              ((symbol-function 'y-or-n-p)
               (lambda (prompt) (setq prompt-text prompt) nil))
              ((symbol-function 'customize-save-variable) #'ignore)
              ((symbol-function 'message) #'ignore))
      (piem--maybe-install-optional-grammars)
      (should (stringp prompt-text))
      (should (string-match-p "piem-install-grammars" prompt-text)))))

;;; Stickiness: Decline persists, new grammars re-prompt

(ert-deftest piem-test-optional-grammars-decline-suppresses-permanently ()
  "After declining, same missing set on next startup does NOT re-prompt."
  (let ((piem--grammar-prompt-done nil)
        (piem-grammar-declined-set nil)
        (noninteractive nil)
        (prompt-count 0))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt)
                 (cl-incf prompt-count)
                 nil))
              ((symbol-function 'customize-save-variable)
               (lambda (var val) (set var val)))
              ((symbol-function 'message) #'ignore))
      ;; First session: user declines
      (piem--maybe-install-optional-grammars)
      (should (= prompt-count 1))
      (should piem-grammar-declined-set)
      ;; Simulate Emacs restart: reset session flag, keep persisted set
      (setq piem--grammar-prompt-done nil)
      ;; Second session: same missing grammars — no prompt
      (piem--maybe-install-optional-grammars)
      (should (= prompt-count 1)))))

(ert-deftest piem-test-optional-grammars-new-grammar-reprompts ()
  "Adding a new grammar to recipes re-prompts even after a prior decline.
Simulates: user declined when javascript/rust were missing, then
a new grammar (e.g., `zig') appears in the missing set."
  (let ((piem--grammar-prompt-done nil)
        ;; Prior decline covered javascript and rust only
        (piem-grammar-declined-set '(javascript rust))
        (noninteractive nil)
        (prompted nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 ;; javascript, rust, AND go are all missing
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) (setq prompted t) nil))
              ((symbol-function 'customize-save-variable)
               (lambda (var val) (set var val)))
              ((symbol-function 'message) #'ignore))
      ;; `go' is missing but not in declined-set → re-prompt
      (piem--maybe-install-optional-grammars)
      (should prompted))))

(ert-deftest piem-test-optional-grammars-accept-does-not-persist ()
  "Accepting the install offer does not persist a declined set."
  (let ((piem--grammar-prompt-done nil)
        (piem-grammar-declined-set nil)
        (noninteractive nil)
        (customize-called nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang '(markdown markdown-inline))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir) nil))
              ((symbol-function 'customize-save-variable)
               (lambda (&rest _) (setq customize-called t)))
              ((symbol-function 'message) #'ignore))
      (piem--maybe-install-optional-grammars)
      (should-not customize-called)
      (should-not piem-grammar-declined-set))))

;;; Install Helper: piem--install-grammars

(ert-deftest piem-test-install-grammars-returns-count ()
  "install-grammars returns number of successfully installed grammars."
  (cl-letf (((symbol-function 'treesit-install-language-grammar)
             (lambda (_lang &optional _out-dir) nil))
            ((symbol-function 'message) #'ignore))
    (should (= (piem--install-grammars '(python rust go)) 3))))

(ert-deftest piem-test-install-grammars-empty-list ()
  "install-grammars with empty list returns 0."
  (should (= (piem--install-grammars '()) 0)))

(ert-deftest piem-test-install-grammars-failure-returns-partial-count ()
  "install-grammars returns count of grammars installed before failure."
  (let ((warning-text nil))
    (cl-letf (((symbol-function 'treesit-install-language-grammar)
               (lambda (lang &optional _out-dir)
                 (when (eq lang 'rust)
                   (error "cc: not found"))))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg)))
              ((symbol-function 'message) #'ignore))
      ;; python succeeds (idx=1), rust fails (idx=2, returned as 1)
      (should (= (piem--install-grammars '(python rust go)) 1))
      (should (string-match-p "rust" warning-text))
      (should (string-match-p "1/3" warning-text)))))

(ert-deftest piem-test-install-grammars-names-failing-grammar ()
  "install-grammars warning identifies which grammar failed."
  (let ((warning-text nil))
    (cl-letf (((symbol-function 'treesit-install-language-grammar)
               (lambda (lang &optional _out-dir)
                 (when (eq lang 'go)
                   (error "compilation failed"))))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg)))
              ((symbol-function 'message) #'ignore))
      (piem--install-grammars '(python rust go))
      (should (string-match-p "`go'" warning-text)))))

;;; Installed Optional Grammars

(ert-deftest piem-test-installed-optional-grammars ()
  "installed-optional-grammars returns only grammars that are available."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (lang &rest _)
               (memq lang '(python rust)))))
    (let ((installed (piem--installed-optional-grammars)))
      (should (memq 'python installed))
      (should (memq 'rust installed))
      (should-not (memq 'javascript installed)))))

;;; Interactive Command: M-x piem-install-grammars

(ert-deftest piem-test-install-grammars-command-all-installed ()
  "Interactive command shows message when all grammars are installed."
  (let ((msg nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (_lang &rest _) t))
              ((symbol-function 'piem--markdown-grammar-compatible-p)
               (lambda () t))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (piem-install-grammars)
      (should (string-match-p "installed" msg))
      (should (string-match-p "✓" msg)))))

(ert-deftest piem-test-install-grammars-command-shows-status-buffer ()
  "Interactive command creates status buffer listing missing grammars."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (lang &rest _)
               (memq lang '(markdown markdown-inline python))))
            ((symbol-function 'piem--markdown-grammar-compatible-p)
             (lambda () t))
            ((symbol-function 'pop-to-buffer)
             #'ignore))
    (unwind-protect
        (progn
          (piem-install-grammars)
          (let ((buf (get-buffer "*piem-grammars*")))
            (should buf)
            (with-current-buffer buf
              ;; Has missing grammars listed
              (should (string-match-p "Missing" (buffer-string)))
              (should (string-match-p "javascript" (buffer-string)))
              ;; Has installed grammars listed
              (should (string-match-p "Installed" (buffer-string)))
              (should (string-match-p "python" (buffer-string)))
              ;; Has keybinding hint
              (should (string-match-p "Press.*i.*to install" (buffer-string)))
              ;; Is in special-mode (read-only)
              (should (derived-mode-p 'special-mode)))))
      (when-let* ((buf (get-buffer "*piem-grammars*")))
        (kill-buffer buf)))))

(ert-deftest piem-test-install-grammars-command-shows-essential-missing ()
  "Interactive command highlights missing essential grammars prominently."
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_lang &rest _) nil))
            ((symbol-function 'piem--markdown-grammar-compatible-p)
             (lambda () t))
            ((symbol-function 'pop-to-buffer)
             #'ignore))
    (unwind-protect
        (progn
          (piem-install-grammars)
          (let ((buf (get-buffer "*piem-grammars*")))
            (should buf)
            (with-current-buffer buf
              (should (string-match-p "ESSENTIAL" (buffer-string)))
              (should (string-match-p "markdown" (buffer-string))))))
      (when-let* ((buf (get-buffer "*piem-grammars*")))
        (kill-buffer buf)))))

(ert-deftest piem-test-install-grammars-command-warns-incompatible-markdown ()
  "Interactive command warns when an installed Markdown grammar is incompatible."
  (let ((warning-text nil)
        (msg nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (_lang &rest _) t))
              ((symbol-function 'piem--markdown-grammar-compatible-p)
               (lambda () nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (piem-install-grammars)
      (should (string-match-p "Incompatible Markdown" warning-text))
      (should-not msg))))

;;; CI Install Script Smoke Test

(ert-deftest piem-test-ci-install-script-loads ()
  "The CI grammar install script loads without error.
Catches wiring bugs like requiring deleted modules."
  ;; Just load it — if the requires are broken, this errors.
  ;; We mock the install loop to avoid actually compiling grammars.
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_lang &rest _) t))
            ((symbol-function 'message) #'ignore))
    ;; Tests run from the project root (Makefile sets load-path to ".")
    (load (expand-file-name "scripts/install-ts-grammars.el") nil t t)))

;;; check-dependencies

(ert-deftest piem-test-check-dependencies-calls-grammar-checks ()
  "check-dependencies invokes grammar install and compatibility checks."
  (let ((essential-called nil)
        (compatibility-called nil)
        (optional-called nil))
    (cl-letf (((symbol-function 'piem--check-pi) (lambda (&optional _directory) t))
              ((symbol-function 'piem--maybe-install-essential-grammars)
               (lambda () (setq essential-called t)))
              ((symbol-function 'piem--maybe-warn-incompatible-markdown-grammar)
               (lambda () (setq compatibility-called t)))
              ((symbol-function 'piem--maybe-install-optional-grammars)
               (lambda () (setq optional-called t))))
      (piem--check-dependencies)
      (should essential-called)
      (should compatibility-called)
      (should optional-called))))

;;; State response

(ert-deftest piem-test-session-busy-includes-prompt-start-wait ()
  "A locally pending prompt keeps the session busy before Pi echoes events."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((generation (piem--begin-prompt-start-wait)))
      (setq piem--status 'idle)
      (should (piem--prompt-start-current-p generation))
      (should (piem--session-busy-p (current-buffer))))))

(ert-deftest piem-test-session-busy-includes-session-transition ()
  "An in-flight session switch keeps the session busy."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--status 'idle)
    (let ((generation (piem--begin-session-transition 'mock-proc)))
      (should (piem--session-transition-active-p (current-buffer)))
      (should (piem--session-busy-p (current-buffer)))
      (piem--finish-session-transition generation)
      (should-not (piem--session-transition-active-p
                   (current-buffer))))))

(ert-deftest piem-test-apply-state-response-normalizes-remote-session-file ()
  "Applying state anchors inbound sessionFile paths in the chat session dir."
  (let ((chat-buf (generate-new-buffer "*test-state-remote-session-file*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (piem-chat-mode)
          (piem--set-chat-session-identity
           "/ssh:pi-host:/home/pi/project/")
          (piem--apply-state-response
           chat-buf
           '(:success t :data (:isStreaming :false
                               :isCompacting :false
                               :sessionId "remote-session"
                               :sessionFile "/home/pi/.pi/sessions/current.jsonl")))
          (should (equal (plist-get piem--state :session-file)
                         "/ssh:pi-host:/home/pi/.pi/sessions/current.jsonl")))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-apply-state-response-ignores-nul-session-file ()
  "Applying state does not store unsafe sessionFile as a navigable path."
  (let ((chat-buf (generate-new-buffer "*test-state-nul-session-file*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (piem-chat-mode)
          (let ((bad (concat "/tmp/a" (string ?\0) "b.jsonl")))
            (piem--apply-state-response
             chat-buf
             (list :success t
                   :data (list :isStreaming :false
                               :isCompacting :false
                               :sessionId "nul-session"
                               :sessionFile bad)))
            (should (equal (plist-get piem--state :session-id)
                           "nul-session"))
            (should-not (plist-get piem--state :session-file))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-apply-state-response-keeps-local-prompt-start-busy ()
  "Stale idle get_state must not erase local prompt preflight state."
  (let ((chat-buf (generate-new-buffer "*test-state-local-prompt-start*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (piem-chat-mode)
          (let ((generation (piem--begin-prompt-start-wait)))
            (setq piem--status 'sending
                  piem--state '(:session-id "same-session"))
            (piem--apply-state-response
             chat-buf
             '(:success t :data (:isStreaming :false
                                 :isCompacting :false
                                 :sessionId "same-session"
                                 :sessionFile "/tmp/same.jsonl")))
            (should (piem--prompt-start-current-p generation))
            (should (eq piem--status 'sending))
            (should (eq (plist-get piem--state :status) 'sending))
            (should (piem--session-busy-p chat-buf))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-apply-state-response-preserves-extension-ui-warnings-without-session-change ()
  "Applying state keeps unsupported UI warnings within the same pi session."
  (let ((chat-buf (generate-new-buffer "*test-state-same-session*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--state nil
                  piem--unsupported-extension-ui-methods-warned
                  '("setWidget")))
          (piem--apply-state-response
           chat-buf
           '(:success t :data (:isStreaming :false
                               :sessionId "new-session"
                               :sessionFile "/tmp/new.jsonl")))
          (with-current-buffer chat-buf
            (should (equal piem--unsupported-extension-ui-methods-warned
                           '("setWidget"))))
          (with-current-buffer chat-buf
            (setq piem--unsupported-extension-ui-methods-warned
                  '("setWidget")))
          (piem--apply-state-response
           chat-buf
           '(:success t :data (:isStreaming :false
                               :sessionId "new-session"
                               :sessionFile "/tmp/newer.jsonl")))
          (with-current-buffer chat-buf
            (should (equal piem--unsupported-extension-ui-methods-warned
                           '("setWidget")))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-apply-state-response-resets-extension-ui-warnings-on-session-change ()
  "Applying state clears unsupported UI warnings when the pi session changes."
  (let ((chat-buf (generate-new-buffer "*test-state-session-change*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--state '(:session-id "old-session")
                  piem--unsupported-extension-ui-methods-warned
                  '("setWidget")))
          (piem--apply-state-response
           chat-buf
           '(:success t :data (:isStreaming :false
                               :sessionId "new-session"
                               :sessionFile "/tmp/new.jsonl")))
          (with-current-buffer chat-buf
            (should (equal (plist-get piem--state :session-id)
                           "new-session"))
            (should (null piem--unsupported-extension-ui-methods-warned))))
      (kill-buffer chat-buf))))

;;; Input Window Height (integer and float ratio)

(ert-deftest piem-test-input-height-integer-returns-configured-value ()
  "Integer setting returns that many lines when window is large enough."
  (let ((piem-input-window-height 10)
        (window-min-height 4))
    (should (= (piem--input-height-for-window-height 40) 10))))

(ert-deftest piem-test-input-height-integer-clamps-to-max ()
  "Integer setting clamps when window is too small."
  (let ((piem-input-window-height 10)
        (window-min-height 4))
    (should (= (piem--input-height-for-window-height 12) 8))))

(ert-deftest piem-test-input-height-float-computes-ratio ()
  "Float setting computes height as fraction of total."
  (let ((piem-input-window-height 0.3)
        (window-min-height 4))
    (should (= (piem--input-height-for-window-height 40) 12))))

(ert-deftest piem-test-input-height-float-clamps-to-min ()
  "Float setting clamps up to window-min-height for tiny ratios."
  (let ((piem-input-window-height 0.05)
        (window-min-height 4))
    (should (= (piem--input-height-for-window-height 40) 4))))

(ert-deftest piem-test-input-height-float-clamps-to-max ()
  "Float setting clamps down to preserve chat min-height."
  (let ((piem-input-window-height 0.9)
        (window-min-height 4))
    (should (= (piem--input-height-for-window-height 40) 36))))

(ert-deftest piem-test-input-height-float-small-window ()
  "Float ratio on a small total still respects min heights."
  (let ((piem-input-window-height 0.3)
        (window-min-height 4))
    (should (= (piem--input-height-for-window-height 10) 4))))

;;; Dynamic ratio rebalancing

(defmacro piem-test-with-split-layout (&rest body)
  "Execute BODY with a chat/input window pair.
Binds `chat-win' and `input-win' for use in BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (piem-chat-mode)
     (let ((input-buf (generate-new-buffer " *test-input*")))
       (unwind-protect
           (progn
             (setq-local piem--input-buffer input-buf)
             (delete-other-windows)
             (switch-to-buffer (current-buffer))
             (let* ((input-win (split-window nil -10 'below))
                    (chat-win (selected-window)))
               (set-window-buffer input-win input-buf)
               ,@body))
         (when (buffer-live-p input-buf)
           (kill-buffer input-buf))))))

(ert-deftest piem-test-rebalance-adjusts-float-ratio ()
  "Rebalance resizes input window to match float ratio."
  (let ((piem-input-window-height 0.3))
    (piem-test-with-split-layout
      (piem--rebalance-input-window chat-win input-win)
      (let* ((total (+ (window-total-height chat-win)
                       (window-total-height input-win)))
             (expected (piem--input-height-for-window-height total)))
        (should (= (window-total-height input-win) expected))))))

(ert-deftest piem-test-rebalance-skips-integer-height ()
  "Rebalance is a no-op when height is an integer."
  (let ((piem-input-window-height 10))
    (piem-test-with-split-layout
      (let ((before (window-total-height input-win)))
        (piem--rebalance-input-window chat-win input-win)
        (should (= (window-total-height input-win) before))))))

(ert-deftest piem-test-chat-mode-adds-size-change-hook ()
  "Chat mode installs the window-size-change rebalance hook."
  (with-temp-buffer
    (piem-chat-mode)
    (should (memq #'piem--maybe-rebalance-windows
                  window-size-change-functions))))

(provide 'piem-ui-test)
;;; piem-ui-test.el ends here
