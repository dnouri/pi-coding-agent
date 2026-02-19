;;; pi-coding-agent-test.el --- Tests for pi-coding-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Entry-point and cross-module integration tests for pi-coding-agent.

;;; Code:

(require 'ert)
(require 'pi-coding-agent)
(require 'pi-coding-agent-test-common)

;;; Main Entry Point

(ert-deftest pi-coding-agent-test-pi-coding-agent-creates-chat-buffer ()
  "M-x pi-coding-agent creates a chat buffer."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-main/"
    (should (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-main/*"))))

(ert-deftest pi-coding-agent-test-pi-coding-agent-creates-input-buffer ()
  "M-x pi-coding-agent creates an input buffer."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-main2/"
    (should (get-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-main2/*"))))

(ert-deftest pi-coding-agent-test-pi-coding-agent-sets-major-modes ()
  "M-x pi-coding-agent sets correct major modes on buffers."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-modes/"
    (with-current-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-modes/*"
      (should (derived-mode-p 'pi-coding-agent-chat-mode)))
    (with-current-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-modes/*"
      (should (derived-mode-p 'pi-coding-agent-input-mode)))))

;;; DWIM & Toggle

(ert-deftest pi-coding-agent-test-dwim-reuses-existing-session ()
  "Calling `pi-coding-agent' from a non-pi buffer reuses the existing session."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-dwim/"
    ;; Session exists; now call from a non-pi buffer in the same project
    (with-temp-buffer
      (setq default-directory "/tmp/pi-coding-agent-test-dwim/")
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
        (pi-coding-agent))
      ;; Should not have created a second chat buffer
      (should (= 1 (length (cl-remove-if-not
                             (lambda (b)
                               (string-prefix-p "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-dwim/"
                                                (buffer-name b)))
                             (buffer-list))))))))

(ert-deftest pi-coding-agent-test-from-chat-buffer-noop-when-both-visible ()
  "From chat, `pi-coding-agent' avoids redisplay and focuses input."
  (let ((root "/tmp/pi-coding-agent-test-chat-visible/")
        (display-called nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (setq default-directory root)
            (pi-coding-agent)
            (let ((chat (get-buffer (pi-coding-agent-test--chat-buffer-name root)))
                  (input (get-buffer (pi-coding-agent-test--input-buffer-name root))))
              (select-window (car (get-buffer-window-list chat nil t)))
              (with-current-buffer chat
                (cl-letf (((symbol-function 'pi-coding-agent--display-buffers)
                           (lambda (&rest _)
                             (setq display-called t))))
                  (pi-coding-agent)))
              (should-not display-called)
              (should (get-buffer-window-list chat nil t))
              (should (get-buffer-window-list input nil t))
              (should (eq (window-buffer (selected-window)) input))))
        (pi-coding-agent-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pi-coding-agent-test-from-input-buffer-noop-when-both-visible ()
  "From input, `pi-coding-agent' avoids redisplay when both panes are visible."
  (let ((root "/tmp/pi-coding-agent-test-input-visible/")
        (display-called nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (setq default-directory root)
            (pi-coding-agent)
            (let ((chat (get-buffer (pi-coding-agent-test--chat-buffer-name root)))
                  (input (get-buffer (pi-coding-agent-test--input-buffer-name root))))
              (with-current-buffer input
                (cl-letf (((symbol-function 'pi-coding-agent--display-buffers)
                           (lambda (&rest _)
                             (setq display-called t))))
                  (pi-coding-agent)))
              (should-not display-called)
              (should (get-buffer-window-list chat nil t))
              (should (get-buffer-window-list input nil t))))
        (pi-coding-agent-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pi-coding-agent-test-from-chat-buffer-focuses-current-session-input ()
  "With multiple sessions visible, `pi-coding-agent' focuses this session's input."
  (let ((root "/tmp/pi-coding-agent-test-focus-root/")
        (sub "/tmp/pi-coding-agent-test-focus-root/somesubdir/")
        (display-called nil))
    (make-directory root t)
    (make-directory sub t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (with-temp-buffer
              (setq default-directory root)
              (pi-coding-agent))
            (with-temp-buffer
              (setq default-directory sub)
              (pi-coding-agent))
            (let ((root-chat (get-buffer (pi-coding-agent-test--chat-buffer-name root)))
                  (root-input (get-buffer (pi-coding-agent-test--input-buffer-name root)))
                  (sub-input (get-buffer (pi-coding-agent-test--input-buffer-name sub))))
              (delete-other-windows)
              (switch-to-buffer root-chat)
              (let ((root-input-win (split-window nil -10 'below)))
                (set-window-buffer root-input-win root-input))
              (let ((sub-win (split-window-right)))
                (set-window-buffer sub-win sub-input))
              (select-window (get-buffer-window root-chat))
              (with-current-buffer root-chat
                (cl-letf (((symbol-function 'pi-coding-agent--display-buffers)
                           (lambda (&rest _)
                             (setq display-called t))))
                  (pi-coding-agent)))
              (should-not display-called)
              (should (eq (window-buffer (selected-window)) root-input))))
        (pi-coding-agent-test--kill-session-buffers root)
        (pi-coding-agent-test--kill-session-buffers sub)
        (delete-other-windows)))))

(ert-deftest pi-coding-agent-test-from-pi-buffer-redisplays-when-visible-only-in-other-frame ()
  "Calling `pi-coding-agent' should redisplay in current frame.
Even if chat/input are visible in another frame, current-frame visibility
must decide whether this is a no-op."
  (let ((root "/tmp/pi-coding-agent-test-other-frame-noop/")
        (display-called nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (setq default-directory root)
            (pi-coding-agent)
            (let ((chat (get-buffer (pi-coding-agent-test--chat-buffer-name root))))
              (with-current-buffer chat
                (cl-letf (((symbol-function 'get-buffer-window-list)
                           (lambda (_buffer _minibuf &optional all-frames)
                             (if all-frames '(foreign-window) nil)))
                          ((symbol-function 'pi-coding-agent--display-buffers)
                           (lambda (&rest _)
                             (setq display-called t))))
                  (pi-coding-agent)))
              (should display-called)))
        (pi-coding-agent-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pi-coding-agent-test-from-chat-buffer-restores-missing-input-window ()
  "Calling `pi-coding-agent' from chat restores input and focuses it."
  (let ((root "/tmp/pi-coding-agent-test-chat-restore/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (setq default-directory root)
            (pi-coding-agent)
            (let ((chat (get-buffer (pi-coding-agent-test--chat-buffer-name root)))
                  (input (get-buffer (pi-coding-agent-test--input-buffer-name root))))
              (delete-window (car (get-buffer-window-list input nil t)))
              (with-current-buffer chat
                (pi-coding-agent))
              (should (= 1 (length (get-buffer-window-list input nil t))))
              (should (eq (window-buffer (selected-window)) input))))
        (pi-coding-agent-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pi-coding-agent-test-from-input-buffer-restores-missing-chat-window ()
  "Calling `pi-coding-agent' from input restores the split layout."
  (let ((root "/tmp/pi-coding-agent-test-input-restore/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (setq default-directory root)
            (pi-coding-agent)
            (let ((chat (get-buffer (pi-coding-agent-test--chat-buffer-name root)))
                  (input (get-buffer (pi-coding-agent-test--input-buffer-name root))))
              (let ((input-win (car (get-buffer-window-list input nil t))))
                (select-window input-win)
                (delete-other-windows input-win))
              (with-current-buffer input
                (pi-coding-agent))
              (should (= 1 (length (get-buffer-window-list chat nil t))))
              (should (= 1 (length (get-buffer-window-list input nil t))))))
        (pi-coding-agent-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pi-coding-agent-test-non-pi-call-creates-default-session-when-only-named-exists ()
  "Calling `pi-coding-agent' creates default session when only named one exists."
  (let* ((root "/tmp/pi-coding-agent-test-dwim-named/")
         (default-directory root)
         (displayed nil)
         (named-chat (pi-coding-agent-test--chat-buffer-name root "my-feature"))
         (named-input (pi-coding-agent-test--input-buffer-name root "my-feature"))
         (default-chat (pi-coding-agent-test--chat-buffer-name root))
         (default-input (pi-coding-agent-test--input-buffer-name root)))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
              ((symbol-function 'pi-coding-agent--display-buffers)
               (lambda (chat _input) (setq displayed chat))))
      (unwind-protect
          (progn
            ;; Create named session first.
            (pi-coding-agent "my-feature")
            ;; Non-pi call should create/reuse default unnamed session.
            (with-temp-buffer
              (setq default-directory root)
              (setq displayed nil)
              (pi-coding-agent)
              (should displayed)
              (should (equal (buffer-name displayed) default-chat)))
            (should (get-buffer named-chat))
            (should (get-buffer named-input))
            (should (get-buffer default-chat))
            (should (get-buffer default-input)))
        (pi-coding-agent-test--kill-session-buffers root "my-feature")
        (pi-coding-agent-test--kill-session-buffers root)))))

(ert-deftest pi-coding-agent-test-new-session-with-prefix-arg ()
  "\\[universal-argument] \\[pi-coding-agent] creates a named session."
  (let ((root "/tmp/pi-coding-agent-test-named/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
              ((symbol-function 'pi-coding-agent--display-buffers) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) "my-session")))
      (let ((current-prefix-arg '(4))
            (default-directory root))
        (unwind-protect
            (progn
              (call-interactively #'pi-coding-agent)
              (should (get-buffer (pi-coding-agent-test--chat-buffer-name root "my-session"))))
          (pi-coding-agent-test--kill-session-buffers root "my-session"))))))

(ert-deftest pi-coding-agent-test-non-pi-rerun-from-small-window-does-not-error ()
  "Calling `pi-coding-agent' from a small non-pi window should not error."
  (let ((root "/tmp/pi-coding-agent-test-small-window/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (with-temp-buffer
              (setq default-directory root)
              (pi-coding-agent))
            (let* ((chat (get-buffer (pi-coding-agent-test--chat-buffer-name root)))
                   (input (get-buffer (pi-coding-agent-test--input-buffer-name root)))
                   (input-win (car (get-buffer-window-list input nil t)))
                   (non-pi (get-buffer-create "*pi-coding-agent-test-non-pi*")))
              (select-window input-win)
              (with-current-buffer non-pi
                (setq default-directory root))
              (switch-to-buffer non-pi)
              (pi-coding-agent)
              (should (get-buffer-window-list chat nil t))
              (should (get-buffer-window-list input nil t))))
        (pi-coding-agent-test--kill-session-buffers root)
        (ignore-errors (kill-buffer "*pi-coding-agent-test-non-pi*"))
        (delete-other-windows)))))

(ert-deftest pi-coding-agent-test-non-pi-rerun-with-chat-hidden-avoids-duplicate-input-windows ()
  "Restoring from input-only visibility should keep a single input window."
  (let ((root "/tmp/pi-coding-agent-test-input-only-rerun/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (with-temp-buffer
              (setq default-directory root)
              (pi-coding-agent))
            (let* ((chat (get-buffer (pi-coding-agent-test--chat-buffer-name root)))
                   (input (get-buffer (pi-coding-agent-test--input-buffer-name root)))
                   (chat-win (car (get-buffer-window-list chat nil t)))
                   (non-pi (get-buffer-create "*pi-coding-agent-test-non-pi*")))
              ;; Hide chat by replacing it with a non-pi buffer, leaving input visible.
              (select-window chat-win)
              (with-current-buffer non-pi
                (setq default-directory root))
              (switch-to-buffer non-pi)
              (pi-coding-agent)
              (should (= 1 (length (get-buffer-window-list input nil t))))
              (should (= 1 (length (get-buffer-window-list chat nil t))))))
        (pi-coding-agent-test--kill-session-buffers root)
        (ignore-errors (kill-buffer "*pi-coding-agent-test-non-pi*"))
        (delete-other-windows)))))

(ert-deftest pi-coding-agent-test-project-buffers-excludes-subdir-sessions ()
  "`pi-coding-agent-project-buffers' should match the directory exactly."
  (let ((root "/tmp/pi-coding-agent-test-root/")
        (sub "/tmp/pi-coding-agent-test-root/somesubdir/"))
    (make-directory root t)
    (make-directory sub t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
              ((symbol-function 'pi-coding-agent--check-dependencies) #'ignore))
      (unwind-protect
          (progn
            (with-temp-buffer
              (setq default-directory root)
              (pi-coding-agent--setup-session root nil))
            (with-temp-buffer
              (setq default-directory sub)
              (pi-coding-agent--setup-session sub nil))
            (with-temp-buffer
              (setq default-directory root)
              (let ((buffers (pi-coding-agent-project-buffers)))
                (should (= 1 (length buffers)))
                (should (equal (car buffers)
                               (get-buffer (pi-coding-agent-test--chat-buffer-name root)))))))
        (pi-coding-agent-test--kill-session-buffers root)
        (pi-coding-agent-test--kill-session-buffers sub)))))

(ert-deftest pi-coding-agent-test-toggle-uses-exact-project-session ()
  "`pi-coding-agent-toggle' should not pick a subdir session for parent dir."
  (let ((root "/tmp/pi-coding-agent-test-toggle-root/")
        (sub "/tmp/pi-coding-agent-test-toggle-root/somesubdir/")
        (displayed-name nil))
    (make-directory root t)
    (make-directory sub t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
              ((symbol-function 'pi-coding-agent--check-dependencies) #'ignore)
              ((symbol-function 'pi-coding-agent--display-buffers)
               (lambda (chat _input)
                 (setq displayed-name (buffer-name chat)))))
      (unwind-protect
          (progn
            (with-temp-buffer
              (setq default-directory root)
              (pi-coding-agent--setup-session root nil))
            (with-temp-buffer
              (setq default-directory sub)
              (pi-coding-agent--setup-session sub nil))
            ;; Make subdir chat more recent, then hide all pi windows.
            (switch-to-buffer (pi-coding-agent-test--chat-buffer-name sub))
            (switch-to-buffer "*scratch*")
            (with-temp-buffer
              (setq default-directory root)
              (pi-coding-agent-toggle))
            (should (equal displayed-name
                           (pi-coding-agent-test--chat-buffer-name root))))
        (pi-coding-agent-test--kill-session-buffers root)
        (pi-coding-agent-test--kill-session-buffers sub)))))

(ert-deftest pi-coding-agent-test-toggle-from-pi-buffer-uses-current-session ()
  "`pi-coding-agent-toggle' from pi buffer should use current session directly."
  (let ((root "/tmp/pi-coding-agent-test-toggle-current-root/")
        (sub "/tmp/pi-coding-agent-test-toggle-current-root/somesubdir/"))
    (make-directory root t)
    (make-directory sub t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
              ((symbol-function 'pi-coding-agent--check-dependencies) #'ignore)
              ;; If toggle consulted project-buffers here, it would pick sub.
              ((symbol-function 'pi-coding-agent-project-buffers)
               (lambda ()
                 (list (get-buffer (pi-coding-agent-test--chat-buffer-name sub))))))
      (unwind-protect
          (progn
            (with-temp-buffer
              (setq default-directory root)
              (pi-coding-agent))
            (with-temp-buffer
              (setq default-directory sub)
              (pi-coding-agent--setup-session sub nil))
            (let ((root-chat (get-buffer (pi-coding-agent-test--chat-buffer-name root)))
                  (root-input (get-buffer (pi-coding-agent-test--input-buffer-name root)))
                  (sub-chat (get-buffer (pi-coding-agent-test--chat-buffer-name sub))))
              (with-current-buffer root-chat
                (pi-coding-agent-toggle))
              (should-not (get-buffer-window-list root-chat nil t))
              (should-not (get-buffer-window-list root-input nil t))
              (should-not (get-buffer-window-list sub-chat nil t))))
        (pi-coding-agent-test--kill-session-buffers root)
        (pi-coding-agent-test--kill-session-buffers sub)
        (delete-other-windows)))))

(ert-deftest pi-coding-agent-test-project-buffers-finds-session ()
  "`pi-coding-agent-project-buffers' returns chat buffer for the current project."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-projbuf/"
    (let ((default-directory "/tmp/pi-coding-agent-test-projbuf/"))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (should (= 1 (length (pi-coding-agent-project-buffers))))
        (should (string-prefix-p "*pi-coding-agent-chat:"
                                 (buffer-name (car (pi-coding-agent-project-buffers)))))))))

(ert-deftest pi-coding-agent-test-project-buffers-excludes-other-projects ()
  "`pi-coding-agent-project-buffers' returns nil for a different project."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-projbuf-a/"
    (let ((default-directory "/tmp/pi-coding-agent-test-projbuf-b/"))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (should (null (pi-coding-agent-project-buffers)))))))

(ert-deftest pi-coding-agent-test-toggle-no-session-errors ()
  "`pi-coding-agent-toggle' signals `user-error' when no session exists."
  (let ((default-directory "/tmp/pi-coding-agent-test-no-session/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (should-error (pi-coding-agent-toggle) :type 'user-error))))

(ert-deftest pi-coding-agent-test-toggle-shows-in-current-frame-when-only-visible-elsewhere ()
  "`pi-coding-agent-toggle' should show in current frame when hidden there."
  (let ((root "/tmp/pi-coding-agent-test-toggle-other-frame/")
        (display-called nil)
        (hide-called nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (with-temp-buffer
              (setq default-directory root)
              (pi-coding-agent))
            (with-temp-buffer
              (setq default-directory root)
              (cl-letf (((symbol-function 'get-buffer-window-list)
                         (lambda (_buffer _minibuf &optional all-frames)
                           (if all-frames '(foreign-window) nil)))
                        ((symbol-function 'pi-coding-agent--display-buffers)
                         (lambda (&rest _)
                           (setq display-called t)))
                        ((symbol-function 'pi-coding-agent--hide-session-windows)
                         (lambda ()
                           (setq hide-called t))))
                (pi-coding-agent-toggle)))
            (should display-called)
            (should-not hide-called))
        (pi-coding-agent-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pi-coding-agent-test-toggle-hides-session-from-non-pi-window ()
  "`pi-coding-agent-toggle' hides a visible session when called from non-pi."
  (let ((root "/tmp/pi-coding-agent-test-toggle-hide/")
        (chat nil)
        (input nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (setq default-directory root)
            (pi-coding-agent)
            (setq chat (get-buffer (pi-coding-agent-test--chat-buffer-name root)))
            (setq input (get-buffer (pi-coding-agent-test--input-buffer-name root)))
            (let* ((input-win (car (get-buffer-window-list input nil t)))
                   (non-pi (get-buffer-create "*pi-coding-agent-test-non-pi*")))
              (select-window input-win)
              (with-current-buffer non-pi
                (setq default-directory root))
              (switch-to-buffer non-pi)
              (pi-coding-agent-toggle))
            (should-not (get-buffer-window-list chat nil t))
            (should-not (get-buffer-window-list input nil t)))
        (pi-coding-agent-test--kill-session-buffers root)
        (ignore-errors (kill-buffer "*pi-coding-agent-test-non-pi*"))
        (delete-other-windows)))))

(ert-deftest pi-coding-agent-test-toggle-hides-session-when-only-input-visible ()
  "`pi-coding-agent-toggle' hides session when only input is visible."
  (let ((root "/tmp/pi-coding-agent-test-toggle-input-only/")
        (chat nil)
        (input nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (setq default-directory root)
            (pi-coding-agent)
            (setq chat (get-buffer (pi-coding-agent-test--chat-buffer-name root)))
            (setq input (get-buffer (pi-coding-agent-test--input-buffer-name root)))
            (let* ((chat-win (car (get-buffer-window-list chat nil t)))
                   (non-pi (get-buffer-create "*pi-coding-agent-test-non-pi*")))
              ;; Keep only input visible by replacing chat with a non-pi buffer.
              (select-window chat-win)
              (with-current-buffer non-pi
                (setq default-directory root))
              (switch-to-buffer non-pi)
              (pi-coding-agent-toggle))
            (should-not (get-buffer-window-list chat nil t))
            (should-not (get-buffer-window-list input nil t)))
        (pi-coding-agent-test--kill-session-buffers root)
        (ignore-errors (kill-buffer "*pi-coding-agent-test-non-pi*"))
        (delete-other-windows)))))

(provide 'pi-coding-agent-test)
;;; pi-coding-agent-test.el ends here
