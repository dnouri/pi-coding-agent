;;; pi-coding-agent-test.el --- Tests for pi-coding-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Entry-point and cross-module integration tests for pi-coding-agent.

;;; Code:

(require 'dired)
(require 'ert)
(require 'pi-coding-agent)
(require 'pi-coding-agent-test-common)

;;; Shared Test Helpers

(defun pi-coding-agent-test--make-open-session-command-buffers (&optional process)
  "Return linked chat/input/process fixtures for open-session-file tests.
PROCESS defaults to a harmless pipe process because pi buffer cleanup expects
`pi-coding-agent--process' to be either nil or a process object.  These tests
still mock the RPC boundary, so the process is never used for I/O."
  (let ((chat-buf (generate-new-buffer " *pi-coding-agent-open-session-chat*"))
        (input-buf (generate-new-buffer " *pi-coding-agent-open-session-input*"))
        (proc (or process
                  (make-pipe-process :name "pi-coding-agent-open-session-test"
                                     :buffer nil
                                     :noquery t))))
    (with-current-buffer chat-buf
      (pi-coding-agent-chat-mode)
      (pi-coding-agent--set-input-buffer input-buf)
      (pi-coding-agent--set-process proc))
    (with-current-buffer input-buf
      (pi-coding-agent-input-mode)
      (pi-coding-agent--set-chat-buffer chat-buf))
    (list chat-buf input-buf proc)))

(ert-deftest pi-coding-agent-test-backend-spec-builds-fake-launch-config ()
  "Shared test helper builds fake backend launch data from a scenario name."
  (let* ((spec (pi-coding-agent-test-backend-spec 'fake "prompt-lifecycle"
                                                  "tool-read"
                                                  '("--log-file" "/tmp/fake-pi.log")))
         (executable (plist-get spec :executable)))
    (should (eq (plist-get spec :name) 'fake))
    (should (equal (plist-get spec :label) "fake:tool-read"))
    (should (equal (plist-get spec :scenario) "tool-read"))
    (should (equal (plist-get spec :extra-args)
                   '("--scenario" "tool-read"
                     "--log-file" "/tmp/fake-pi.log")))
    (should (equal (car executable)
                   (pi-coding-agent-test-python-executable)))
    (should (equal (cadr executable)
                   pi-coding-agent-test-fake-pi-script))))

(ert-deftest pi-coding-agent-test-backend-spec-builds-real-launch-config ()
  "Shared test helper preserves the configured real backend launch command."
  (let ((pi-coding-agent-executable '("pi" "rpc"))
        (pi-coding-agent-extra-args '("--model" "fake")))
    (let ((spec (pi-coding-agent-test-backend-spec 'real "prompt-lifecycle")))
      (should (eq (plist-get spec :name) 'real))
      (should (equal (plist-get spec :label) "real"))
      (should (equal (plist-get spec :executable) '("pi" "rpc")))
      (should (equal (plist-get spec :extra-args) '("--model" "fake")))
      (should-not (plist-member spec :scenario)))))

(ert-deftest pi-coding-agent-test-backend-spec-rejects-unknown-backend ()
  "Shared backend helper should fail loudly for unsupported backends."
  (should-error
   (pi-coding-agent-test-backend-spec 'bogus "prompt-lifecycle")))

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

(ert-deftest pi-coding-agent-test-open-session-file-validates-sets-up-displays-and-resumes ()
  "Opening a valid session file uses the normal live-session path."
  (let* ((project-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-project-"))
         (session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-sessions-"))
         (session-file (expand-file-name "session.jsonl" session-dir))
         (original-validator (symbol-function
                              'pi-coding-agent--session-file-cwd-or-error))
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (proc (caddr buffers))
         (calls nil))
    (unwind-protect
        (progn
          (pi-coding-agent-test--write-session-file
           session-file "hello" (directory-file-name project-dir))
          (cl-letf (((symbol-function 'pi-coding-agent--session-file-cwd-or-error)
                     (lambda (path)
                       (push (list 'validate path) calls)
                       (funcall original-validator path)))
                    ((symbol-function 'pi-coding-agent--check-dependencies)
                     (lambda (&optional directory)
                       (push (list 'check-dependencies directory) calls)))
                    ((symbol-function 'pi-coding-agent--setup-session)
                     (lambda (dir &optional session)
                       (push (list 'setup-session dir session) calls)
                       chat-buf))
                    ((symbol-function 'pi-coding-agent--display-buffers)
                     (lambda (chat input)
                       (push (list 'display chat input) calls)))
                    ((symbol-function 'pi-coding-agent--session-transition-ready-p)
                     (lambda (chat action)
                       (push (list 'ready chat action) calls)
                       t))
                    ((symbol-function 'pi-coding-agent--resume-selected-session)
                     (lambda (proc chat path)
                       (push (list 'resume proc chat path) calls))))
            (should (eq (pi-coding-agent-open-session-file session-file) chat-buf)))
          (should (equal (nreverse calls)
                         `((validate ,session-file)
                           (check-dependencies ,project-dir)
                           (setup-session ,project-dir nil)
                           (display ,chat-buf ,input-buf)
                           (ready ,chat-buf "open")
                           (resume ,proc ,chat-buf ,session-file)))))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-open-session-file-checks-dependencies-in-session-cwd ()
  "Opening a remote session file checks dependencies in the recorded cwd."
  (let* ((session-file "/ssh:pi-host:/home/pi/.pi/session.jsonl")
         (session-dir "/ssh:pi-host:/home/pi/project/")
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (checked-dir nil))
    (unwind-protect
        (cl-letf (((symbol-function 'pi-coding-agent--session-file-cwd-or-error)
                   (lambda (_path) session-dir))
                  ((symbol-function 'pi-coding-agent--check-dependencies)
                   (lambda (&optional directory)
                     (setq checked-dir directory)))
                  ((symbol-function 'pi-coding-agent--setup-session)
                   (lambda (_dir &optional _session) chat-buf))
                  ((symbol-function 'pi-coding-agent--display-buffers) #'ignore)
                  ((symbol-function 'pi-coding-agent--session-transition-ready-p)
                   (lambda (&rest _) nil)))
          (let ((default-directory "/tmp/"))
            (should (eq (pi-coding-agent-open-session-file session-file) chat-buf)))
          (should (equal checked-dir session-dir)))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf))))

(ert-deftest pi-coding-agent-test-open-session-file-preserves-multi-hop-session-path ()
  "Opening a multi-hop remote session file keeps the full route through setup."
  (let* ((session-file
          "/ssh:bastion|sudo:root@pi-host:/home/pi/.pi/session.jsonl")
         (session-dir "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (proc (caddr buffers))
         (calls nil))
    (unwind-protect
        (cl-letf (((symbol-function 'pi-coding-agent--session-file-cwd-or-error)
                   (lambda (path)
                     (push (list 'validate path) calls)
                     session-dir))
                  ((symbol-function 'pi-coding-agent--check-dependencies)
                   (lambda (&optional directory)
                     (push (list 'check-dependencies directory) calls)))
                  ((symbol-function 'pi-coding-agent--setup-session)
                   (lambda (dir &optional session)
                     (push (list 'setup-session dir session) calls)
                     chat-buf))
                  ((symbol-function 'pi-coding-agent--display-buffers)
                   (lambda (chat input)
                     (push (list 'display chat input) calls)))
                  ((symbol-function 'pi-coding-agent--session-transition-ready-p)
                   (lambda (chat action)
                     (push (list 'ready chat action) calls)
                     t))
                  ((symbol-function 'pi-coding-agent--resume-selected-session)
                   (lambda (proc chat path)
                     (push (list 'resume proc chat path) calls))))
          (let ((default-directory "/tmp/"))
            (should (eq (pi-coding-agent-open-session-file session-file)
                        chat-buf)))
          (should (equal (nreverse calls)
                         `((validate ,session-file)
                           (check-dependencies ,session-dir)
                           (setup-session ,session-dir nil)
                           (display ,chat-buf ,input-buf)
                           (ready ,chat-buf "open")
                           (resume ,proc ,chat-buf ,session-file)))))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf))))

(ert-deftest pi-coding-agent-test-open-session-file-skips-resume-when-not-ready ()
  "Opening a valid session file displays the session but does not switch if busy."
  (let* ((project-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-not-ready-project-"))
         (session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-not-ready-sessions-"))
         (session-file (expand-file-name "session.jsonl" session-dir))
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (displayed nil)
         (resume-called nil))
    (unwind-protect
        (progn
          (pi-coding-agent-test--write-session-file
           session-file "hello" (directory-file-name project-dir))
          (cl-letf (((symbol-function 'pi-coding-agent--check-dependencies)
                     #'ignore)
                    ((symbol-function 'pi-coding-agent--setup-session)
                     (lambda (_dir &optional _session) chat-buf))
                    ((symbol-function 'pi-coding-agent--display-buffers)
                     (lambda (_chat _input) (setq displayed t)))
                    ((symbol-function 'pi-coding-agent--session-transition-ready-p)
                     (lambda (_chat _action) nil))
                    ((symbol-function 'pi-coding-agent--resume-selected-session)
                     (lambda (&rest _) (setq resume-called t))))
            (should (eq (pi-coding-agent-open-session-file session-file) chat-buf)))
          (should displayed)
          (should-not resume-called))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-open-session-file-rejects-bad-cwd-before-setup ()
  "Rejected session files do not start or display a pi session."
  (let* ((session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-reject-sessions-"))
         (cases `(("missing-cwd" . ,(lambda (path)
                                      (pi-coding-agent-test--write-session-file
                                       path "hello")))
                  ("relative-cwd" . ,(lambda (path)
                                       (pi-coding-agent-test--write-session-file
                                        path "hello" "relative-project")))
                  ("stale-cwd" . ,(lambda (path)
                                    (pi-coding-agent-test--write-session-file
                                     path "hello"
                                     (expand-file-name "deleted-project"
                                                       session-dir)))))))
    (unwind-protect
        (dolist (case cases)
          (let ((session-file (expand-file-name
                               (format "%s.jsonl" (car case))
                               session-dir)))
            (funcall (cdr case) session-file)
            (ert-info ((format "rejected case: %s" (car case)))
              (cl-letf (((symbol-function 'pi-coding-agent--check-dependencies)
                         (lambda ()
                           (ert-fail "Dependencies checked before cwd validation")))
                        ((symbol-function 'pi-coding-agent--setup-session)
                         (lambda (&rest _)
                           (ert-fail "Session setup ran for rejected file")))
                        ((symbol-function 'pi-coding-agent--display-buffers)
                         (lambda (&rest _)
                           (ert-fail "Buffers displayed for rejected file")))
                        ((symbol-function 'pi-coding-agent--resume-selected-session)
                         (lambda (&rest _)
                           (ert-fail "Resume ran for rejected file"))))
                (should-error (pi-coding-agent-open-session-file session-file)
                              :type 'user-error)))))
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-open-session-file-interactive-defaults-to-dired-file ()
  "Interactively opening from Dired defaults to the regular file at point."
  (let* ((session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-dired-sessions-"))
         (project-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-dired-project-"))
         (session-file (expand-file-name "session.jsonl" session-dir))
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (dired-buf nil))
    (unwind-protect
        (progn
          (pi-coding-agent-test--write-session-file
           session-file "hello" (directory-file-name project-dir))
          (setq dired-buf (dired-noselect session-dir))
          (with-current-buffer dired-buf
            (dired-goto-file session-file)
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args)
                         (expand-file-name (or (nth 4 args) "")
                                           (or (nth 1 args) default-directory))))
                      ((symbol-function 'pi-coding-agent--check-dependencies)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'pi-coding-agent--display-buffers)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'pi-coding-agent--resume-selected-session)
                       #'ignore))
              (call-interactively #'pi-coding-agent-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should (equal (nth 1 read-args) session-dir))
          (should (equal (nth 2 read-args) session-file))
          (should (eq (nth 3 read-args) t))
          (should (equal (nth 4 read-args)
                         (file-name-nondirectory session-file))))
      (when dired-buf (kill-buffer dired-buf))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-open-session-file-dired-default-preserves-multi-hop ()
  "Dired's open-session default keeps the full multi-hop TRAMP route."
  (let* ((session-file
          "/ssh:bastion|sudo:root@pi-host:/home/pi/.pi/session.jsonl")
         (project-dir "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (proc (caddr buffers))
         (read-args nil)
         (resumed-path nil))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'derived-mode-p)
                     (lambda (&rest modes) (memq 'dired-mode modes)))
                    ((symbol-function 'dired-get-filename)
                     (lambda (&rest _) session-file))
                    ((symbol-function 'file-regular-p)
                     (lambda (path) (equal path session-file)))
                    ((symbol-function 'read-file-name)
                     (lambda (&rest args)
                       (setq read-args args)
                       (nth 2 args)))
                    ((symbol-function 'pi-coding-agent--session-file-cwd-or-error)
                     (lambda (path)
                       (should (equal path session-file))
                       project-dir))
                    ((symbol-function 'pi-coding-agent--check-dependencies)
                     #'ignore)
                    ((symbol-function 'pi-coding-agent--setup-session)
                     (lambda (dir &optional _session)
                       (should (equal dir project-dir))
                       chat-buf))
                    ((symbol-function 'pi-coding-agent--display-buffers)
                     #'ignore)
                    ((symbol-function 'pi-coding-agent--session-transition-ready-p)
                     (lambda (_chat _action) t))
                    ((symbol-function 'pi-coding-agent--resume-selected-session)
                     (lambda (actual-proc actual-chat path)
                       (should (eq actual-proc proc))
                       (should (eq actual-chat chat-buf))
                       (setq resumed-path path))))
            (call-interactively #'pi-coding-agent-open-session-file)))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf))
    (should (equal (nth 1 read-args)
                   "/ssh:bastion|sudo:root@pi-host:/home/pi/.pi/"))
    (should (equal (nth 2 read-args) session-file))
    (should (equal (nth 4 read-args) "session.jsonl"))
    (should (equal resumed-path session-file))))

(ert-deftest pi-coding-agent-test-open-session-file-interactive-defaults-to-visited-jsonl-file ()
  "Interactively opening from a JSONL buffer defaults to its file."
  (let* ((session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-visited-sessions-"))
         (project-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-visited-project-"))
         (session-file (expand-file-name "session.jsonl" session-dir))
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (pi-coding-agent-test--write-session-file
           session-file "hello" (directory-file-name project-dir))
          (setq file-buf (find-file-noselect session-file))
          (with-current-buffer file-buf
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args)
                         (expand-file-name (or (nth 4 args) "")
                                           (or (nth 1 args) default-directory))))
                      ((symbol-function 'pi-coding-agent--check-dependencies)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'pi-coding-agent--display-buffers)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'pi-coding-agent--resume-selected-session)
                       #'ignore))
              (call-interactively #'pi-coding-agent-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should (equal (nth 1 read-args) session-dir))
          (should (equal (nth 2 read-args) session-file))
          (should (eq (nth 3 read-args) t))
          (should (equal (nth 4 read-args)
                         (file-name-nondirectory session-file))))
      (when file-buf (kill-buffer file-buf))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-open-session-file-rejects-invalid-visited-jsonl-before-setup ()
  "A visited invalid JSONL default is rejected before session setup."
  (let* ((session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-invalid-visited-sessions-"))
         (session-file (expand-file-name "invalid.jsonl" session-dir))
         (read-args nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (pi-coding-agent-test--write-session-file session-file "hello")
          (setq file-buf (find-file-noselect session-file))
          (with-current-buffer file-buf
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args)
                         (expand-file-name (or (nth 4 args) "")
                                           (or (nth 1 args) default-directory))))
                      ((symbol-function 'pi-coding-agent--check-dependencies)
                       (lambda ()
                         (ert-fail "Dependencies checked before cwd validation")))
                      ((symbol-function 'pi-coding-agent--setup-session)
                       (lambda (&rest _)
                         (ert-fail "Session setup ran for rejected file")))
                      ((symbol-function 'pi-coding-agent--display-buffers)
                       (lambda (&rest _)
                         (ert-fail "Buffers displayed for rejected file")))
                      ((symbol-function 'pi-coding-agent--resume-selected-session)
                       (lambda (&rest _)
                         (ert-fail "Resume ran for rejected file"))))
              (should-error (call-interactively #'pi-coding-agent-open-session-file)
                            :type 'user-error)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should (equal (nth 1 read-args) session-dir))
          (should (equal (nth 2 read-args) session-file))
          (should (eq (nth 3 read-args) t))
          (should (equal (nth 4 read-args)
                         (file-name-nondirectory session-file))))
      (when file-buf (kill-buffer file-buf))
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-open-session-file-dired-default-wins-over-visited-jsonl-file ()
  "Dired's regular file at point has priority over `buffer-file-name'."
  (let* ((session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-dired-priority-sessions-"))
         (project-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-dired-priority-project-"))
         (dired-file (expand-file-name "dired.jsonl" session-dir))
         (visited-file (expand-file-name "visited.jsonl" session-dir))
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (dired-buf nil))
    (unwind-protect
        (progn
          (pi-coding-agent-test--write-session-file
           dired-file "dired" (directory-file-name project-dir))
          (pi-coding-agent-test--write-session-file
           visited-file "visited" (directory-file-name project-dir))
          (setq dired-buf (dired-noselect session-dir))
          (with-current-buffer dired-buf
            (setq-local buffer-file-name visited-file)
            (dired-goto-file dired-file)
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args)
                         (expand-file-name (or (nth 4 args) "")
                                           (or (nth 1 args) default-directory))))
                      ((symbol-function 'pi-coding-agent--check-dependencies)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'pi-coding-agent--display-buffers)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'pi-coding-agent--resume-selected-session)
                       #'ignore))
              (call-interactively #'pi-coding-agent-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should (equal (nth 1 read-args) session-dir))
          (should (equal (nth 2 read-args) dired-file))
          (should (eq (nth 3 read-args) t))
          (should (equal (nth 4 read-args)
                         (file-name-nondirectory dired-file))))
      (when dired-buf (kill-buffer dired-buf))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-open-session-file-visited-non-jsonl-has-no-file-default ()
  "Visited non-JSONL buffers do not become session-file defaults."
  (let* ((session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-non-jsonl-sessions-"))
         (project-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-non-jsonl-project-"))
         (text-file (expand-file-name "notes.txt" session-dir))
         (chosen-file (expand-file-name "chosen.jsonl" session-dir))
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (read-buffer-file-name nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (with-temp-file text-file (insert "not a session\n"))
          (pi-coding-agent-test--write-session-file
           chosen-file "chosen" (directory-file-name project-dir))
          (setq file-buf (find-file-noselect text-file))
          (with-current-buffer file-buf
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args
                               read-buffer-file-name buffer-file-name)
                         chosen-file))
                      ((symbol-function 'pi-coding-agent--check-dependencies)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'pi-coding-agent--display-buffers)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'pi-coding-agent--resume-selected-session)
                       #'ignore))
              (call-interactively #'pi-coding-agent-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should-not (nth 1 read-args))
          (should-not (nth 2 read-args))
          (should (eq (nth 3 read-args) t))
          (should-not (nth 4 read-args))
          (should-not read-buffer-file-name))
      (when file-buf (kill-buffer file-buf))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-open-session-file-jsonl-probe-errors-have-no-file-default ()
  "Errors while probing a visited .jsonl file do not abort the prompt."
  (let* ((session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-probe-error-sessions-"))
         (project-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-probe-error-project-"))
         (probed-file (expand-file-name "probed.jsonl" session-dir))
         (chosen-file (expand-file-name "chosen.jsonl" session-dir))
         (original-file-regular-p (symbol-function 'file-regular-p))
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (read-buffer-file-name nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (with-temp-file probed-file (insert "not important\n"))
          (pi-coding-agent-test--write-session-file
           chosen-file "chosen" (directory-file-name project-dir))
          (setq file-buf (find-file-noselect probed-file))
          (with-current-buffer file-buf
            (cl-letf (((symbol-function 'file-regular-p)
                       (lambda (path)
                         (if (equal (expand-file-name path) probed-file)
                             (error "probe failed")
                           (funcall original-file-regular-p path))))
                      ((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args
                               read-buffer-file-name buffer-file-name)
                         chosen-file))
                      ((symbol-function 'pi-coding-agent--check-dependencies)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'pi-coding-agent--display-buffers)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'pi-coding-agent--resume-selected-session)
                       #'ignore))
              (call-interactively #'pi-coding-agent-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should-not (nth 1 read-args))
          (should-not (nth 2 read-args))
          (should (eq (nth 3 read-args) t))
          (should-not (nth 4 read-args))
          (should-not read-buffer-file-name))
      (when file-buf (kill-buffer file-buf))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-open-session-file-unreadable-jsonl-has-no-file-default ()
  "An unreadable .jsonl file is not used as a session-file default."
  (let* ((session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-unreadable-sessions-"))
         (project-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-unreadable-project-"))
         (unreadable-file (expand-file-name "unreadable.jsonl" session-dir))
         (chosen-file (expand-file-name "chosen.jsonl" session-dir))
         (original-file-readable-p (symbol-function 'file-readable-p))
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (read-buffer-file-name nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (with-temp-file unreadable-file (insert "not important\n"))
          (pi-coding-agent-test--write-session-file
           chosen-file "chosen" (directory-file-name project-dir))
          (setq file-buf (find-file-noselect unreadable-file))
          (with-current-buffer file-buf
            (cl-letf (((symbol-function 'file-readable-p)
                       (lambda (path)
                         (and (not (equal (expand-file-name path)
                                          unreadable-file))
                              (funcall original-file-readable-p path))))
                      ((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args
                               read-buffer-file-name buffer-file-name)
                         chosen-file))
                      ((symbol-function 'pi-coding-agent--check-dependencies)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'pi-coding-agent--display-buffers)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'pi-coding-agent--resume-selected-session)
                       #'ignore))
              (call-interactively #'pi-coding-agent-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should-not (nth 1 read-args))
          (should-not (nth 2 read-args))
          (should (eq (nth 3 read-args) t))
          (should-not (nth 4 read-args))
          (should-not read-buffer-file-name))
      (when file-buf (kill-buffer file-buf))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-open-session-file-non-regular-jsonl-has-no-file-default ()
  "A non-regular .jsonl path is not used as a session-file default."
  (let* ((session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-non-regular-sessions-"))
         (project-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-non-regular-project-"))
         (jsonl-dir (expand-file-name "directory.jsonl" session-dir))
         (chosen-file (expand-file-name "chosen.jsonl" session-dir))
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (read-buffer-file-name nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (make-directory jsonl-dir)
          (pi-coding-agent-test--write-session-file
           chosen-file "chosen" (directory-file-name project-dir))
          (setq file-buf (generate-new-buffer " *pi-coding-agent-jsonl-dir*"))
          (with-current-buffer file-buf
            (setq buffer-file-name jsonl-dir)
            (setq default-directory session-dir)
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args
                               read-buffer-file-name buffer-file-name)
                         chosen-file))
                      ((symbol-function 'pi-coding-agent--check-dependencies)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'pi-coding-agent--display-buffers)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'pi-coding-agent--resume-selected-session)
                       #'ignore))
              (call-interactively #'pi-coding-agent-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should-not (nth 1 read-args))
          (should-not (nth 2 read-args))
          (should (eq (nth 3 read-args) t))
          (should-not (nth 4 read-args))
          (should-not read-buffer-file-name))
      (when file-buf (kill-buffer file-buf))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-open-session-file-dired-directory-has-no-file-default ()
  "Interactively opening from Dired does not default to a directory at point."
  (let* ((session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-dired-dir-sessions-"))
         (project-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-dired-dir-project-"))
         (subdir (expand-file-name "subdir" session-dir))
         (visited-file (expand-file-name "visited.jsonl" session-dir))
         (chosen-file (expand-file-name "chosen.jsonl" session-dir))
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (read-buffer-file-name nil)
         (dired-buf nil))
    (unwind-protect
        (progn
          (make-directory subdir)
          (with-temp-file chosen-file (insert "{}\n"))
          (with-temp-file visited-file (insert "{}\n"))
          (setq dired-buf (dired-noselect session-dir))
          (with-current-buffer dired-buf
            (setq-local buffer-file-name visited-file)
            (dired-goto-file subdir)
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args
                               read-buffer-file-name buffer-file-name)
                         chosen-file))
                      ((symbol-function 'pi-coding-agent--session-file-cwd-or-error)
                       (lambda (_path) project-dir))
                      ((symbol-function 'pi-coding-agent--check-dependencies)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'pi-coding-agent--display-buffers)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'pi-coding-agent--resume-selected-session)
                       #'ignore))
              (call-interactively #'pi-coding-agent-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should-not (nth 1 read-args))
          (should-not (nth 2 read-args))
          (should (eq (nth 3 read-args) t))
          (should-not (nth 4 read-args))
          (should-not read-buffer-file-name))
      (when dired-buf (kill-buffer dired-buf))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-open-session-file-does-not-change-dired-pi-coding-agent ()
  "Plain `pi-coding-agent' stays directory-oriented when called from Dired."
  (let* ((session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-plain-dired-"))
         (session-file (expand-file-name "session.jsonl" session-dir))
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (setup-dir nil)
         (dired-buf nil))
    (unwind-protect
        (progn
          (with-temp-file session-file (insert "{}\n"))
          (setq dired-buf (dired-noselect session-dir))
          (with-current-buffer dired-buf
            (dired-goto-file session-file)
            (cl-letf (((symbol-function 'dired-get-filename)
                       (lambda (&rest _)
                         (ert-fail "pi-coding-agent inspected Dired point")))
                      ((symbol-function 'project-current)
                       (lambda (&rest _) nil))
                      ((symbol-function 'pi-coding-agent--check-dependencies)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--setup-session)
                       (lambda (dir &optional _session)
                         (setq setup-dir dir)
                         chat-buf))
                      ((symbol-function 'pi-coding-agent--display-buffers)
                       #'ignore))
              (pi-coding-agent)))
          (should (equal setup-dir session-dir)))
      (when dired-buf (kill-buffer dired-buf))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf)
      (delete-directory session-dir t))))

(ert-deftest pi-coding-agent-test-open-session-file-does-not-change-visited-jsonl-pi-coding-agent ()
  "Plain `pi-coding-agent' stays directory-oriented in a JSONL buffer."
  (let* ((session-dir (pi-coding-agent-test--make-temp-directory
                       "pi-coding-agent-test-open-plain-jsonl-"))
         (session-file (expand-file-name "session.jsonl" session-dir))
         (buffers (pi-coding-agent-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (setup-dir nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (with-temp-file session-file (insert "{}\n"))
          (setq file-buf (find-file-noselect session-file))
          (with-current-buffer file-buf
            (cl-letf (((symbol-function 'pi-coding-agent--read-session-file-name)
                       (lambda ()
                         (ert-fail "pi-coding-agent read a session file")))
                      ((symbol-function 'pi-coding-agent--session-file-cwd-or-error)
                       (lambda (&rest _)
                         (ert-fail "pi-coding-agent validated a session file")))
                      ((symbol-function 'pi-coding-agent--resume-selected-session)
                       (lambda (&rest _)
                         (ert-fail "pi-coding-agent resumed a session file")))
                      ((symbol-function 'project-current)
                       (lambda (&rest _) nil))
                      ((symbol-function 'pi-coding-agent--check-dependencies)
                       #'ignore)
                      ((symbol-function 'pi-coding-agent--setup-session)
                       (lambda (dir &optional _session)
                         (setq setup-dir dir)
                         chat-buf))
                      ((symbol-function 'pi-coding-agent--display-buffers)
                       #'ignore))
              (pi-coding-agent)))
          (should (equal setup-dir session-dir)))
      (when file-buf (kill-buffer file-buf))
      (pi-coding-agent-test--kill-live-buffers input-buf chat-buf)
      (delete-directory session-dir t))))

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

(ert-deftest pi-coding-agent-test-dwim-reuses-saved-chat-buffer-after-write-file ()
  "A saved chat buffer is still reused as the project session."
  (let ((root (pi-coding-agent-test--make-temp-directory
               "pi-coding-agent-test-dwim-write-file-"))
        (file nil)
        (chat nil)
        (input nil)
        (make-backup-files nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
                  ((symbol-function 'pi-coding-agent--display-buffers) #'ignore)
                  ((symbol-function 'pi-coding-agent--check-dependencies) #'ignore))
          (setq chat (pi-coding-agent--setup-session root nil)
                input (buffer-local-value 'pi-coding-agent--input-buffer chat)
                file (pi-coding-agent-test--write-chat-buffer
                      chat "pi-coding-agent-chat-dwim-" "Saved copy\n"))
          (with-temp-buffer
            (setq default-directory root)
            (pi-coding-agent))
          (should (eq (pi-coding-agent--find-session root) chat))
          (should (eq (buffer-local-value 'pi-coding-agent--input-buffer chat)
                      input))
          (with-current-buffer chat
            (should (equal (pi-coding-agent--chat-session-buffer-name)
                           (pi-coding-agent-test--chat-buffer-name root)))
            (should (equal (pi-coding-agent--session-directory) root))
            (should (equal buffer-file-name file))))
      (pi-coding-agent-test--kill-live-buffers input chat)
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

(ert-deftest pi-coding-agent-test-setup-session-shows-startup-error-from-initial-state-request ()
  "Initial startup failure should be rendered into the chat buffer."
  (let ((root (pi-coding-agent-test--make-temp-directory
               "pi-coding-agent-test-startup-error-"))
        (proc (start-process "pi-coding-agent-startup-error" nil "cat"))
        (chat nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'pi-coding-agent--start-process) (lambda (_) proc))
                  ((symbol-function 'pi-coding-agent--fetch-commands) (lambda (&rest _) nil))
                  ((symbol-function 'pi-coding-agent--rpc-async)
                   (lambda (_proc cmd callback)
                     (should (equal (plist-get cmd :type) "get_state"))
                     (funcall callback
                              '(:type "response"
                                :command "get_state"
                                :success :false
                                :error "Process exited: exited abnormally with code 1"
                                :stderr "InvalidArgumentError: Invalid URL protocol")))))
          (setq chat (pi-coding-agent--setup-session root nil))
          (should (buffer-live-p chat))
          (with-current-buffer chat
            (should (string-match-p "failed to start" (buffer-string)))
            (should (string-match-p "Invalid URL protocol" (buffer-string)))))
      (when (process-live-p proc)
        (delete-process proc))
      (pi-coding-agent-test--kill-session-buffers root))))

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

(ert-deftest pi-coding-agent-test-named-session-reuses-saved-chat-buffer-after-write-file ()
  "Saving a named session keeps it distinct from the default session."
  (let ((root (pi-coding-agent-test--make-temp-directory
               "pi-coding-agent-test-named-write-file-"))
        (file nil)
        (default-chat nil)
        (default-input nil)
        (named-chat nil)
        (named-input nil)
        (make-backup-files nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
                  ((symbol-function 'pi-coding-agent--display-buffers) #'ignore)
                  ((symbol-function 'pi-coding-agent--check-dependencies) #'ignore))
          (setq default-chat (pi-coding-agent--setup-session root nil)
                default-input (buffer-local-value 'pi-coding-agent--input-buffer default-chat)
                named-chat (pi-coding-agent--setup-session root "feature")
                named-input (buffer-local-value 'pi-coding-agent--input-buffer named-chat)
                file (pi-coding-agent-test--write-chat-buffer
                      named-chat "pi-coding-agent-chat-named-"
                      "Named session archive\n"))
          (with-temp-buffer
            (setq default-directory root)
            (pi-coding-agent "feature"))
          (should (eq (pi-coding-agent--find-session root) default-chat))
          (should (eq (pi-coding-agent--find-session root "feature") named-chat))
          (should-not (eq default-chat named-chat))
          (with-current-buffer named-chat
            (should (equal (pi-coding-agent--chat-session-buffer-name)
                           (pi-coding-agent-test--chat-buffer-name root "feature")))
            (should (equal (pi-coding-agent--session-directory) root))
            (should (equal buffer-file-name file))))
      (pi-coding-agent-test--kill-live-buffers
       named-input named-chat default-input default-chat)
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

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

(ert-deftest pi-coding-agent-test-toggle-existing-session-does-not-check-dependencies ()
  "Toggling an existing session is a UI action, not a process launch."
  (let* ((root (pi-coding-agent-test--make-temp-directory
                "pi-coding-agent-test-toggle-no-deps-"))
         (chat (generate-new-buffer (pi-coding-agent-test--chat-buffer-name root)))
         (input (generate-new-buffer (pi-coding-agent-test--input-buffer-name root)))
         (displayed nil))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (pi-coding-agent-chat-mode)
            (pi-coding-agent--set-chat-session-identity root)
            (pi-coding-agent--set-input-buffer input))
          (with-current-buffer input
            (pi-coding-agent-input-mode)
            (pi-coding-agent--set-chat-buffer chat))
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                    ((symbol-function 'pi-coding-agent--check-dependencies)
                     (lambda (&rest _)
                       (ert-fail "toggle checked process dependencies")))
                    ((symbol-function 'pi-coding-agent--display-buffers)
                     (lambda (_chat _input)
                       (setq displayed t))))
            (with-temp-buffer
              (setq default-directory root)
              (pi-coding-agent-toggle)))
          (should displayed))
      (pi-coding-agent-test--kill-live-buffers input chat)
      (delete-directory root t))))

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

(ert-deftest pi-coding-agent-test-project-buffers-finds-saved-session-after-write-file ()
  "`pi-coding-agent-project-buffers' still finds a saved chat buffer."
  (let ((root (pi-coding-agent-test--make-temp-directory
               "pi-coding-agent-test-projbuf-write-file-"))
        (file nil)
        (chat nil)
        (input nil)
        (make-backup-files nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
                  ((symbol-function 'pi-coding-agent--display-buffers) #'ignore)
                  ((symbol-function 'pi-coding-agent--check-dependencies) #'ignore))
          (setq chat (pi-coding-agent--setup-session root nil)
                input (buffer-local-value 'pi-coding-agent--input-buffer chat)
                file (pi-coding-agent-test--write-chat-buffer
                      chat "pi-coding-agent-chat-projbuf-"))
          (with-temp-buffer
            (setq default-directory root)
            (should (equal (pi-coding-agent-project-buffers)
                           (list chat)))))
      (pi-coding-agent-test--kill-live-buffers input chat)
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

(ert-deftest pi-coding-agent-test-project-root-session-reused-after-write-file-from-subdir ()
  "Saving from a subdir keeps the project-root session identity."
  (let* ((root (pi-coding-agent-test--make-temp-directory
                "pi-coding-agent-test-write-file-project-root-"))
         (nested (expand-file-name "src/nested/" root))
         (sibling (expand-file-name "docs/" root))
         (file nil)
         (chat nil)
         (input nil)
         (make-backup-files nil))
    (make-directory nested t)
    (make-directory sibling t)
    (unwind-protect
        (cl-letf (((symbol-function 'project-current)
                   (lambda (&rest _) 'mock-project))
                  ((symbol-function 'project-root)
                   (lambda (_project) root))
                  ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
                  ((symbol-function 'pi-coding-agent--check-dependencies) #'ignore)
                  ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
          (with-temp-buffer
            (setq default-directory nested)
            (pi-coding-agent)
            (setq chat (pi-coding-agent--find-session root)
                  input (get-buffer (pi-coding-agent-test--input-buffer-name root))))
          (setq file (pi-coding-agent-test--write-chat-buffer
                      chat "pi-coding-agent-chat-project-root-"
                      "Saved from nested dir\n"))
          (with-current-buffer chat
            (should (equal (pi-coding-agent--chat-session-buffer-name)
                           (pi-coding-agent-test--chat-buffer-name root)))
            (should (equal (pi-coding-agent--session-directory) root))
            (should (equal buffer-file-name file)))
          (with-temp-buffer
            (setq default-directory sibling)
            (pi-coding-agent)
            (should (equal (pi-coding-agent-project-buffers)
                           (list chat))))
          (should (eq (pi-coding-agent--find-session root) chat))
          (should (eq (get-buffer (pi-coding-agent-test--input-buffer-name root))
                      input)))
      (pi-coding-agent-test--kill-live-buffers input chat)
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

(ert-deftest pi-coding-agent-test-project-buffers-excludes-other-projects ()
  "`pi-coding-agent-project-buffers' returns nil for a different project."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-projbuf-a/"
    (let ((default-directory "/tmp/pi-coding-agent-test-projbuf-b/"))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (should (null (pi-coding-agent-project-buffers)))))))

(ert-deftest pi-coding-agent-test-toggle-finds-saved-session-after-write-file ()
  "`pi-coding-agent-toggle' still finds a saved chat buffer."
  (let ((root (pi-coding-agent-test--make-temp-directory
               "pi-coding-agent-test-toggle-write-file-"))
        (file nil)
        (chat nil)
        (input nil)
        (displayed-chat nil)
        (displayed-input nil)
        (make-backup-files nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
                  ((symbol-function 'pi-coding-agent--check-dependencies) #'ignore)
                  ((symbol-function 'pi-coding-agent--display-buffers)
                   (lambda (chat-buf input-buf)
                     (setq displayed-chat chat-buf
                           displayed-input input-buf))))
          (setq chat (pi-coding-agent--setup-session root nil)
                input (buffer-local-value 'pi-coding-agent--input-buffer chat)
                file (pi-coding-agent-test--write-chat-buffer
                      chat "pi-coding-agent-chat-toggle-"))
          (with-temp-buffer
            (setq default-directory root)
            (pi-coding-agent-toggle))
          (should (eq displayed-chat chat))
          (should (eq displayed-input input)))
      (pi-coding-agent-test--kill-live-buffers input chat)
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

(ert-deftest pi-coding-agent-test-toggle-hides-and-shows-saved-session-after-write-file ()
  "`pi-coding-agent-toggle' hides and restores a saved session."
  (let ((root (pi-coding-agent-test--make-temp-directory
               "pi-coding-agent-test-toggle-write-file-live-"))
        (file nil)
        (chat nil)
        (input nil)
        (make-backup-files nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
                  ((symbol-function 'pi-coding-agent--check-dependencies) #'ignore))
          (delete-other-windows)
          (switch-to-buffer "*scratch*")
          (setq default-directory root)
          (pi-coding-agent)
          (setq chat (pi-coding-agent--find-session root)
                input (get-buffer (pi-coding-agent-test--input-buffer-name root)))
          (setq file (pi-coding-agent-test--write-chat-buffer
                      chat "pi-coding-agent-chat-toggle-live-"))
          (with-current-buffer chat
            (should (equal (pi-coding-agent--chat-session-buffer-name)
                           (pi-coding-agent-test--chat-buffer-name root)))
            (should (equal (pi-coding-agent--session-directory) root))
            (should (equal buffer-file-name file)))
          (should (get-buffer-window-list chat nil t))
          (should (get-buffer-window-list input nil t))
          (let ((non-pi (get-buffer-create "*pi-coding-agent-test-toggle-non-pi*")))
            (with-current-buffer non-pi
              (setq default-directory root))
            (switch-to-buffer non-pi)
            (pi-coding-agent-toggle)
            (should-not (get-buffer-window-list chat nil t))
            (should-not (get-buffer-window-list input nil t))
            (pi-coding-agent-toggle)
            (should (get-buffer-window-list chat nil t))
            (should (get-buffer-window-list input nil t))
            (with-current-buffer chat
              (should (equal buffer-file-name file)))))
      (pi-coding-agent-test--kill-live-buffers input chat)
      (ignore-errors (kill-buffer "*pi-coding-agent-test-toggle-non-pi*"))
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t))
      (delete-other-windows))))

(ert-deftest pi-coding-agent-test-toggle-no-session-errors ()
  "`pi-coding-agent-toggle' signals `user-error' when no session exists."
  (let ((default-directory "/tmp/pi-coding-agent-test-no-session/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--check-dependencies) #'ignore))
      (should-error (pi-coding-agent-toggle) :type 'user-error))))

(ert-deftest pi-coding-agent-test-toggle-shows-in-current-frame-when-only-visible-elsewhere ()
  "`pi-coding-agent-toggle' should show in current frame when hidden there."
  (let ((root "/tmp/pi-coding-agent-test-toggle-other-frame/")
        (display-called nil)
        (hide-called nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
              ((symbol-function 'pi-coding-agent--check-dependencies) #'ignore))
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
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
              ((symbol-function 'pi-coding-agent--check-dependencies) #'ignore))
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
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
              ((symbol-function 'pi-coding-agent--check-dependencies) #'ignore))
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

(ert-deftest pi-coding-agent-test-transient-warning-explains-built-in-upgrade ()
  "Loading the menu with an old transient explains how to upgrade it."
  (let* ((expression
          (mapconcat
           #'identity
           '("(progn"
             "  (require 'cl-lib)"
             "  (require 'transient)"
             "  (setq transient-version \"0.7.2.2\")"
             "  (let (captured)"
             "    (cl-letf (((symbol-function 'display-warning)"
             "               (lambda (_type message &rest _)"
             "                 (setq captured message))))"
             "      (load (expand-file-name \"pi-coding-agent-menu.el\""
             "                              (file-name-directory"
             "                               (locate-library \"pi-coding-agent\")))"
             "            nil t))"
             "    (prin1 captured)))")
           " "))
         (result (pi-coding-agent-test--read-batch-emacs-result expression)))
    (should (string-match-p "upgrade transient from MELPA" result))
    (should (string-match-p "package-install-upgrade-built-in" result))))

(ert-deftest pi-coding-agent-test-transient-version-check-handles-built-in-snapshot-format ()
  "Loading the menu tolerates built-in transient version strings with a prefix."
  (let* ((expression
          (mapconcat
           #'identity
           '("(progn"
             "  (require 'cl-lib)"
             "  (require 'transient)"
             "  (setq transient-version \"v0.12.0-15-gfe5214e6-builtin\")"
             "  (let (captured err)"
             "    (cl-letf (((symbol-function 'display-warning)"
             "               (lambda (_type message &rest _)"
             "                 (setq captured message))))"
             "      (condition-case load-err"
             "          (load (expand-file-name \"pi-coding-agent-menu.el\""
             "                                  (file-name-directory"
             "                                   (locate-library \"pi-coding-agent\")))"
             "                nil t)"
             "        (error (setq err (error-message-string load-err)))))"
             "    (prin1 (list :warning captured :error err))))")
           " "))
         (result (pi-coding-agent-test--read-batch-emacs-result expression)))
    (should-not (plist-get result :error))
    (should-not (plist-get result :warning))))

(ert-deftest pi-coding-agent-test-md-ts-mode-package-load-leaves-global-markdown-settings-alone ()
  "Loading `md-ts-mode' keeps global Markdown associations opt-in."
  (let ((result (pi-coding-agent-test--markdown-load-state 'md-ts-mode)))
    (should (eq t (plist-get result :auto-unchanged)))
    (should (eq t (plist-get result :major-remap-unchanged)))
    (should (eq t (plist-get result :treesit-remap-unchanged)))
    (should (eq t (plist-get result :md-mode-defined)))
    (should (eq t (plist-get result :md-mode-maybe-defined)))
    (should (equal (plist-get result :before-md-association)
                   (plist-get result :after-md-association)))
    (should (equal (plist-get result :before-major-markdown-remap)
                   (plist-get result :after-major-markdown-remap)))
    (should (equal (plist-get result :before-treesit-markdown-remap)
                   (plist-get result :after-treesit-markdown-remap)))))

(ert-deftest pi-coding-agent-test-package-load-leaves-global-markdown-settings-alone ()
  "Loading `pi-coding-agent' does not change global Markdown mode settings."
  (let ((result (pi-coding-agent-test--markdown-load-state 'pi-coding-agent)))
    (should (eq t (plist-get result :auto-unchanged)))
    (should (eq t (plist-get result :major-remap-unchanged)))
    (should (eq t (plist-get result :treesit-remap-unchanged)))
    (should (equal (plist-get result :before-md-association)
                   (plist-get result :after-md-association)))
    (should (equal (plist-get result :before-major-markdown-remap)
                   (plist-get result :after-major-markdown-remap)))
    (should (equal (plist-get result :before-treesit-markdown-remap)
                   (plist-get result :after-treesit-markdown-remap)))))

(provide 'pi-coding-agent-test)
;;; pi-coding-agent-test.el ends here
