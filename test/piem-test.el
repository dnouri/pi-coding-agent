;;; piem-test.el --- Tests for piem -*- lexical-binding: t; -*-

;;; Commentary:

;; Entry-point and cross-module integration tests for piem.

;;; Code:

(require 'dired)
(require 'ert)
(require 'piem)
(require 'piem-test-common)

;;; Shared Test Helpers

(defun piem-test--make-open-session-command-buffers (&optional process)
  "Return linked chat/input/process fixtures for open-session-file tests.
PROCESS defaults to a harmless pipe process because pi buffer cleanup expects
`piem--process' to be either nil or a process object.  These tests
still mock the RPC boundary, so the process is never used for I/O."
  (let ((chat-buf (generate-new-buffer " *piem-open-session-chat*"))
        (input-buf (generate-new-buffer " *piem-open-session-input*"))
        (proc (or process
                  (make-pipe-process :name "piem-open-session-test"
                                     :buffer nil
                                     :noquery t))))
    (with-current-buffer chat-buf
      (piem-chat-mode)
      (piem--set-input-buffer input-buf)
      (piem--set-process proc))
    (with-current-buffer input-buf
      (piem-input-mode)
      (piem--set-chat-buffer chat-buf))
    (list chat-buf input-buf proc)))

(ert-deftest piem-test-backend-spec-builds-fake-launch-config ()
  "Shared test helper builds fake backend launch data from a scenario name."
  (let* ((spec (piem-test-backend-spec 'fake "prompt-lifecycle"
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
                   (piem-test-python-executable)))
    (should (equal (cadr executable)
                   piem-test-fake-pi-script))))

(ert-deftest piem-test-backend-spec-builds-real-launch-config ()
  "Shared test helper preserves the configured real backend launch command."
  (let ((piem-executable '("pi" "rpc"))
        (piem-extra-args '("--model" "fake")))
    (let ((spec (piem-test-backend-spec 'real "prompt-lifecycle")))
      (should (eq (plist-get spec :name) 'real))
      (should (equal (plist-get spec :label) "real"))
      (should (equal (plist-get spec :executable) '("pi" "rpc")))
      (should (equal (plist-get spec :extra-args) '("--model" "fake")))
      (should-not (plist-member spec :scenario)))))

(ert-deftest piem-test-backend-spec-rejects-unknown-backend ()
  "Shared backend helper should fail loudly for unsupported backends."
  (should-error
   (piem-test-backend-spec 'bogus "prompt-lifecycle")))

;;; Main Entry Point

(ert-deftest piem-test-piem-creates-chat-buffer ()
  "M-x piem creates a chat buffer."
  (piem-test-with-mock-session "/tmp/piem-test-main/"
    (should (get-buffer "*piem-chat:/tmp/piem-test-main/*"))))

(ert-deftest piem-test-piem-creates-input-buffer ()
  "M-x piem creates an input buffer."
  (piem-test-with-mock-session "/tmp/piem-test-main2/"
    (should (get-buffer "*piem-input:/tmp/piem-test-main2/*"))))

(ert-deftest piem-test-piem-sets-major-modes ()
  "M-x piem sets correct major modes on buffers."
  (piem-test-with-mock-session "/tmp/piem-test-modes/"
    (with-current-buffer "*piem-chat:/tmp/piem-test-modes/*"
      (should (derived-mode-p 'piem-chat-mode)))
    (with-current-buffer "*piem-input:/tmp/piem-test-modes/*"
      (should (derived-mode-p 'piem-input-mode)))))

(ert-deftest piem-test-open-session-file-validates-sets-up-displays-and-resumes ()
  "Opening a valid session file uses the normal live-session path."
  (let* ((project-dir (piem-test--make-temp-directory
                       "piem-test-open-project-"))
         (session-dir (piem-test--make-temp-directory
                       "piem-test-open-sessions-"))
         (session-file (expand-file-name "session.jsonl" session-dir))
         (original-validator (symbol-function
                              'piem--session-file-cwd-or-error))
         (buffers (piem-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (proc (caddr buffers))
         (calls nil))
    (unwind-protect
        (progn
          (piem-test--write-session-file
           session-file "hello" (directory-file-name project-dir))
          (cl-letf (((symbol-function 'piem--session-file-cwd-or-error)
                     (lambda (path)
                       (push (list 'validate path) calls)
                       (funcall original-validator path)))
                    ((symbol-function 'piem--check-dependencies)
                     (lambda (&optional directory)
                       (push (list 'check-dependencies directory) calls)))
                    ((symbol-function 'piem--setup-session)
                     (lambda (dir &optional session)
                       (push (list 'setup-session dir session) calls)
                       chat-buf))
                    ((symbol-function 'piem--display-buffers)
                     (lambda (chat input &optional _chat-only)
                       (push (list 'display chat input) calls)))
                    ((symbol-function 'piem--session-transition-ready-p)
                     (lambda (chat action)
                       (push (list 'ready chat action) calls)
                       t))
                    ((symbol-function 'piem--resume-selected-session)
                     (lambda (proc chat path)
                       (push (list 'resume proc chat path) calls))))
            (should (eq (piem-open-session-file session-file) chat-buf)))
          (should (equal (nreverse calls)
                         `((validate ,session-file)
                           (check-dependencies ,project-dir)
                           (setup-session ,project-dir nil)
                           (display ,chat-buf ,input-buf)
                           (ready ,chat-buf "open")
                           (resume ,proc ,chat-buf ,session-file)))))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-open-session-file-checks-dependencies-in-session-cwd ()
  "Opening a remote session file checks dependencies in the recorded cwd."
  (let* ((session-file "/ssh:pi-host:/home/pi/.pi/session.jsonl")
         (session-dir "/ssh:pi-host:/home/pi/project/")
         (buffers (piem-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (checked-dir nil))
    (unwind-protect
        (cl-letf (((symbol-function 'piem--session-file-cwd-or-error)
                   (lambda (_path) session-dir))
                  ((symbol-function 'piem--check-dependencies)
                   (lambda (&optional directory)
                     (setq checked-dir directory)))
                  ((symbol-function 'piem--setup-session)
                   (lambda (_dir &optional _session) chat-buf))
                  ((symbol-function 'piem--display-buffers) #'ignore)
                  ((symbol-function 'piem--session-transition-ready-p)
                   (lambda (&rest _) nil)))
          (let ((default-directory "/tmp/"))
            (should (eq (piem-open-session-file session-file) chat-buf)))
          (should (equal checked-dir session-dir)))
      (piem-test--kill-live-buffers input-buf chat-buf))))

(ert-deftest piem-test-open-session-file-preserves-multi-hop-session-path ()
  "Opening a multi-hop remote session file keeps the full route through setup."
  (let* ((session-file
          "/ssh:bastion|sudo:root@pi-host:/home/pi/.pi/session.jsonl")
         (session-dir "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
         (buffers (piem-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (proc (caddr buffers))
         (calls nil))
    (unwind-protect
        (cl-letf (((symbol-function 'piem--session-file-cwd-or-error)
                   (lambda (path)
                     (push (list 'validate path) calls)
                     session-dir))
                  ((symbol-function 'piem--check-dependencies)
                   (lambda (&optional directory)
                     (push (list 'check-dependencies directory) calls)))
                  ((symbol-function 'piem--setup-session)
                   (lambda (dir &optional session)
                     (push (list 'setup-session dir session) calls)
                     chat-buf))
                  ((symbol-function 'piem--display-buffers)
                   (lambda (chat input &optional _chat-only)
                     (push (list 'display chat input) calls)))
                  ((symbol-function 'piem--session-transition-ready-p)
                   (lambda (chat action)
                     (push (list 'ready chat action) calls)
                     t))
                  ((symbol-function 'piem--resume-selected-session)
                   (lambda (proc chat path)
                     (push (list 'resume proc chat path) calls))))
          (let ((default-directory "/tmp/"))
            (should (eq (piem-open-session-file session-file)
                        chat-buf)))
          (should (equal (nreverse calls)
                         `((validate ,session-file)
                           (check-dependencies ,session-dir)
                           (setup-session ,session-dir nil)
                           (display ,chat-buf ,input-buf)
                           (ready ,chat-buf "open")
                           (resume ,proc ,chat-buf ,session-file)))))
      (piem-test--kill-live-buffers input-buf chat-buf))))

(ert-deftest piem-test-open-session-file-skips-resume-when-not-ready ()
  "Opening a valid session file displays the session but does not switch if busy."
  (let* ((project-dir (piem-test--make-temp-directory
                       "piem-test-open-not-ready-project-"))
         (session-dir (piem-test--make-temp-directory
                       "piem-test-open-not-ready-sessions-"))
         (session-file (expand-file-name "session.jsonl" session-dir))
         (buffers (piem-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (displayed nil)
         (resume-called nil))
    (unwind-protect
        (progn
          (piem-test--write-session-file
           session-file "hello" (directory-file-name project-dir))
          (cl-letf (((symbol-function 'piem--check-dependencies)
                     #'ignore)
                    ((symbol-function 'piem--setup-session)
                     (lambda (_dir &optional _session) chat-buf))
                    ((symbol-function 'piem--display-buffers)
                     (lambda (_chat _input &optional _chat-only) (setq displayed t)))
                    ((symbol-function 'piem--session-transition-ready-p)
                     (lambda (_chat _action) nil))
                    ((symbol-function 'piem--resume-selected-session)
                     (lambda (&rest _) (setq resume-called t))))
            (should (eq (piem-open-session-file session-file) chat-buf)))
          (should displayed)
          (should-not resume-called))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-open-session-file-rejects-bad-cwd-before-setup ()
  "Rejected session files do not start or display a pi session."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-open-reject-sessions-"))
         (cases `(("missing-cwd" . ,(lambda (path)
                                      (piem-test--write-session-file
                                       path "hello")))
                  ("relative-cwd" . ,(lambda (path)
                                       (piem-test--write-session-file
                                        path "hello" "relative-project")))
                  ("stale-cwd" . ,(lambda (path)
                                    (piem-test--write-session-file
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
              (cl-letf (((symbol-function 'piem--check-dependencies)
                         (lambda ()
                           (ert-fail "Dependencies checked before cwd validation")))
                        ((symbol-function 'piem--setup-session)
                         (lambda (&rest _)
                           (ert-fail "Session setup ran for rejected file")))
                        ((symbol-function 'piem--display-buffers)
                         (lambda (&rest _)
                           (ert-fail "Buffers displayed for rejected file")))
                        ((symbol-function 'piem--resume-selected-session)
                         (lambda (&rest _)
                           (ert-fail "Resume ran for rejected file"))))
                (should-error (piem-open-session-file session-file)
                              :type 'user-error)))))
      (delete-directory session-dir t))))

(ert-deftest piem-test-open-session-file-interactive-defaults-to-dired-file ()
  "Interactively opening from Dired defaults to the regular file at point."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-open-dired-sessions-"))
         (project-dir (piem-test--make-temp-directory
                       "piem-test-open-dired-project-"))
         (session-file (expand-file-name "session.jsonl" session-dir))
         (buffers (piem-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (dired-buf nil))
    (unwind-protect
        (progn
          (piem-test--write-session-file
           session-file "hello" (directory-file-name project-dir))
          (setq dired-buf (dired-noselect session-dir))
          (with-current-buffer dired-buf
            (dired-goto-file session-file)
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args)
                         (expand-file-name (or (nth 4 args) "")
                                           (or (nth 1 args) default-directory))))
                      ((symbol-function 'piem--check-dependencies)
                       #'ignore)
                      ((symbol-function 'piem--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'piem--display-buffers)
                       #'ignore)
                      ((symbol-function 'piem--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'piem--resume-selected-session)
                       #'ignore))
              (call-interactively #'piem-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should (equal (nth 1 read-args) session-dir))
          (should (equal (nth 2 read-args) session-file))
          (should (eq (nth 3 read-args) t))
          (should (equal (nth 4 read-args)
                         (file-name-nondirectory session-file))))
      (when dired-buf (kill-buffer dired-buf))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-open-session-file-dired-default-preserves-multi-hop ()
  "Dired's open-session default keeps the full multi-hop TRAMP route."
  (let* ((session-file
          "/ssh:bastion|sudo:root@pi-host:/home/pi/.pi/session.jsonl")
         (project-dir "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
         (buffers (piem-test--make-open-session-command-buffers))
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
                    ((symbol-function 'piem--session-file-cwd-or-error)
                     (lambda (path)
                       (should (equal path session-file))
                       project-dir))
                    ((symbol-function 'piem--check-dependencies)
                     #'ignore)
                    ((symbol-function 'piem--setup-session)
                     (lambda (dir &optional _session)
                       (should (equal dir project-dir))
                       chat-buf))
                    ((symbol-function 'piem--display-buffers)
                     #'ignore)
                    ((symbol-function 'piem--session-transition-ready-p)
                     (lambda (_chat _action) t))
                    ((symbol-function 'piem--resume-selected-session)
                     (lambda (actual-proc actual-chat path)
                       (should (eq actual-proc proc))
                       (should (eq actual-chat chat-buf))
                       (setq resumed-path path))))
            (call-interactively #'piem-open-session-file)))
      (piem-test--kill-live-buffers input-buf chat-buf))
    (should (equal (nth 1 read-args)
                   "/ssh:bastion|sudo:root@pi-host:/home/pi/.pi/"))
    (should (equal (nth 2 read-args) session-file))
    (should (equal (nth 4 read-args) "session.jsonl"))
    (should (equal resumed-path session-file))))

(ert-deftest piem-test-open-session-file-interactive-defaults-to-visited-jsonl-file ()
  "Interactively opening from a JSONL buffer defaults to its file."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-open-visited-sessions-"))
         (project-dir (piem-test--make-temp-directory
                       "piem-test-open-visited-project-"))
         (session-file (expand-file-name "session.jsonl" session-dir))
         (buffers (piem-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (piem-test--write-session-file
           session-file "hello" (directory-file-name project-dir))
          (setq file-buf (find-file-noselect session-file))
          (with-current-buffer file-buf
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args)
                         (expand-file-name (or (nth 4 args) "")
                                           (or (nth 1 args) default-directory))))
                      ((symbol-function 'piem--check-dependencies)
                       #'ignore)
                      ((symbol-function 'piem--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'piem--display-buffers)
                       #'ignore)
                      ((symbol-function 'piem--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'piem--resume-selected-session)
                       #'ignore))
              (call-interactively #'piem-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should (equal (nth 1 read-args) session-dir))
          (should (equal (nth 2 read-args) session-file))
          (should (eq (nth 3 read-args) t))
          (should (equal (nth 4 read-args)
                         (file-name-nondirectory session-file))))
      (when file-buf (kill-buffer file-buf))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-open-session-file-rejects-invalid-visited-jsonl-before-setup ()
  "A visited invalid JSONL default is rejected before session setup."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-open-invalid-visited-sessions-"))
         (session-file (expand-file-name "invalid.jsonl" session-dir))
         (read-args nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (piem-test--write-session-file session-file "hello")
          (setq file-buf (find-file-noselect session-file))
          (with-current-buffer file-buf
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args)
                         (expand-file-name (or (nth 4 args) "")
                                           (or (nth 1 args) default-directory))))
                      ((symbol-function 'piem--check-dependencies)
                       (lambda ()
                         (ert-fail "Dependencies checked before cwd validation")))
                      ((symbol-function 'piem--setup-session)
                       (lambda (&rest _)
                         (ert-fail "Session setup ran for rejected file")))
                      ((symbol-function 'piem--display-buffers)
                       (lambda (&rest _)
                         (ert-fail "Buffers displayed for rejected file")))
                      ((symbol-function 'piem--resume-selected-session)
                       (lambda (&rest _)
                         (ert-fail "Resume ran for rejected file"))))
              (should-error (call-interactively #'piem-open-session-file)
                            :type 'user-error)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should (equal (nth 1 read-args) session-dir))
          (should (equal (nth 2 read-args) session-file))
          (should (eq (nth 3 read-args) t))
          (should (equal (nth 4 read-args)
                         (file-name-nondirectory session-file))))
      (when file-buf (kill-buffer file-buf))
      (delete-directory session-dir t))))

(ert-deftest piem-test-open-session-file-dired-default-wins-over-visited-jsonl-file ()
  "Dired's regular file at point has priority over `buffer-file-name'."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-open-dired-priority-sessions-"))
         (project-dir (piem-test--make-temp-directory
                       "piem-test-open-dired-priority-project-"))
         (dired-file (expand-file-name "dired.jsonl" session-dir))
         (visited-file (expand-file-name "visited.jsonl" session-dir))
         (buffers (piem-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (dired-buf nil))
    (unwind-protect
        (progn
          (piem-test--write-session-file
           dired-file "dired" (directory-file-name project-dir))
          (piem-test--write-session-file
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
                      ((symbol-function 'piem--check-dependencies)
                       #'ignore)
                      ((symbol-function 'piem--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'piem--display-buffers)
                       #'ignore)
                      ((symbol-function 'piem--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'piem--resume-selected-session)
                       #'ignore))
              (call-interactively #'piem-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should (equal (nth 1 read-args) session-dir))
          (should (equal (nth 2 read-args) dired-file))
          (should (eq (nth 3 read-args) t))
          (should (equal (nth 4 read-args)
                         (file-name-nondirectory dired-file))))
      (when dired-buf (kill-buffer dired-buf))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-open-session-file-visited-non-jsonl-has-no-file-default ()
  "Visited non-JSONL buffers do not become session-file defaults."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-open-non-jsonl-sessions-"))
         (project-dir (piem-test--make-temp-directory
                       "piem-test-open-non-jsonl-project-"))
         (text-file (expand-file-name "notes.txt" session-dir))
         (chosen-file (expand-file-name "chosen.jsonl" session-dir))
         (buffers (piem-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (read-buffer-file-name nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (with-temp-file text-file (insert "not a session\n"))
          (piem-test--write-session-file
           chosen-file "chosen" (directory-file-name project-dir))
          (setq file-buf (find-file-noselect text-file))
          (with-current-buffer file-buf
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args
                               read-buffer-file-name buffer-file-name)
                         chosen-file))
                      ((symbol-function 'piem--check-dependencies)
                       #'ignore)
                      ((symbol-function 'piem--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'piem--display-buffers)
                       #'ignore)
                      ((symbol-function 'piem--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'piem--resume-selected-session)
                       #'ignore))
              (call-interactively #'piem-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should-not (nth 1 read-args))
          (should-not (nth 2 read-args))
          (should (eq (nth 3 read-args) t))
          (should-not (nth 4 read-args))
          (should-not read-buffer-file-name))
      (when file-buf (kill-buffer file-buf))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-open-session-file-jsonl-probe-errors-have-no-file-default ()
  "Errors while probing a visited .jsonl file do not abort the prompt."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-open-probe-error-sessions-"))
         (project-dir (piem-test--make-temp-directory
                       "piem-test-open-probe-error-project-"))
         (probed-file (expand-file-name "probed.jsonl" session-dir))
         (chosen-file (expand-file-name "chosen.jsonl" session-dir))
         (original-file-regular-p (symbol-function 'file-regular-p))
         (buffers (piem-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (read-buffer-file-name nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (with-temp-file probed-file (insert "not important\n"))
          (piem-test--write-session-file
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
                      ((symbol-function 'piem--check-dependencies)
                       #'ignore)
                      ((symbol-function 'piem--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'piem--display-buffers)
                       #'ignore)
                      ((symbol-function 'piem--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'piem--resume-selected-session)
                       #'ignore))
              (call-interactively #'piem-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should-not (nth 1 read-args))
          (should-not (nth 2 read-args))
          (should (eq (nth 3 read-args) t))
          (should-not (nth 4 read-args))
          (should-not read-buffer-file-name))
      (when file-buf (kill-buffer file-buf))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-open-session-file-unreadable-jsonl-has-no-file-default ()
  "An unreadable .jsonl file is not used as a session-file default."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-open-unreadable-sessions-"))
         (project-dir (piem-test--make-temp-directory
                       "piem-test-open-unreadable-project-"))
         (unreadable-file (expand-file-name "unreadable.jsonl" session-dir))
         (chosen-file (expand-file-name "chosen.jsonl" session-dir))
         (original-file-readable-p (symbol-function 'file-readable-p))
         (buffers (piem-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (read-buffer-file-name nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (with-temp-file unreadable-file (insert "not important\n"))
          (piem-test--write-session-file
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
                      ((symbol-function 'piem--check-dependencies)
                       #'ignore)
                      ((symbol-function 'piem--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'piem--display-buffers)
                       #'ignore)
                      ((symbol-function 'piem--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'piem--resume-selected-session)
                       #'ignore))
              (call-interactively #'piem-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should-not (nth 1 read-args))
          (should-not (nth 2 read-args))
          (should (eq (nth 3 read-args) t))
          (should-not (nth 4 read-args))
          (should-not read-buffer-file-name))
      (when file-buf (kill-buffer file-buf))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-open-session-file-non-regular-jsonl-has-no-file-default ()
  "A non-regular .jsonl path is not used as a session-file default."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-open-non-regular-sessions-"))
         (project-dir (piem-test--make-temp-directory
                       "piem-test-open-non-regular-project-"))
         (jsonl-dir (expand-file-name "directory.jsonl" session-dir))
         (chosen-file (expand-file-name "chosen.jsonl" session-dir))
         (buffers (piem-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (read-args nil)
         (read-buffer-file-name nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (make-directory jsonl-dir)
          (piem-test--write-session-file
           chosen-file "chosen" (directory-file-name project-dir))
          (setq file-buf (generate-new-buffer " *piem-jsonl-dir*"))
          (with-current-buffer file-buf
            (setq buffer-file-name jsonl-dir)
            (setq default-directory session-dir)
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (&rest args)
                         (setq read-args args
                               read-buffer-file-name buffer-file-name)
                         chosen-file))
                      ((symbol-function 'piem--check-dependencies)
                       #'ignore)
                      ((symbol-function 'piem--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'piem--display-buffers)
                       #'ignore)
                      ((symbol-function 'piem--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'piem--resume-selected-session)
                       #'ignore))
              (call-interactively #'piem-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should-not (nth 1 read-args))
          (should-not (nth 2 read-args))
          (should (eq (nth 3 read-args) t))
          (should-not (nth 4 read-args))
          (should-not read-buffer-file-name))
      (when file-buf (kill-buffer file-buf))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-open-session-file-dired-directory-has-no-file-default ()
  "Interactively opening from Dired does not default to a directory at point."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-open-dired-dir-sessions-"))
         (project-dir (piem-test--make-temp-directory
                       "piem-test-open-dired-dir-project-"))
         (subdir (expand-file-name "subdir" session-dir))
         (visited-file (expand-file-name "visited.jsonl" session-dir))
         (chosen-file (expand-file-name "chosen.jsonl" session-dir))
         (buffers (piem-test--make-open-session-command-buffers))
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
                      ((symbol-function 'piem--session-file-cwd-or-error)
                       (lambda (_path) project-dir))
                      ((symbol-function 'piem--check-dependencies)
                       #'ignore)
                      ((symbol-function 'piem--setup-session)
                       (lambda (_dir &optional _session) chat-buf))
                      ((symbol-function 'piem--display-buffers)
                       #'ignore)
                      ((symbol-function 'piem--session-transition-ready-p)
                       (lambda (_chat _action) t))
                      ((symbol-function 'piem--resume-selected-session)
                       #'ignore))
              (call-interactively #'piem-open-session-file)))
          (should (equal (nth 0 read-args) "Pi session file: "))
          (should-not (nth 1 read-args))
          (should-not (nth 2 read-args))
          (should (eq (nth 3 read-args) t))
          (should-not (nth 4 read-args))
          (should-not read-buffer-file-name))
      (when dired-buf (kill-buffer dired-buf))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-open-session-file-does-not-change-dired-piem ()
  "Plain `piem' stays directory-oriented when called from Dired."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-open-plain-dired-"))
         (session-file (expand-file-name "session.jsonl" session-dir))
         (buffers (piem-test--make-open-session-command-buffers))
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
                         (ert-fail "piem inspected Dired point")))
                      ((symbol-function 'project-current)
                       (lambda (&rest _) nil))
                      ((symbol-function 'piem--check-dependencies)
                       #'ignore)
                      ((symbol-function 'piem--setup-session)
                       (lambda (dir &optional _session)
                         (setq setup-dir dir)
                         chat-buf))
                      ((symbol-function 'piem--display-buffers)
                       #'ignore))
              (piem)))
          (should (equal setup-dir session-dir)))
      (when dired-buf (kill-buffer dired-buf))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory session-dir t))))

(ert-deftest piem-test-open-session-file-does-not-change-visited-jsonl-piem ()
  "Plain `piem' stays directory-oriented in a JSONL buffer."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-open-plain-jsonl-"))
         (session-file (expand-file-name "session.jsonl" session-dir))
         (buffers (piem-test--make-open-session-command-buffers))
         (chat-buf (car buffers))
         (input-buf (cadr buffers))
         (setup-dir nil)
         (file-buf nil))
    (unwind-protect
        (progn
          (with-temp-file session-file (insert "{}\n"))
          (setq file-buf (find-file-noselect session-file))
          (with-current-buffer file-buf
            (cl-letf (((symbol-function 'piem--read-session-file-name)
                       (lambda ()
                         (ert-fail "piem read a session file")))
                      ((symbol-function 'piem--session-file-cwd-or-error)
                       (lambda (&rest _)
                         (ert-fail "piem validated a session file")))
                      ((symbol-function 'piem--resume-selected-session)
                       (lambda (&rest _)
                         (ert-fail "piem resumed a session file")))
                      ((symbol-function 'project-current)
                       (lambda (&rest _) nil))
                      ((symbol-function 'piem--check-dependencies)
                       #'ignore)
                      ((symbol-function 'piem--setup-session)
                       (lambda (dir &optional _session)
                         (setq setup-dir dir)
                         chat-buf))
                      ((symbol-function 'piem--display-buffers)
                       #'ignore))
              (piem)))
          (should (equal setup-dir session-dir)))
      (when file-buf (kill-buffer file-buf))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory session-dir t))))

;;; DWIM & Toggle

(ert-deftest piem-test-dwim-reuses-existing-session ()
  "Calling `piem' from a non-pi buffer reuses the existing session."
  (piem-test-with-mock-session "/tmp/piem-test-dwim/"
    ;; Session exists; now call from a non-pi buffer in the same project
    (with-temp-buffer
      (setq default-directory "/tmp/piem-test-dwim/")
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'piem--display-buffers) #'ignore))
        (piem))
      ;; Should not have created a second chat buffer
      (should (= 1 (length (cl-remove-if-not
                             (lambda (b)
                               (string-prefix-p "*piem-chat:/tmp/piem-test-dwim/"
                                                (buffer-name b)))
                             (buffer-list))))))))

(ert-deftest piem-test-dwim-reuses-saved-chat-buffer-after-write-file ()
  "A saved chat buffer is still reused as the project session."
  (let ((root (piem-test--make-temp-directory
               "piem-test-dwim-write-file-"))
        (file nil)
        (chat nil)
        (input nil)
        (make-backup-files nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'piem--start-process) (lambda (_) nil))
                  ((symbol-function 'piem--display-buffers) #'ignore)
                  ((symbol-function 'piem--check-dependencies) #'ignore))
          (setq chat (piem--setup-session root nil)
                input (buffer-local-value 'piem--input-buffer chat)
                file (piem-test--write-chat-buffer
                      chat "piem-chat-dwim-" "Saved copy\n"))
          (with-temp-buffer
            (setq default-directory root)
            (piem))
          (should (eq (piem--find-session root) chat))
          (should (eq (buffer-local-value 'piem--input-buffer chat)
                      input))
          (with-current-buffer chat
            (should (equal (piem--chat-session-buffer-name)
                           (piem-test--chat-buffer-name root)))
            (should (equal (piem--session-directory) root))
            (should (equal buffer-file-name file))))
      (piem-test--kill-live-buffers input chat)
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

(ert-deftest piem-test-setup-session-shows-startup-error-from-initial-state-request ()
  "Initial startup failure should be rendered into the chat buffer."
  (let ((root (piem-test--make-temp-directory
               "piem-test-startup-error-"))
        (proc (start-process "piem-startup-error" nil "cat"))
        (chat nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'piem--start-process) (lambda (_) proc))
                  ((symbol-function 'piem--fetch-commands) (lambda (&rest _) nil))
                  ((symbol-function 'piem--rpc-async)
                   (lambda (_proc cmd callback)
                     (should (equal (plist-get cmd :type) "get_state"))
                     (funcall callback
                              '(:type "response"
                                :command "get_state"
                                :success :false
                                :error "Process exited: exited abnormally with code 1"
                                :stderr "InvalidArgumentError: Invalid URL protocol")))))
          (setq chat (piem--setup-session root nil))
          (should (buffer-live-p chat))
          (with-current-buffer chat
            (should (string-match-p "failed to start" (buffer-string)))
            (should (string-match-p "Invalid URL protocol" (buffer-string)))))
      (when (process-live-p proc)
        (delete-process proc))
      (piem-test--kill-session-buffers root))))

(ert-deftest piem-test-setup-session-ignores-stale-startup-error ()
  "A replaced startup process must not render into the newer process chat."
  (let ((root (piem-test--make-temp-directory
               "piem-test-stale-startup-error-"))
        (old-proc (start-process "piem-old-startup" nil "cat"))
        (new-proc (start-process "piem-new-startup" nil "cat"))
        (state-callback nil)
        (chat nil))
    (unwind-protect
        (progn
          (set-process-query-on-exit-flag old-proc nil)
          (set-process-query-on-exit-flag new-proc nil)
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                    ((symbol-function 'piem--start-process)
                     (lambda (_) old-proc))
                    ((symbol-function 'piem--fetch-commands)
                     (lambda (&rest _) nil))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (proc cmd callback)
                       (should (eq proc old-proc))
                       (should (equal (plist-get cmd :type) "get_state"))
                       (setq state-callback callback))))
            (setq chat (piem--setup-session root nil)))
          (should state-callback)
          (with-current-buffer chat
            (piem--set-process new-proc))
          (delete-process old-proc)
          (funcall state-callback
                   '(:type "response"
                     :command "get_state"
                     :success :false
                     :processExit t
                     :error "Process exited: old startup failed"
                     :stderr "OLD-STARTUP-STDERR"
                     :exitCode 1))
          (with-current-buffer chat
            (should (eq piem--process new-proc))
            (should-not (string-match-p "OLD-STARTUP-STDERR"
                                        (buffer-string)))
            (should-not (string-match-p "pi failed to start"
                                        (buffer-string))))
          (should-not (process-get old-proc
                                   'piem-exit-error-rendered)))
      (piem-test--kill-session-buffers root)
      (piem--unregister-display-handler old-proc)
      (when (process-live-p old-proc)
        (delete-process old-proc))
      (when (process-live-p new-proc)
        (delete-process new-proc)))))

(ert-deftest piem-test-setup-session-deduplicates-dead-startup-exit ()
  "A dead initial get_state process renders its stderr only once."
  (let ((root (piem-test--make-temp-directory
               "piem-test-startup-exit-dedup-"))
        (proc (start-process "piem-startup-exit-dedup" nil
                             "sh" "-c" "exit 1"))
        (response '(:type "response"
                    :command "get_state"
                    :success :false
                    :processExit t
                    :error "Process exited: exited abnormally with code 1"
                    :stderr "ECOMPROMISED: lock was compromised"
                    :exitCode 1))
        (chat nil))
    (unwind-protect
        (progn
          (set-process-sentinel proc nil)
          (set-process-query-on-exit-flag proc nil)
          (should (piem-test-wait-for-process-exit proc))
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                    ((symbol-function 'piem--start-process)
                     (lambda (_) proc))
                    ((symbol-function 'piem--fetch-commands)
                     (lambda (&rest _) nil))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (rpc-proc cmd callback)
                       (should (eq rpc-proc proc))
                       (should (equal (plist-get cmd :type) "get_state"))
                       ;; Core dispatches pending callbacks before the exit
                       ;; handler, so reproduce that order here.
                       (funcall callback response)
                       (funcall (process-get proc
                                            'piem-exit-handler)
                                response))))
            (setq chat (piem--setup-session root nil)))
          (should (process-get proc
                               'piem-exit-error-rendered))
          (with-current-buffer chat
            (let ((chat-text (buffer-string)))
              (should (= (piem-test--count-matches
                          "ECOMPROMISED" chat-text)
                         1))
              (should (= (piem-test--count-matches
                          "pi failed to start" chat-text)
                         1))
              (should-not (string-match-p "pi process exited" chat-text)))))
      (when (process-live-p proc)
        (delete-process proc))
      (piem-test--kill-session-buffers root))))

(ert-deftest piem-test-setup-session-shows-startup-env-node-hint ()
  "Initial env/node startup failures should explain subprocess PATH."
  (let ((root (piem-test--make-temp-directory
               "piem-test-startup-env-node-"))
        (proc (start-process "piem-startup-env-node" nil "cat"))
        (chat nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'piem--start-process) (lambda (_) proc))
                  ((symbol-function 'piem--fetch-commands) (lambda (&rest _) nil))
                  ((symbol-function 'piem--rpc-async)
                   (lambda (_proc cmd callback)
                     (should (equal (plist-get cmd :type) "get_state"))
                     (funcall callback
                              '(:type "response"
                                :command "get_state"
                                :success :false
                                :error "Process exited: exited abnormally with code 127"
                                :stderr "/usr/bin/env: node: No such file or directory"
                                :exitCode 127)))))
          (setq chat (piem--setup-session root nil))
          (should (buffer-live-p chat))
          (with-current-buffer chat
            (should (string-match-p "failed to start" (buffer-string)))
            (should (string-match-p "Node launcher" (buffer-string)))
            (should (string-match-p "subprocess PATH" (buffer-string)))))
      (when (process-live-p proc)
        (delete-process proc))
      (piem-test--kill-session-buffers root))))

(ert-deftest piem-test-from-chat-buffer-noop-when-both-visible ()
  "From chat, `piem' avoids redisplay and focuses input."
  (let ((root "/tmp/piem-test-chat-visible/")
        (display-called nil))
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
                  (input (get-buffer (piem-test--input-buffer-name root))))
              (select-window (car (get-buffer-window-list chat nil t)))
              (with-current-buffer chat
                (cl-letf (((symbol-function 'piem--display-buffers)
                           (lambda (&rest _)
                             (setq display-called t))))
                  (piem)))
              (should-not display-called)
              (should (get-buffer-window-list chat nil t))
              (should (get-buffer-window-list input nil t))
              (should (eq (window-buffer (selected-window)) input))))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-from-input-buffer-noop-when-both-visible ()
  "From input, `piem' avoids redisplay when both panes are visible."
  (let ((root "/tmp/piem-test-input-visible/")
        (display-called nil))
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
                  (input (get-buffer (piem-test--input-buffer-name root))))
              (with-current-buffer input
                (cl-letf (((symbol-function 'piem--display-buffers)
                           (lambda (&rest _)
                             (setq display-called t))))
                  (piem)))
              (should-not display-called)
              (should (get-buffer-window-list chat nil t))
              (should (get-buffer-window-list input nil t))))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-from-chat-buffer-focuses-current-session-input ()
  "With multiple sessions visible, `piem' focuses this session's input."
  (let ((root "/tmp/piem-test-focus-root/")
        (sub "/tmp/piem-test-focus-root/somesubdir/")
        (display-called nil))
    (make-directory root t)
    (make-directory sub t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (with-temp-buffer
              (setq default-directory root)
              (piem))
            (with-temp-buffer
              (setq default-directory sub)
              (piem))
            (let ((root-chat (get-buffer (piem-test--chat-buffer-name root)))
                  (root-input (get-buffer (piem-test--input-buffer-name root)))
                  (sub-input (get-buffer (piem-test--input-buffer-name sub))))
              (delete-other-windows)
              (switch-to-buffer root-chat)
              (let ((root-input-win (split-window nil -10 'below)))
                (set-window-buffer root-input-win root-input))
              (let ((sub-win (split-window-right)))
                (set-window-buffer sub-win sub-input))
              (select-window (get-buffer-window root-chat))
              (with-current-buffer root-chat
                (cl-letf (((symbol-function 'piem--display-buffers)
                           (lambda (&rest _)
                             (setq display-called t))))
                  (piem)))
              (should-not display-called)
              (should (eq (window-buffer (selected-window)) root-input))))
        (piem-test--kill-session-buffers root)
        (piem-test--kill-session-buffers sub)
        (delete-other-windows)))))

(ert-deftest piem-test-from-pi-buffer-redisplays-when-visible-only-in-other-frame ()
  "Calling `piem' should redisplay in current frame.
Even if chat/input are visible in another frame, current-frame visibility
must decide whether this is a no-op."
  (let ((root "/tmp/piem-test-other-frame-noop/")
        (display-called nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (setq default-directory root)
            (piem)
            (let ((chat (get-buffer (piem-test--chat-buffer-name root))))
              (with-current-buffer chat
                (cl-letf (((symbol-function 'get-buffer-window-list)
                           (lambda (_buffer _minibuf &optional all-frames)
                             (if all-frames '(foreign-window) nil)))
                          ((symbol-function 'piem--display-buffers)
                           (lambda (&rest _)
                             (setq display-called t))))
                  (piem)))
              (should display-called)))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-from-chat-buffer-restores-missing-input-window ()
  "Calling `piem' from chat restores input and focuses it."
  (let ((root "/tmp/piem-test-chat-restore/"))
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
                  (input (get-buffer (piem-test--input-buffer-name root))))
              (delete-window (car (get-buffer-window-list input nil t)))
              (with-current-buffer chat
                (piem))
              (should (= 1 (length (get-buffer-window-list input nil t))))
              (should (eq (window-buffer (selected-window)) input))))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-from-input-buffer-restores-missing-chat-window ()
  "Calling `piem' from input restores the split layout."
  (let ((root "/tmp/piem-test-input-restore/"))
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
                  (input (get-buffer (piem-test--input-buffer-name root))))
              (let ((input-win (car (get-buffer-window-list input nil t))))
                (select-window input-win)
                (delete-other-windows input-win))
              (with-current-buffer input
                (piem))
              (should (= 1 (length (get-buffer-window-list chat nil t))))
              (should (= 1 (length (get-buffer-window-list input nil t))))))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-non-pi-call-creates-default-session-when-only-named-exists ()
  "Calling `piem' creates default session when only named one exists."
  (let* ((root "/tmp/piem-test-dwim-named/")
         (default-directory root)
         (displayed nil)
         (named-chat (piem-test--chat-buffer-name root "my-feature"))
         (named-input (piem-test--input-buffer-name root "my-feature"))
         (default-chat (piem-test--chat-buffer-name root))
         (default-input (piem-test--input-buffer-name root)))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--display-buffers)
               (lambda (chat _input &rest _) (setq displayed chat))))
      (unwind-protect
          (progn
            ;; Create named session first.
            (piem "my-feature")
            ;; Non-pi call should create/reuse default unnamed session.
            (with-temp-buffer
              (setq default-directory root)
              (setq displayed nil)
              (piem)
              (should displayed)
              (should (equal (buffer-name displayed) default-chat)))
            (should (get-buffer named-chat))
            (should (get-buffer named-input))
            (should (get-buffer default-chat))
            (should (get-buffer default-input)))
        (piem-test--kill-session-buffers root "my-feature")
        (piem-test--kill-session-buffers root)))))

(ert-deftest piem-test-named-session-reuses-saved-chat-buffer-after-write-file ()
  "Saving a named session keeps it distinct from the default session."
  (let ((root (piem-test--make-temp-directory
               "piem-test-named-write-file-"))
        (file nil)
        (default-chat nil)
        (default-input nil)
        (named-chat nil)
        (named-input nil)
        (make-backup-files nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'piem--start-process) (lambda (_) nil))
                  ((symbol-function 'piem--display-buffers) #'ignore)
                  ((symbol-function 'piem--check-dependencies) #'ignore))
          (setq default-chat (piem--setup-session root nil)
                default-input (buffer-local-value 'piem--input-buffer default-chat)
                named-chat (piem--setup-session root "feature")
                named-input (buffer-local-value 'piem--input-buffer named-chat)
                file (piem-test--write-chat-buffer
                      named-chat "piem-chat-named-"
                      "Named session archive\n"))
          (with-temp-buffer
            (setq default-directory root)
            (piem "feature"))
          (should (eq (piem--find-session root) default-chat))
          (should (eq (piem--find-session root "feature") named-chat))
          (should-not (eq default-chat named-chat))
          (with-current-buffer named-chat
            (should (equal (piem--chat-session-buffer-name)
                           (piem-test--chat-buffer-name root "feature")))
            (should (equal (piem--session-directory) root))
            (should (equal buffer-file-name file))))
      (piem-test--kill-live-buffers
       named-input named-chat default-input default-chat)
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

(ert-deftest piem-test-new-session-with-prefix-arg ()
  "\\[universal-argument] \\[piem] creates a named session."
  (let ((root "/tmp/piem-test-named/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--display-buffers) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) "my-session")))
      (let ((current-prefix-arg '(4))
            (default-directory root))
        (unwind-protect
            (progn
              (call-interactively #'piem)
              (should (get-buffer (piem-test--chat-buffer-name root "my-session"))))
          (piem-test--kill-session-buffers root "my-session"))))))

(ert-deftest piem-test-non-pi-rerun-from-small-window-does-not-error ()
  "Calling `piem' from a small non-pi window should not error."
  (let ((root "/tmp/piem-test-small-window/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (with-temp-buffer
              (setq default-directory root)
              (piem))
            (let* ((chat (get-buffer (piem-test--chat-buffer-name root)))
                   (input (get-buffer (piem-test--input-buffer-name root)))
                   (input-win (car (get-buffer-window-list input nil t)))
                   (non-pi (get-buffer-create "*piem-test-non-pi*")))
              (select-window input-win)
              (with-current-buffer non-pi
                (setq default-directory root))
              (switch-to-buffer non-pi)
              (piem)
              (should (get-buffer-window-list chat nil t))
              (should (get-buffer-window-list input nil t))))
        (piem-test--kill-session-buffers root)
        (ignore-errors (kill-buffer "*piem-test-non-pi*"))
        (delete-other-windows)))))

(ert-deftest piem-test-non-pi-rerun-with-chat-hidden-avoids-duplicate-input-windows ()
  "Restoring from input-only visibility should keep a single input window."
  (let ((root "/tmp/piem-test-input-only-rerun/"))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (with-temp-buffer
              (setq default-directory root)
              (piem))
            (let* ((chat (get-buffer (piem-test--chat-buffer-name root)))
                   (input (get-buffer (piem-test--input-buffer-name root)))
                   (chat-win (car (get-buffer-window-list chat nil t)))
                   (non-pi (get-buffer-create "*piem-test-non-pi*")))
              ;; Hide chat by replacing it with a non-pi buffer, leaving input visible.
              (select-window chat-win)
              (with-current-buffer non-pi
                (setq default-directory root))
              (switch-to-buffer non-pi)
              (piem)
              (should (= 1 (length (get-buffer-window-list input nil t))))
              (should (= 1 (length (get-buffer-window-list chat nil t))))))
        (piem-test--kill-session-buffers root)
        (ignore-errors (kill-buffer "*piem-test-non-pi*"))
        (delete-other-windows)))))

(ert-deftest piem-test-project-buffers-excludes-subdir-sessions ()
  "`piem-project-buffers' should match the directory exactly."
  (let ((root "/tmp/piem-test-root/")
        (sub "/tmp/piem-test-root/somesubdir/"))
    (make-directory root t)
    (make-directory sub t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--check-dependencies) #'ignore))
      (unwind-protect
          (progn
            (with-temp-buffer
              (setq default-directory root)
              (piem--setup-session root nil))
            (with-temp-buffer
              (setq default-directory sub)
              (piem--setup-session sub nil))
            (with-temp-buffer
              (setq default-directory root)
              (let ((buffers (piem-project-buffers)))
                (should (= 1 (length buffers)))
                (should (equal (car buffers)
                               (get-buffer (piem-test--chat-buffer-name root)))))))
        (piem-test--kill-session-buffers root)
        (piem-test--kill-session-buffers sub)))))

(ert-deftest piem-test-toggle-existing-session-does-not-check-dependencies ()
  "Toggling an existing session is a UI action, not a process launch."
  (let* ((root (piem-test--make-temp-directory
                "piem-test-toggle-no-deps-"))
         (chat (generate-new-buffer (piem-test--chat-buffer-name root)))
         (input (generate-new-buffer (piem-test--input-buffer-name root)))
         (displayed nil))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (piem-chat-mode)
            (piem--set-chat-session-identity root)
            (piem--set-input-buffer input))
          (with-current-buffer input
            (piem-input-mode)
            (piem--set-chat-buffer chat))
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                    ((symbol-function 'piem--check-dependencies)
                     (lambda (&rest _)
                       (ert-fail "toggle checked process dependencies")))
                    ((symbol-function 'piem--display-buffers)
                     (lambda (_chat _input &rest _)
                       (setq displayed t))))
            (with-temp-buffer
              (setq default-directory root)
              (piem-toggle)))
          (should displayed))
      (piem-test--kill-live-buffers input chat)
      (delete-directory root t))))

(ert-deftest piem-test-toggle-uses-exact-project-session ()
  "`piem-toggle' should not pick a subdir session for parent dir."
  (let ((root "/tmp/piem-test-toggle-root/")
        (sub "/tmp/piem-test-toggle-root/somesubdir/")
        (displayed-name nil))
    (make-directory root t)
    (make-directory sub t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--check-dependencies) #'ignore)
              ((symbol-function 'piem--display-buffers)
               (lambda (chat _input &rest _)
                 (setq displayed-name (buffer-name chat)))))
      (unwind-protect
          (progn
            (with-temp-buffer
              (setq default-directory root)
              (piem--setup-session root nil))
            (with-temp-buffer
              (setq default-directory sub)
              (piem--setup-session sub nil))
            ;; Make subdir chat more recent, then hide all pi windows.
            (switch-to-buffer (piem-test--chat-buffer-name sub))
            (switch-to-buffer "*scratch*")
            (with-temp-buffer
              (setq default-directory root)
              (piem-toggle))
            (should (equal displayed-name
                           (piem-test--chat-buffer-name root))))
        (piem-test--kill-session-buffers root)
        (piem-test--kill-session-buffers sub)))))

(ert-deftest piem-test-toggle-from-pi-buffer-uses-current-session ()
  "`piem-toggle' from pi buffer should use current session directly."
  (let ((root "/tmp/piem-test-toggle-current-root/")
        (sub "/tmp/piem-test-toggle-current-root/somesubdir/"))
    (make-directory root t)
    (make-directory sub t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--check-dependencies) #'ignore)
              ;; If toggle consulted project-buffers here, it would pick sub.
              ((symbol-function 'piem-project-buffers)
               (lambda ()
                 (list (get-buffer (piem-test--chat-buffer-name sub))))))
      (unwind-protect
          (progn
            (with-temp-buffer
              (setq default-directory root)
              (piem))
            (with-temp-buffer
              (setq default-directory sub)
              (piem--setup-session sub nil))
            (let ((root-chat (get-buffer (piem-test--chat-buffer-name root)))
                  (root-input (get-buffer (piem-test--input-buffer-name root)))
                  (sub-chat (get-buffer (piem-test--chat-buffer-name sub))))
              (with-current-buffer root-chat
                (piem-toggle))
              (should-not (get-buffer-window-list root-chat nil t))
              (should-not (get-buffer-window-list root-input nil t))
              (should-not (get-buffer-window-list sub-chat nil t))))
        (piem-test--kill-session-buffers root)
        (piem-test--kill-session-buffers sub)
        (delete-other-windows)))))

(ert-deftest piem-test-project-buffers-finds-session ()
  "`piem-project-buffers' returns chat buffer for the current project."
  (piem-test-with-mock-session "/tmp/piem-test-projbuf/"
    (let ((default-directory "/tmp/piem-test-projbuf/"))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (should (= 1 (length (piem-project-buffers))))
        (should (string-prefix-p "*piem-chat:"
                                 (buffer-name (car (piem-project-buffers)))))))))

(ert-deftest piem-test-project-buffers-finds-saved-session-after-write-file ()
  "`piem-project-buffers' still finds a saved chat buffer."
  (let ((root (piem-test--make-temp-directory
               "piem-test-projbuf-write-file-"))
        (file nil)
        (chat nil)
        (input nil)
        (make-backup-files nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'piem--start-process) (lambda (_) nil))
                  ((symbol-function 'piem--display-buffers) #'ignore)
                  ((symbol-function 'piem--check-dependencies) #'ignore))
          (setq chat (piem--setup-session root nil)
                input (buffer-local-value 'piem--input-buffer chat)
                file (piem-test--write-chat-buffer
                      chat "piem-chat-projbuf-"))
          (with-temp-buffer
            (setq default-directory root)
            (should (equal (piem-project-buffers)
                           (list chat)))))
      (piem-test--kill-live-buffers input chat)
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

(ert-deftest piem-test-project-root-session-reused-after-write-file-from-subdir ()
  "Saving from a subdir keeps the project-root session identity."
  (let* ((root (piem-test--make-temp-directory
                "piem-test-write-file-project-root-"))
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
                  ((symbol-function 'piem--start-process) (lambda (_) nil))
                  ((symbol-function 'piem--check-dependencies) #'ignore)
                  ((symbol-function 'piem--display-buffers) #'ignore))
          (with-temp-buffer
            (setq default-directory nested)
            (piem)
            (setq chat (piem--find-session root)
                  input (get-buffer (piem-test--input-buffer-name root))))
          (setq file (piem-test--write-chat-buffer
                      chat "piem-chat-project-root-"
                      "Saved from nested dir\n"))
          (with-current-buffer chat
            (should (equal (piem--chat-session-buffer-name)
                           (piem-test--chat-buffer-name root)))
            (should (equal (piem--session-directory) root))
            (should (equal buffer-file-name file)))
          (with-temp-buffer
            (setq default-directory sibling)
            (piem)
            (should (equal (piem-project-buffers)
                           (list chat))))
          (should (eq (piem--find-session root) chat))
          (should (eq (get-buffer (piem-test--input-buffer-name root))
                      input)))
      (piem-test--kill-live-buffers input chat)
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

(ert-deftest piem-test-project-buffers-excludes-other-projects ()
  "`piem-project-buffers' returns nil for a different project."
  (piem-test-with-mock-session "/tmp/piem-test-projbuf-a/"
    (let ((default-directory "/tmp/piem-test-projbuf-b/"))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (should (null (piem-project-buffers)))))))

(ert-deftest piem-test-toggle-finds-saved-session-after-write-file ()
  "`piem-toggle' still finds a saved chat buffer."
  (let ((root (piem-test--make-temp-directory
               "piem-test-toggle-write-file-"))
        (file nil)
        (chat nil)
        (input nil)
        (displayed-chat nil)
        (displayed-input nil)
        (make-backup-files nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'piem--start-process) (lambda (_) nil))
                  ((symbol-function 'piem--check-dependencies) #'ignore)
                  ((symbol-function 'piem--display-buffers)
                   (lambda (chat-buf input-buf &rest _)
                     (setq displayed-chat chat-buf
                           displayed-input input-buf))))
          (setq chat (piem--setup-session root nil)
                input (buffer-local-value 'piem--input-buffer chat)
                file (piem-test--write-chat-buffer
                      chat "piem-chat-toggle-"))
          (with-temp-buffer
            (setq default-directory root)
            (piem-toggle))
          (should (eq displayed-chat chat))
          (should (eq displayed-input input)))
      (piem-test--kill-live-buffers input chat)
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t)))))

(ert-deftest piem-test-toggle-hides-and-shows-saved-session-after-write-file ()
  "`piem-toggle' hides and restores a saved session."
  (let ((root (piem-test--make-temp-directory
               "piem-test-toggle-write-file-live-"))
        (file nil)
        (chat nil)
        (input nil)
        (make-backup-files nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'piem--start-process) (lambda (_) nil))
                  ((symbol-function 'piem--check-dependencies) #'ignore))
          (delete-other-windows)
          (switch-to-buffer "*scratch*")
          (setq default-directory root)
          (piem)
          (setq chat (piem--find-session root)
                input (get-buffer (piem-test--input-buffer-name root)))
          (setq file (piem-test--write-chat-buffer
                      chat "piem-chat-toggle-live-"))
          (with-current-buffer chat
            (should (equal (piem--chat-session-buffer-name)
                           (piem-test--chat-buffer-name root)))
            (should (equal (piem--session-directory) root))
            (should (equal buffer-file-name file)))
          (should (get-buffer-window-list chat nil t))
          (should (get-buffer-window-list input nil t))
          (let ((non-pi (get-buffer-create "*piem-test-toggle-non-pi*")))
            (with-current-buffer non-pi
              (setq default-directory root))
            (switch-to-buffer non-pi)
            (piem-toggle)
            (should-not (get-buffer-window-list chat nil t))
            (should-not (get-buffer-window-list input nil t))
            (piem-toggle)
            (should (get-buffer-window-list chat nil t))
            (should (get-buffer-window-list input nil t))
            (with-current-buffer chat
              (should (equal buffer-file-name file)))))
      (piem-test--kill-live-buffers input chat)
      (ignore-errors (kill-buffer "*piem-test-toggle-non-pi*"))
      (ignore-errors (delete-file file))
      (ignore-errors (delete-directory root t))
      (delete-other-windows))))

(ert-deftest piem-test-toggle-no-session-errors ()
  "`piem-toggle' signals `user-error' when no session exists."
  (let ((default-directory "/tmp/piem-test-no-session/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--check-dependencies) #'ignore))
      (should-error (piem-toggle) :type 'user-error))))

(ert-deftest piem-test-toggle-shows-in-current-frame-when-only-visible-elsewhere ()
  "`piem-toggle' should show in current frame when hidden there."
  (let ((root "/tmp/piem-test-toggle-other-frame/")
        (display-called nil)
        (hide-called nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--check-dependencies) #'ignore))
      (unwind-protect
          (progn
            (with-temp-buffer
              (setq default-directory root)
              (piem))
            (with-temp-buffer
              (setq default-directory root)
              (cl-letf (((symbol-function 'get-buffer-window-list)
                         (lambda (_buffer _minibuf &optional all-frames)
                           (if all-frames '(foreign-window) nil)))
                        ((symbol-function 'piem--display-buffers)
                         (lambda (&rest _)
                           (setq display-called t)))
                        ((symbol-function 'piem--hide-session-windows)
                         (lambda ()
                           (setq hide-called t))))
                (piem-toggle)))
            (should display-called)
            (should-not hide-called))
        (piem-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest piem-test-toggle-hides-session-from-non-pi-window ()
  "`piem-toggle' hides a visible session when called from non-pi."
  (let ((root "/tmp/piem-test-toggle-hide/")
        (chat nil)
        (input nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--check-dependencies) #'ignore))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (setq default-directory root)
            (piem)
            (setq chat (get-buffer (piem-test--chat-buffer-name root)))
            (setq input (get-buffer (piem-test--input-buffer-name root)))
            (let* ((input-win (car (get-buffer-window-list input nil t)))
                   (non-pi (get-buffer-create "*piem-test-non-pi*")))
              (select-window input-win)
              (with-current-buffer non-pi
                (setq default-directory root))
              (switch-to-buffer non-pi)
              (piem-toggle))
            (should-not (get-buffer-window-list chat nil t))
            (should-not (get-buffer-window-list input nil t)))
        (piem-test--kill-session-buffers root)
        (ignore-errors (kill-buffer "*piem-test-non-pi*"))
        (delete-other-windows)))))

(ert-deftest piem-test-toggle-hides-session-when-only-input-visible ()
  "`piem-toggle' hides session when only input is visible."
  (let ((root "/tmp/piem-test-toggle-input-only/")
        (chat nil)
        (input nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--check-dependencies) #'ignore))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (setq default-directory root)
            (piem)
            (setq chat (get-buffer (piem-test--chat-buffer-name root)))
            (setq input (get-buffer (piem-test--input-buffer-name root)))
            (let* ((chat-win (car (get-buffer-window-list chat nil t)))
                   (non-pi (get-buffer-create "*piem-test-non-pi*")))
              ;; Keep only input visible by replacing chat with a non-pi buffer.
              (select-window chat-win)
              (with-current-buffer non-pi
                (setq default-directory root))
              (switch-to-buffer non-pi)
              (piem-toggle))
            (should-not (get-buffer-window-list chat nil t))
            (should-not (get-buffer-window-list input nil t)))
        (piem-test--kill-session-buffers root)
        (ignore-errors (kill-buffer "*piem-test-non-pi*"))
        (delete-other-windows)))))

(ert-deftest piem-test-batch-emacs-loads-overridden-package-directory ()
  "A child Emacs loads dependencies from the caller's package directory."
  (let* ((root (make-temp-file "pi-child-package-" t))
         (package-user-dir (expand-file-name "elpa" root))
         (package-dir (expand-file-name "pi-child-dependency-1.0"
                                        package-user-dir)))
    (unwind-protect
        (progn
          (make-directory package-dir t)
          (with-temp-file (expand-file-name "pi-child-dependency-pkg.el"
                                             package-dir)
            (insert "(define-package \"pi-child-dependency\" \"1.0\" \"Test dependency\")\n"))
          (with-temp-file (expand-file-name "pi-child-dependency-autoloads.el"
                                             package-dir)
            (insert "(add-to-list 'load-path\n"
                    "             (directory-file-name\n"
                    "              (file-name-directory\n"
                    "               (or load-file-name buffer-file-name))))\n"))
          (with-temp-file (expand-file-name "pi-child-dependency.el" package-dir)
            (insert "(defconst pi-child-dependency-value 'loaded-from-override)\n"
                    "(provide 'pi-child-dependency)\n"))
          (let ((process-environment (copy-sequence process-environment)))
            (setenv "PACKAGE_USER_DIR" package-user-dir)
            (should
             (eq 'loaded-from-override
                 (piem-test--read-batch-emacs-result
                  "(progn
  (display-warning 'piem-test \"injected child warning\")
  (require 'pi-child-dependency)
  (prin1 pi-child-dependency-value))")))))
      (delete-directory root t))))

(ert-deftest piem-test-transient-warning-explains-built-in-upgrade ()
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
             "      (load (expand-file-name \"piem-menu.el\""
             "                              (file-name-directory"
             "                               (locate-library \"piem\")))"
             "            nil t))"
             "    (prin1 captured)))")
           " "))
         (result (piem-test--read-batch-emacs-result expression)))
    (should (string-match-p "upgrade transient from MELPA" result))
    (should (string-match-p "package-install-upgrade-built-in" result))))

(ert-deftest piem-test-transient-version-check-handles-built-in-snapshot-format ()
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
             "          (load (expand-file-name \"piem-menu.el\""
             "                                  (file-name-directory"
             "                                   (locate-library \"piem\")))"
             "                nil t)"
             "        (error (setq err (error-message-string load-err)))))"
             "    (prin1 (list :warning captured :error err))))")
           " "))
         (result (piem-test--read-batch-emacs-result expression)))
    (should-not (plist-get result :error))
    (should-not (plist-get result :warning))))

(ert-deftest piem-test-md-ts-mode-package-load-leaves-global-markdown-settings-alone ()
  "Loading `md-ts-mode' keeps global Markdown associations opt-in."
  (let ((result (piem-test--markdown-load-state 'md-ts-mode)))
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

(ert-deftest piem-test-package-load-leaves-global-markdown-settings-alone ()
  "Loading `piem' does not change global Markdown mode settings."
  (let ((result (piem-test--markdown-load-state 'piem)))
    (should (eq t (plist-get result :auto-unchanged)))
    (should (eq t (plist-get result :major-remap-unchanged)))
    (should (eq t (plist-get result :treesit-remap-unchanged)))
    (should (equal (plist-get result :before-md-association)
                   (plist-get result :after-md-association)))
    (should (equal (plist-get result :before-major-markdown-remap)
                   (plist-get result :after-major-markdown-remap)))
    (should (equal (plist-get result :before-treesit-markdown-remap)
                   (plist-get result :after-treesit-markdown-remap)))))

(provide 'piem-test)
;;; piem-test.el ends here
