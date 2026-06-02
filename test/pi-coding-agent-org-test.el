;;; pi-coding-agent-org-test.el --- Tests for pi-coding-agent Org links -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for pi-coding-agent Org link parsing, generation, and opening.

;;; Code:

(require 'ert)
(require 'org)
(require 'pi-coding-agent-org)
(require 'pi-coding-agent-test-common)

(ert-deftest pi-coding-agent-org-test-parse-session-link-query ()
  "Session links decode query-style file, dir, and name parameters."
  (let ((file-link (pi-coding-agent-org--parse-link
                    "session?file=%2Ftmp%2Fpi%20session.jsonl"))
        (named-link (pi-coding-agent-org--parse-link
                     "session?dir=%2Ftmp%2Fproject%2F&name=my+session")))
    (should (equal (plist-get file-link :kind) "session"))
    (should (equal (plist-get file-link :file) "/tmp/pi session.jsonl"))
    (should (equal (plist-get named-link :dir) "/tmp/project/"))
    (should (equal (plist-get named-link :name) "my session"))))

(ert-deftest pi-coding-agent-org-test-session-link-prefers-file ()
  "Generated session links prefer durable session files over names."
  (should (equal (pi-coding-agent-org-session-link
                  "/tmp/project/.pi/sessions/abc.jsonl"
                  "/tmp/project/"
                  "display")
                 "[[pi:session?file=%2Ftmp%2Fproject%2F.pi%2Fsessions%2Fabc.jsonl&dir=%2Ftmp%2Fproject%2F&name=display][Open pi session]]")))

(ert-deftest pi-coding-agent-org-test-store-link-from-chat-buffer ()
  "Org link storing records the current pi chat session."
  (let ((dir (make-temp-file "pi-coding-agent-org-store-" t)))
    (unwind-protect
        (with-temp-buffer
          (let ((session-file (expand-file-name ".pi/sessions/abc.jsonl" dir)))
            (make-directory (file-name-directory session-file) t)
            (write-region "{\"type\":\"session\"}\n" nil session-file nil 'silent))
          (pi-coding-agent-chat-mode)
          (pi-coding-agent--set-chat-session-identity dir "demo")
          (setq pi-coding-agent--state '(:session-file ".pi/sessions/abc.jsonl")
                pi-coding-agent--session-name "demo")
          (let ((expected-link
                 (concat "pi:session?file="
                         (pi-coding-agent-org--query-encode
                          (expand-file-name ".pi/sessions/abc.jsonl" dir))
                         "&dir="
                         (pi-coding-agent-org--query-encode
                          (file-name-as-directory dir))
                         "&name=demo"))
                org-store-link-plist)
            (should (pi-coding-agent-org-store-session-link nil))
            (should (equal (plist-get org-store-link-plist :type) "pi"))
            (should (equal (plist-get org-store-link-plist :link)
                           expected-link))
            (should (equal (plist-get org-store-link-plist :description)
                           "pi session: demo"))
            (should (equal (org-store-link nil nil)
                           (format "[[%s][pi session: demo]]"
                                   expected-link)))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest pi-coding-agent-org-test-store-link-from-named-buffer-without-metadata ()
  "Org link storing preserves named-session buffers before metadata exists."
  (let ((dir (make-temp-file "pi-coding-agent-org-store-named-" t)))
    (unwind-protect
        (with-temp-buffer
          (pi-coding-agent-chat-mode)
          (pi-coding-agent--set-chat-session-identity dir "demo")
          (let (org-store-link-plist)
            (should (pi-coding-agent-org-store-session-link nil))
            (should (equal (plist-get org-store-link-plist :link)
                           (concat "pi:session?dir="
                                   (pi-coding-agent-org--query-encode
                                    (file-name-as-directory dir))
                                   "&name=demo")))
            (should (equal (plist-get org-store-link-plist :description)
                           "pi session: demo"))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest pi-coding-agent-org-test-store-link-ignores-unflushed-session-file ()
  "Org link storing falls back to dir/name when the session file is absent."
  (let ((dir (make-temp-file "pi-coding-agent-org-store-missing-file-" t)))
    (unwind-protect
        (with-temp-buffer
          (pi-coding-agent-chat-mode)
          (pi-coding-agent--set-chat-session-identity dir "demo")
          (setq pi-coding-agent--state '(:session-file ".pi/sessions/unflushed.jsonl"))
          (let (org-store-link-plist)
            (should (pi-coding-agent-org-store-session-link nil))
            (should (equal (plist-get org-store-link-plist :link)
                           (concat "pi:session?dir="
                                   (pi-coding-agent-org--query-encode
                                    (file-name-as-directory dir))
                                   "&name=demo")))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest pi-coding-agent-org-test-store-link-from-input-buffer ()
  "Org link storing also works from the pi input buffer."
  (let ((dir (make-temp-file "pi-coding-agent-org-store-input-" t))
        (chat (generate-new-buffer " *pi-org-chat*"))
        (input (generate-new-buffer " *pi-org-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (pi-coding-agent-chat-mode)
            (pi-coding-agent--set-chat-session-identity dir "demo")
            (setq pi-coding-agent--session-name "demo"))
          (with-current-buffer input
            (pi-coding-agent-input-mode)
            (pi-coding-agent--set-chat-buffer chat)
            (let (org-store-link-plist)
              (should (pi-coding-agent-org-store-session-link nil))
              (should (equal (plist-get org-store-link-plist :link)
                             (concat "pi:session?dir="
                                     (pi-coding-agent-org--query-encode
                                      (file-name-as-directory dir))
                                     "&name=demo"))))))
      (kill-buffer chat)
      (kill-buffer input)
      (ignore-errors (delete-directory dir t)))))

(ert-deftest pi-coding-agent-org-test-open-file-link-uses-session-cwd ()
  "Opening a file-only link runs pi from the cwd recorded in the session."
  (let* ((repo-dir (make-temp-file "pi-coding-agent-org-repo-" t))
         (session-dir (make-temp-file "pi-coding-agent-org-session-" t))
         (session-file (expand-file-name "abc.jsonl" session-dir))
         (opened-dir nil)
         (opened-chat (generate-new-buffer " *pi-org-opened-cwd-chat*")))
    (unwind-protect
        (progn
          (write-region
           (json-encode (list :type "session" :version 3 :cwd repo-dir))
           nil session-file nil 'silent)
          (with-current-buffer opened-chat
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--input-buffer (generate-new-buffer
                                                 " *pi-org-opened-cwd-input*")))
          (cl-letf (((symbol-function 'pi-coding-agent--check-dependencies)
                     #'ignore)
                    ((symbol-function 'pi-coding-agent--setup-session)
                     (lambda (dir _session)
                       (setq opened-dir dir)
                       opened-chat))
                    ((symbol-function 'pi-coding-agent--display-buffers)
                     #'ignore))
            (should (eq (pi-coding-agent-org-open-link
                         (concat "session?file="
                                 (pi-coding-agent-org--query-encode
                                  session-file))
                         nil)
                        opened-chat))
            (should (equal opened-dir (file-name-as-directory repo-dir)))))
      (when (buffer-live-p opened-chat)
        (let ((input (buffer-local-value 'pi-coding-agent--input-buffer
                                         opened-chat)))
          (when (buffer-live-p input) (kill-buffer input))))
      (when (buffer-live-p opened-chat) (kill-buffer opened-chat))
      (ignore-errors (delete-directory repo-dir t))
      (ignore-errors (delete-directory session-dir t)))))

(ert-deftest pi-coding-agent-org-test-open-file-link-uses-named-buffer-when-dir-open ()
  "Opening a different session file in an open dir uses named-session buffers."
  (let* ((dir (make-temp-file "pi-coding-agent-org-open-" t))
         (session-file (expand-file-name ".pi/sessions/abc.jsonl" dir))
         (existing-chat (generate-new-buffer " *pi-org-existing-chat*"))
         (opened-chat (generate-new-buffer " *pi-org-opened-chat*"))
         (opened-session nil))
    (unwind-protect
        (progn
          (make-directory (file-name-directory session-file) t)
          (with-current-buffer existing-chat
            (pi-coding-agent-chat-mode)
            (pi-coding-agent--set-chat-session-identity dir nil))
          (with-current-buffer opened-chat
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--input-buffer (generate-new-buffer
                                                 " *pi-org-opened-input*")))
          (cl-letf (((symbol-function 'pi-coding-agent--check-dependencies)
                     #'ignore)
                    ((symbol-function 'pi-coding-agent--setup-session)
                     (lambda (_dir session)
                       (setq opened-session session)
                       opened-chat))
                    ((symbol-function 'pi-coding-agent--display-buffers)
                     #'ignore))
            (should (eq (pi-coding-agent-session-open-file session-file dir)
                        opened-chat))
            (should (equal opened-session "abc"))))
      (when (buffer-live-p opened-chat)
        (let ((input (buffer-local-value 'pi-coding-agent--input-buffer
                                         opened-chat)))
          (when (buffer-live-p input) (kill-buffer input))))
      (when (buffer-live-p existing-chat) (kill-buffer existing-chat))
      (when (buffer-live-p opened-chat) (kill-buffer opened-chat))
      (ignore-errors (delete-directory dir t)))))

(provide 'pi-coding-agent-org-test)
;;; pi-coding-agent-org-test.el ends here
