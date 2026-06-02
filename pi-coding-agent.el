;;; pi-coding-agent.el --- Emacs frontend for pi coding agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pi-coding-agent
;; Keywords: ai llm ai-pair-programming tools
;; Version: 2.4.0
;; Package-Requires: ((emacs "29.1") (transient "0.9.0") (md-ts-mode "0.3.0") (markdown-table-wrap "0.2.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs frontend for the pi coding agent (https://pi.dev).
;; Provides a two-window interface for AI-assisted coding: chat history
;; with rendered markdown, and a separate prompt composition buffer.
;;
;; Requirements:
;;   - Emacs 29.1 or later (tree-sitter support required)
;;   - pi coding agent @earendil-works/pi-coding-agent 0.75.5 or later, installed and in PATH
;;   - tree-sitter grammars for markdown and markdown-inline
;;
;; pi-coding-agent uses `md-ts-mode` for its own chat and input buffers;
;; loading it does not change global Markdown file associations.
;;
;; Usage:
;;   M-x pi-coding-agent         Start or focus session in current project
;;   C-u M-x pi-coding-agent     Start a named session
;;   M-x pi-coding-agent-toggle  Hide/show session windows in current frame
;;
;; Many users define an alias: (defalias 'pi 'pi-coding-agent)
;;
;; Key Bindings:
;;   Input buffer:
;;     C-c C-c        Send prompt (queues as follow-up if busy)
;;     C-c C-s        Queue steering (interrupts after current tool; busy only)
;;     C-c C-k        Abort streaming
;;     C-c C-p        Open menu
;;     C-c C-r        Resume session
;;     M-p / M-n      History navigation
;;     C-r            Incremental history search (like readline)
;;     TAB            Path/file completion
;;     @              File reference (search project files)
;;
;;   Chat buffer:
;;     n / p          Navigate messages
;;     TAB            Toggle completed thinking/tool section or fold turn
;;     RET            Visit file at point (from tool blocks)
;;     C-c C-p        Open menu
;;
;; Editor Features:
;;   - File reference (@): Type @ to search project files (respects .gitignore)
;;   - Path completion (Tab): Complete relative paths, ../, ~/, etc.
;;   - Message queuing: Submit messages while agent is working:
;;       C-c C-c  queues follow-up (delivered after agent completes)
;;       C-c C-s  queues steering (interrupts after current tool)
;;
;; Press C-c C-p for the full transient menu with model selection,
;; thinking level, completed-thinking controls, session management,
;; and custom commands.
;;
;; See README.org for more documentation.

;;; Code:

(require 'pi-coding-agent-menu)
(require 'pi-coding-agent-input)
(require 'json)

;;;; Public Session API

(defun pi-coding-agent--session-file-cwd (session-file)
  "Return the recorded cwd from SESSION-FILE, or nil if unavailable."
  (when (file-readable-p session-file)
    (with-temp-buffer
      (insert-file-contents session-file nil 0 4096)
      (goto-char (point-min))
      (when-let* ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
        (ignore-errors
          (let ((event (json-parse-string line :object-type 'plist)))
            (when (equal (plist-get event :type) "session")
              (plist-get event :cwd))))))))

(defun pi-coding-agent--session-file-project-directory (session-file)
  "Return a likely project directory for SESSION-FILE.
This prefers the cwd recorded inside the session file, then the nearest
ancestor containing .git, falling back to the session file's containing
directory."
  (let* ((file (expand-file-name session-file))
         (dir (file-name-directory file)))
    (file-name-as-directory
     (expand-file-name
      (or (pi-coding-agent--session-file-cwd file)
          (locate-dominating-file dir ".git")
          dir)))))

(defun pi-coding-agent--find-session-file-buffer (session-file)
  "Return a live chat buffer currently displaying SESSION-FILE, or nil."
  (let ((target (expand-file-name session-file))
        found)
    (dolist (buffer (buffer-list) found)
      (when (and (not found)
                 (buffer-live-p buffer))
        (with-current-buffer buffer
          (when (and (derived-mode-p 'pi-coding-agent-chat-mode)
                     (let ((file (plist-get pi-coding-agent--state
                                            :session-file)))
                       (and (stringp file)
                            (equal (expand-file-name
                                    file
                                    (or (pi-coding-agent--chat-session-directory)
                                        default-directory))
                                   target))))
            (setq found buffer)))))))

;;;###autoload
(defun pi-coding-agent-session-open (dir &optional display-name)
  "Open or create a pi session in DIR with optional DISPLAY-NAME.
The normal pi chat/input UI is shown and the chat buffer is returned."
  (interactive
   (list (read-directory-name "Pi directory: " nil nil t)
         (let ((name (read-string "Session name (optional): ")))
           (and (not (string-empty-p (string-trim name))) name))))
  (pi-coding-agent--check-dependencies)
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (name (and display-name (string-trim display-name)))
         (name (and name (not (string-empty-p name)) name))
         (chat-buf (pi-coding-agent--setup-session dir name))
         (input-buf (buffer-local-value 'pi-coding-agent--input-buffer chat-buf)))
    (pi-coding-agent--display-buffers chat-buf input-buf)
    (when name
      (with-current-buffer chat-buf
        (when (and pi-coding-agent--process
                   (process-live-p pi-coding-agent--process))
          (pi-coding-agent-set-session-name name))))
    chat-buf))

(defun pi-coding-agent--session-buffer-for-dir-p (dir)
  "Return non-nil when a live chat buffer already exists for DIR."
  (let ((target (file-name-as-directory (expand-file-name dir)))
        found)
    (dolist (buffer (buffer-list) found)
      (when (and (not found)
                 (buffer-live-p buffer))
        (with-current-buffer buffer
          (when (and (derived-mode-p 'pi-coding-agent-chat-mode)
                     (equal (file-name-as-directory
                             (expand-file-name
                              (pi-coding-agent--chat-session-directory)))
                            target))
            (setq found t)))))))

(defun pi-coding-agent--session-file-buffer-session-name (session-file)
  "Return a stable buffer session name for SESSION-FILE."
  (let ((name (file-name-base (directory-file-name session-file))))
    (if (string-empty-p name) "session" name)))

;;;###autoload
(defun pi-coding-agent-session-open-file (session-file &optional dir display-name)
  "Open durable pi SESSION-FILE, optionally running pi in DIR.
When DIR is nil, a likely project directory is inferred from SESSION-FILE.
DISPLAY-NAME names the Emacs session buffer.  When another pi session is
already open for DIR, a distinct named buffer is used, matching the named
multi-session behavior of `pi-coding-agent'.  The normal pi chat/input UI is
shown and the chat buffer is returned."
  (interactive (list (read-file-name "Pi session file: " nil nil t)))
  (pi-coding-agent--check-dependencies)
  (let* ((session-file (expand-file-name session-file))
         (dir (file-name-as-directory
               (expand-file-name
                (or dir (pi-coding-agent--session-file-project-directory
                         session-file)))))
         (existing (pi-coding-agent--find-session-file-buffer session-file))
         (trimmed-name (and display-name (string-trim display-name)))
         (display-name (and trimmed-name
                            (not (string-empty-p trimmed-name))
                            trimmed-name))
         (session-name (and (not existing)
                            (or display-name
                                (and (pi-coding-agent--session-buffer-for-dir-p dir)
                                     (pi-coding-agent--session-file-buffer-session-name
                                      session-file)))))
         (chat-buf (or existing (pi-coding-agent--setup-session dir session-name)))
         (input-buf (buffer-local-value 'pi-coding-agent--input-buffer chat-buf))
         (proc (buffer-local-value 'pi-coding-agent--process chat-buf)))
    (pi-coding-agent--display-buffers chat-buf input-buf)
    (when (and (not existing)
               proc
               (process-live-p proc)
               (pi-coding-agent--session-transition-ready-p chat-buf "resume"))
      (pi-coding-agent--resume-selected-session proc chat-buf session-file))
    chat-buf))

;;;###autoload
(defun pi-coding-agent-session-file (&optional chat-buffer)
  "Return the durable session file for CHAT-BUFFER, if it exists."
  (let ((chat-buf (or chat-buffer (pi-coding-agent--get-chat-buffer))))
    (when (buffer-live-p chat-buf)
      (with-current-buffer chat-buf
        (when-let* ((file (plist-get pi-coding-agent--state :session-file))
                    ((stringp file))
                    ((not (string-empty-p file)))
                    (expanded (expand-file-name
                               file
                               (or (pi-coding-agent--chat-session-directory)
                                   default-directory)))
                    ((file-readable-p expanded)))
          expanded)))))

;;;###autoload
(defun pi-coding-agent-session-display-name (&optional chat-buffer)
  "Return the human-readable display name for CHAT-BUFFER, if known."
  (let ((chat-buf (or chat-buffer (pi-coding-agent--get-chat-buffer))))
    (and (buffer-live-p chat-buf)
         (or (buffer-local-value 'pi-coding-agent--session-name chat-buf)
             (buffer-local-value 'pi-coding-agent--canonical-session-name
                                 chat-buf)))))

;;;; Main Entry Point

(defun pi-coding-agent--setup-session (dir &optional session)
  "Set up a new or existing session for DIR with optional SESSION name.
Returns the chat buffer."
  (let* ((chat-buf (pi-coding-agent--get-or-create-buffer :chat dir session))
         (input-buf (pi-coding-agent--get-or-create-buffer :input dir session))
         (new-session nil))
    ;; Link buffers to each other
    (with-current-buffer chat-buf
      (pi-coding-agent--set-chat-session-identity dir session)
      (pi-coding-agent--set-input-buffer input-buf)
      ;; Start process if not already running
      (unless (and pi-coding-agent--process (process-live-p pi-coding-agent--process))
        (pi-coding-agent--set-process (pi-coding-agent--start-process dir))
        (setq new-session t)
        ;; Associate process with chat buffer for built-in kill confirmation
        (when (processp pi-coding-agent--process)
          (set-process-buffer pi-coding-agent--process chat-buf)
          (process-put pi-coding-agent--process 'pi-coding-agent-chat-buffer chat-buf)
          ;; Register event handler
          (pi-coding-agent--register-display-handler pi-coding-agent--process)
          ;; Initialize state from server
          (let ((buf chat-buf)
                (proc pi-coding-agent--process))  ; Capture for closures
            (pi-coding-agent--rpc-async proc '(:type "get_state")
              (lambda (response)
                (if (eq (plist-get response :success) t)
                    (progn
                      (pi-coding-agent--apply-state-response buf response)
                      ;; Check if no model available and warn user
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (unless (plist-get pi-coding-agent--state :model)
                            (pi-coding-agent--display-no-model-warning)))))
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (pi-coding-agent--display-startup-error
                       (plist-get response :error)
                       (plist-get response :stderr)))))))
            ;; Fetch commands via RPC (independent of get_state)
            (pi-coding-agent--fetch-commands proc
              (lambda (commands)
                (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (pi-coding-agent--set-commands commands)
                    (pi-coding-agent--rebuild-commands-menu))))))))
      ;; Display startup header for new sessions
      (when new-session
        (pi-coding-agent--display-startup-header)))
    (with-current-buffer input-buf
      (setq default-directory dir)
      (pi-coding-agent--set-chat-buffer chat-buf))
    chat-buf))

;;;###autoload
(defun pi-coding-agent (&optional session)
  "Start or switch to pi coding agent session in current project.
With prefix arg, prompt for SESSION name to allow multiple sessions.
If already in a pi buffer and no SESSION specified, ensures this session
is visible. When both chat and input are already shown in the current
frame, keeps layout unchanged and focuses the input window."
  (interactive
   (list (when current-prefix-arg
           (read-string "Session name: "))))
  (pi-coding-agent--check-dependencies)
  (let (chat-buf input-buf)
    (if (and (derived-mode-p 'pi-coding-agent-chat-mode 'pi-coding-agent-input-mode)
             (not session))
        ;; Already in pi buffer with no new session requested - use current session
        (setq chat-buf (pi-coding-agent--get-chat-buffer)
              input-buf (pi-coding-agent--get-input-buffer))
      ;; Find or create session for current directory
      (let ((dir (pi-coding-agent--session-directory)))
        (setq chat-buf (pi-coding-agent--setup-session dir session))
        (setq input-buf (buffer-local-value 'pi-coding-agent--input-buffer chat-buf))))
    ;; When both windows are already visible in current frame, just focus
    ;; the session input window. Otherwise restore/show the session layout.
    (if (and (get-buffer-window-list chat-buf nil)
             (get-buffer-window-list input-buf nil))
        (pi-coding-agent--focus-input-window chat-buf input-buf)
      (pi-coding-agent--display-buffers chat-buf input-buf))))

;;;###autoload
(defun pi-coding-agent-toggle ()
  "Toggle pi coding agent window visibility for the current project.
If pi windows are visible in the current frame, hide them.
If hidden there but a session exists, show them.
If no session exists, signal an error."
  (interactive)
  (pi-coding-agent--check-dependencies)
  (let* ((chat-buf (if (derived-mode-p 'pi-coding-agent-chat-mode 'pi-coding-agent-input-mode)
                       (pi-coding-agent--get-chat-buffer)
                     (car (pi-coding-agent-project-buffers))))
         (input-buf (and chat-buf
                         (buffer-local-value 'pi-coding-agent--input-buffer chat-buf))))
    (cond
     ;; No session at all
     ((null chat-buf)
      (user-error "No pi session for this project"))
     ;; Session visible in current frame: hide it
     ((or (get-buffer-window-list chat-buf nil)
          (and input-buf (get-buffer-window-list input-buf nil)))
      (with-current-buffer chat-buf
        (pi-coding-agent--hide-session-windows)))
     ;; Session hidden: show it
     (t
      (pi-coding-agent--display-buffers chat-buf input-buf)))))

(provide 'pi-coding-agent)
;;; pi-coding-agent.el ends here
