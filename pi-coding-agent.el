;;; pi-coding-agent.el --- Emacs frontend for pi coding agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pi-coding-agent
;; Keywords: ai llm ai-pair-programming tools
;; Version: 2.5.2
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
;;   - pi coding agent @earendil-works/pi-coding-agent 0.79.1 or later,
;;     installed and in PATH on the host where Pi runs
;;   - tree-sitter grammars for markdown and markdown-inline
;;
;; pi-coding-agent uses `md-ts-mode` for its own chat and input buffers;
;; loading it does not change global Markdown file associations.
;;
;; Usage:
;;   M-x pi-coding-agent                    Start or focus session in current project
;;   C-u M-x pi-coding-agent                Start a named session
;;   M-x pi-coding-agent-open-session-file  Open a JSONL session file as live session
;;   M-x pi-coding-agent-toggle             Hide/show session windows in current frame
;;
;; Many users define an alias: (defalias 'pi 'pi-coding-agent)
;;
;; Key Bindings:
;;   Input buffer:
;;     C-c C-c        Send prompt (queues as follow-up if busy)
;;     C-c C-s        Queue steering (interrupts after current tool; busy only)
;;     C-c C-k        Abort current operation
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

(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))

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
        (pi-coding-agent--check-dependencies dir)
        (pi-coding-agent--set-process (pi-coding-agent--start-process dir))
        (setq new-session t)
        ;; Associate process events and ownership with this chat buffer.
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
                    (pi-coding-agent--rebuild-commands-menu))))
              dir))))
      ;; Display startup header for new sessions
      (when new-session
        (pi-coding-agent--display-startup-header)))
    (with-current-buffer input-buf
      (setq default-directory dir)
      (pi-coding-agent--set-chat-buffer chat-buf))
    chat-buf))

(defun pi-coding-agent--show-session-buffers (chat-buf input-buf)
  "Show CHAT-BUF and INPUT-BUF, focusing input when both are visible."
  (if (and (get-buffer-window-list chat-buf nil)
           (get-buffer-window-list input-buf nil))
      (pi-coding-agent--focus-input-window chat-buf input-buf)
    (pi-coding-agent--display-buffers chat-buf input-buf)))

(defun pi-coding-agent--dired-regular-file-at-point ()
  "Return Dired's regular file at point, or nil."
  (when (derived-mode-p 'dired-mode)
    (when-let* ((file (dired-get-filename nil t)))
      (and (file-regular-p file)
           (pi-coding-agent--route-preserving-expand-file-name file)))))

(defun pi-coding-agent--regular-jsonl-file-p (file)
  "Return non-nil if FILE is a cheap local JSONL file candidate."
  (when (stringp file)
    (let ((path (expand-file-name file)))
      (and (string-suffix-p ".jsonl" path)
           (not (file-remote-p path))
           (ignore-errors
             (and (file-regular-p path)
                  (file-readable-p path)))))))

(defun pi-coding-agent--visited-jsonl-file-prompt-default ()
  "Return the current buffer's visited JSONL file for the prompt, or nil."
  (when-let* ((file buffer-file-name)
              (path (expand-file-name file)))
    (and (pi-coding-agent--regular-jsonl-file-p path)
         path)))

(defun pi-coding-agent--session-file-prompt-default ()
  "Return an explicit default file for the session-file prompt, or nil."
  (if (derived-mode-p 'dired-mode)
      (pi-coding-agent--dired-regular-file-at-point)
    (pi-coding-agent--visited-jsonl-file-prompt-default)))

(defun pi-coding-agent--read-session-file-name ()
  "Read an existing pi session file name from the minibuffer."
  (let* ((default-file (pi-coding-agent--session-file-prompt-default))
         (default-dir (and default-file
                           (pi-coding-agent--route-preserving-file-name-directory
                            default-file)))
         (initial (and default-file (file-name-nondirectory default-file)))
         ;; `read-file-name' otherwise uses the current buffer's visited file
         ;; as a hidden default when DEFAULT-FILENAME and INITIAL are nil.
         (buffer-file-name nil))
    (read-file-name "Pi session file: "
                    default-dir
                    default-file
                    t
                    initial)))

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
    (pi-coding-agent--show-session-buffers chat-buf input-buf)))

;;;###autoload
(defun pi-coding-agent-open-session-file (session-file)
  "Open pi JSONL SESSION-FILE as a live session.
This uses the normal chat/input UI and switches pi to SESSION-FILE; it is not a
static viewer.  The session header must record a non-empty absolute cwd that
names an existing directory.  Interactively, prompt for an existing file.  In
Dired, default to the regular file at point; otherwise, default to the current
visited local regular readable .jsonl file when there is one."
  (interactive (list (pi-coding-agent--read-session-file-name)))
  (let* ((session-file (pi-coding-agent--route-preserving-expand-file-name
                        session-file))
         (dir (pi-coding-agent--session-file-cwd-or-error session-file)))
    (pi-coding-agent--check-dependencies dir)
    (let* ((chat-buf (pi-coding-agent--setup-session dir))
           (input-buf (buffer-local-value 'pi-coding-agent--input-buffer
                                          chat-buf))
           (proc (buffer-local-value 'pi-coding-agent--process chat-buf)))
      (pi-coding-agent--show-session-buffers chat-buf input-buf)
      (when (pi-coding-agent--session-transition-ready-p chat-buf "open")
        (pi-coding-agent--resume-selected-session proc chat-buf session-file))
      chat-buf)))

;;;###autoload
(defun pi-coding-agent-toggle ()
  "Toggle pi coding agent window visibility for the current project.
If pi windows are visible in the current frame, hide them.
If hidden there but a session exists, show them.
If no session exists, signal an error."
  (interactive)
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
