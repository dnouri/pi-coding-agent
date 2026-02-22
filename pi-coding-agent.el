;;; pi-coding-agent.el --- Emacs frontend for pi coding agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pi-coding-agent
;; Keywords: ai llm ai-pair-programming tools
;; Version: 1.3.4
;; Package-Requires: ((emacs "28.1") (markdown-mode "2.6") (transient "0.9.0"))

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
;;   - pi coding agent 0.51.3 or later, installed and in PATH
;;
;; Optional Dependencies:
;;   - phscroll: Markdown tables that exceed the window width wrap awkwardly.
;;     phscroll enables horizontal scrolling so tables stay readable.
;;     Install from: https://github.com/misohena/phscroll
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
;;     TAB            Toggle tool output
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
;; thinking level, session management, and custom commands.
;;
;; See README.org for more documentation.

;;; Code:

(require 'pi-coding-agent-menu)
(require 'pi-coding-agent-input)

;;;; Main Entry Point

(defun pi-coding-agent--setup-session (dir &optional session)
  "Set up a new or existing session for DIR with optional SESSION name.
Returns the chat buffer."
  (let* ((chat-buf (pi-coding-agent--get-or-create-buffer :chat dir session))
         (input-buf (pi-coding-agent--get-or-create-buffer :input dir session))
         (new-session nil))
    ;; Link buffers to each other
    (with-current-buffer chat-buf
      (setq default-directory dir)
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
                (pi-coding-agent--apply-state-response buf response)
                ;; Check if no model available and warn user
                (when (and (plist-get response :success)
                           (buffer-live-p buf))
                  (with-current-buffer buf
                    (unless (plist-get pi-coding-agent--state :model)
                      (pi-coding-agent--display-no-model-warning))))))
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

;;;; Performance Optimizations

;; Limit markdown backward search to prevent O(n) scanning in large buffers.
;; See `pi-coding-agent-markdown-search-limit' for details.
(advice-add 'markdown-find-previous-prop :around
            #'pi-coding-agent--limit-markdown-backward-search)

(defun pi-coding-agent-unload-function ()
  "Clean up when `pi-coding-agent' is unloaded."
  (advice-remove 'markdown-find-previous-prop
                 #'pi-coding-agent--limit-markdown-backward-search)
  nil)  ;; Return nil to continue standard unloading

(provide 'pi-coding-agent)
;;; pi-coding-agent.el ends here
