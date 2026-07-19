;;; pi-coding-agent-evil.el --- Evil keybindings for pi-coding-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pi-coding-agent

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

;; Optional Evil integration for pi-coding-agent, modeled on how Evil
;; and Magit cooperate: the read-only chat buffer starts in motion
;; state so navigation keys work unmodified, the input buffer starts
;; in insert state, and `?' opens the transient menu.
;;
;; This file loads automatically when a session is set up while Evil
;; is in use; set `pi-coding-agent-evil-integration' to nil before
;; loading pi-coding-agent to opt out.  It can also be loaded
;; explicitly:
;;
;;   (require 'pi-coding-agent-evil)
;;
;; Loading the file runs `pi-coding-agent-evil-setup', which is
;; idempotent and can also be called interactively to re-apply the
;; configuration after changing the user options below.
;;
;; Chat buffer (motion state):
;;
;;   n / p   next / previous message (like Magit's section motion)
;;   f       fork session at point
;;   TAB     toggle tool/thinking section
;;   RET     visit file at point
;;   i / a   focus input window (append moves to end of input)
;;   ?       transient menu
;;   q       quit session
;;
;; Input buffer (normal state):
;;
;;   RET     send
;;   q       close input window
;;   ?       transient menu
;;
;; All bindings are registered with `evil-define-key' on the mode
;; keymaps, so user bindings via the same mechanism take precedence
;; when made after `pi-coding-agent-evil-setup' runs.

;;; Code:

;; Require the submodules directly rather than `pi-coding-agent': this
;; file may be loaded while pi-coding-agent.el itself is still
;; loading, and requiring the top-level feature from here would be a
;; recursive require.
(require 'pi-coding-agent-ui)
(require 'pi-coding-agent-input)
(require 'pi-coding-agent-menu)

;; Evil is an optional dependency: this file must byte-compile and
;; load in environments where Evil is not installed (e.g. MELPA
;; builds), and only activates when Evil is present.  Call the
;; function `evil-define-key*' rather than the `evil-define-key'
;; macro so byte-compiled output is correct regardless of whether
;; Evil was present at compile time.
(require 'evil nil t)

(declare-function evil-change-state "evil")
(declare-function evil-define-key* "evil")
(declare-function evil-set-initial-state "evil")
(declare-function evil-snipe-local-mode "evil-snipe")
(declare-function evil-snipe-override-local-mode "evil-snipe")

;; Note on the evil-snipe references below: hook variables are only
;; ever quoted, and mode variables are tested with
;; `bound-and-true-p', so nothing needs declaring here.  Adding to
;; the hooks before evil-snipe loads is safe: the hooks exist as soon
;; as `add-hook' creates them, and `defvar' in evil-snipe does not
;; reset an already-bound variable.

(defcustom pi-coding-agent-evil-chat-state 'motion
  "Initial Evil state for pi chat buffers.
The chat buffer is read-only; motion state provides navigation keys
while unbound keys fall through to the mode's own keymap."
  :type 'symbol
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-evil-input-state 'insert
  "Evil state for pi input buffers.
Used both as the initial state when an input buffer is created and as
the state entered when focusing the input window from the chat
buffer with `pi-coding-agent-evil-insert-input' or
`pi-coding-agent-evil-append-input'."
  :type 'symbol
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-evil-disable-snipe t
  "When non-nil, disable `evil-snipe' in pi chat buffers.
evil-snipe's minor-mode keymaps take precedence over the chat mode's
own `f' binding (fork at point), so `pi-coding-agent-evil-setup'
turns the snipe minor modes off in chat buffers via
`evil-snipe-local-mode-hook' and
`evil-snipe-override-local-mode-hook'.  Without evil-snipe, fork
stays on `f' while F, t, and T remain Evil's native char-finding
motions."
  :type 'boolean
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-evil-copy-raw-markdown t
  "When non-nil, yanking from the chat buffer copies raw Markdown.
`pi-coding-agent-evil-setup' arranges for
`pi-coding-agent-copy-raw-markdown' to be set buffer-locally in chat
buffers, so that `evil-yank' preserves code fences and markup.  Set
to nil before loading this file to keep the upstream default of
copying only visible text."
  :type 'boolean
  :group 'pi-coding-agent)

(defun pi-coding-agent-evil--copy-raw-markdown-in-chat ()
  "Set `pi-coding-agent-copy-raw-markdown' buffer-locally.
Added to `pi-coding-agent-chat-mode-hook' by
`pi-coding-agent-evil-setup' when
`pi-coding-agent-evil-copy-raw-markdown' is non-nil."
  (setq-local pi-coding-agent-copy-raw-markdown t))

(defun pi-coding-agent-evil--maybe-disable-snipe ()
  "Disable the `evil-snipe' minor modes in pi chat buffers.
Added to `evil-snipe-local-mode-hook' and
`evil-snipe-override-local-mode-hook' by
`pi-coding-agent-evil-setup' when `pi-coding-agent-evil-disable-snipe'
is non-nil.  Mode hooks run on disable as well as enable, so guard
on the modes being active to avoid recursing."
  (when (derived-mode-p 'pi-coding-agent-chat-mode)
    (when (bound-and-true-p evil-snipe-local-mode)
      (evil-snipe-local-mode -1))
    (when (bound-and-true-p evil-snipe-override-local-mode)
      (evil-snipe-override-local-mode -1))))

(defun pi-coding-agent-evil-insert-input ()
  "Focus the session input window and enter the configured input state.
Enter the state named by `pi-coding-agent-evil-input-state' (insert
by default).  Restore the session window layout when no input window
is visible."
  (interactive)
  (pi-coding-agent-evil--focus-input nil))

(defun pi-coding-agent-evil-append-input ()
  "Focus the session input window at end of buffer.
Enter the state named by `pi-coding-agent-evil-input-state' (insert
by default)."
  (interactive)
  (pi-coding-agent-evil--focus-input t))

(defun pi-coding-agent-evil--enter-input-state ()
  "Enter the state named by `pi-coding-agent-evil-input-state'."
  (evil-change-state pi-coding-agent-evil-input-state))

(defun pi-coding-agent-evil--focus-input (append)
  "Focus the session input window and enter the configured input state.
When APPEND is non-nil, move point to the end of the input buffer."
  (let ((chat-buf (pi-coding-agent--get-chat-buffer))
        (input-buf (pi-coding-agent--get-input-buffer)))
    (unless (and (buffer-live-p chat-buf) (buffer-live-p input-buf))
      (user-error "No pi session for this buffer"))
    (if-let* ((input-win (get-buffer-window input-buf)))
        (select-window input-win)
      (pi-coding-agent--display-buffers chat-buf input-buf))
    (when (derived-mode-p 'pi-coding-agent-input-mode)
      (when append
        (goto-char (point-max)))
      (pi-coding-agent-evil--enter-input-state))))

(defun pi-coding-agent-evil-close-input ()
  "Close the session input window and select the chat window."
  (interactive)
  (when-let* ((input-buf (pi-coding-agent--get-input-buffer))
              (input-win (get-buffer-window input-buf)))
    (when (window-parent input-win)
      (delete-window input-win)
      (when-let* ((chat-buf (pi-coding-agent--get-chat-buffer))
                  (chat-win (get-buffer-window chat-buf)))
        (select-window chat-win)))))

;;;###autoload
(defun pi-coding-agent-evil-setup ()
  "Set up Evil integration for pi-coding-agent.
Set initial buffer states, install keybindings, and apply the user
options `pi-coding-agent-evil-chat-state',
`pi-coding-agent-evil-input-state',
`pi-coding-agent-evil-copy-raw-markdown', and
`pi-coding-agent-evil-disable-snipe'.  Safe to call more than once."
  (interactive)
  (unless (featurep 'evil)
    (user-error "pi-coding-agent-evil: Evil is not loaded"))
  (evil-set-initial-state 'pi-coding-agent-chat-mode
                          pi-coding-agent-evil-chat-state)
  (evil-set-initial-state 'pi-coding-agent-input-mode
                          pi-coding-agent-evil-input-state)
  (evil-define-key* 'motion pi-coding-agent-chat-mode-map
    "n" #'pi-coding-agent-next-message
    "p" #'pi-coding-agent-previous-message
    "f" #'pi-coding-agent-fork-at-point
    "?" #'pi-coding-agent-menu
    "q" #'pi-coding-agent-quit
    "i" #'pi-coding-agent-evil-insert-input
    "a" #'pi-coding-agent-evil-append-input
    (kbd "RET") #'pi-coding-agent-visit-file
    (kbd "TAB") #'pi-coding-agent-toggle-tool-section
    [tab] #'pi-coding-agent-toggle-tool-section)
  (evil-define-key* 'normal pi-coding-agent-input-mode-map
    (kbd "RET") #'pi-coding-agent-send
    "q" #'pi-coding-agent-evil-close-input
    "?" #'pi-coding-agent-menu)
  (when pi-coding-agent-evil-copy-raw-markdown
    (add-hook 'pi-coding-agent-chat-mode-hook
              #'pi-coding-agent-evil--copy-raw-markdown-in-chat))
  (when pi-coding-agent-evil-disable-snipe
    (add-hook 'evil-snipe-local-mode-hook
              #'pi-coding-agent-evil--maybe-disable-snipe)
    (add-hook 'evil-snipe-override-local-mode-hook
              #'pi-coding-agent-evil--maybe-disable-snipe)))

;; Activate on load when Evil is present.
(when (featurep 'evil)
  (pi-coding-agent-evil-setup))

(provide 'pi-coding-agent-evil)
;;; pi-coding-agent-evil.el ends here
