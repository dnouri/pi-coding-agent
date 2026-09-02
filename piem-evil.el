;;; piem-evil.el --- Evil keybindings for piem -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/piem

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

;; Optional Evil integration for piem, modeled on how Evil
;; and Magit cooperate: the read-only chat buffer starts in motion
;; state so navigation keys work unmodified, the input buffer starts
;; in insert state, and `?' opens the transient menu.
;;
;; This file loads automatically when a session is set up while Evil
;; is in use; set `piem-evil-integration' to nil before
;; loading piem to opt out.  It can also be loaded
;; explicitly:
;;
;;   (require 'piem-evil)
;;
;; Loading the file runs `piem-evil-setup', which is
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
;; when made after `piem-evil-setup' runs.

;;; Code:

;; Require the submodules directly rather than `piem': this
;; file may be loaded while piem.el itself is still
;; loading, and requiring the top-level feature from here would be a
;; recursive require.
(require 'piem-ui)
(require 'piem-input)
(require 'piem-menu)

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

(defcustom piem-evil-chat-state 'motion
  "Initial Evil state for pi chat buffers.
The chat buffer is read-only; motion state provides navigation keys
while unbound keys fall through to the mode's own keymap."
  :type 'symbol
  :group 'piem)

(defcustom piem-evil-input-state 'insert
  "Evil state for pi input buffers.
Used both as the initial state when an input buffer is created and as
the state entered when focusing the input window from the chat
buffer with `piem-evil-insert-input' or
`piem-evil-append-input'."
  :type 'symbol
  :group 'piem)

(defcustom piem-evil-disable-snipe t
  "When non-nil, disable `evil-snipe' in pi chat buffers.
evil-snipe's minor-mode keymaps take precedence over the chat mode's
own `f' binding (fork at point), so `piem-evil-setup'
turns the snipe minor modes off in chat buffers via
`evil-snipe-local-mode-hook' and
`evil-snipe-override-local-mode-hook'.  Without evil-snipe, fork
stays on `f' while F, t, and T remain Evil's native char-finding
motions."
  :type 'boolean
  :group 'piem)

(defcustom piem-evil-copy-raw-markdown t
  "When non-nil, yanking from the chat buffer copies raw Markdown.
`piem-evil-setup' arranges for
`piem-copy-raw-markdown' to be set buffer-locally in chat
buffers, so that `evil-yank' preserves code fences and markup.  Set
to nil before loading this file to keep the upstream default of
copying only visible text."
  :type 'boolean
  :group 'piem)

(defun piem-evil--copy-raw-markdown-in-chat ()
  "Set `piem-copy-raw-markdown' buffer-locally.
Added to `piem-chat-mode-hook' by
`piem-evil-setup' when
`piem-evil-copy-raw-markdown' is non-nil."
  (setq-local piem-copy-raw-markdown t))

(defun piem-evil--maybe-disable-snipe ()
  "Disable the `evil-snipe' minor modes in pi chat buffers.
Added to `evil-snipe-local-mode-hook' and
`evil-snipe-override-local-mode-hook' by
`piem-evil-setup' when `piem-evil-disable-snipe'
is non-nil.  Mode hooks run on disable as well as enable, so guard
on the modes being active to avoid recursing."
  (when (derived-mode-p 'piem-chat-mode)
    (when (bound-and-true-p evil-snipe-local-mode)
      (evil-snipe-local-mode -1))
    (when (bound-and-true-p evil-snipe-override-local-mode)
      (evil-snipe-override-local-mode -1))))

(defun piem-evil-insert-input ()
  "Focus the session input window and enter the configured input state.
Enter the state named by `piem-evil-input-state' (insert
by default).  Restore the session window layout when no input window
is visible."
  (interactive)
  (piem-evil--focus-input nil))

(defun piem-evil-append-input ()
  "Focus the session input window at end of buffer.
Enter the state named by `piem-evil-input-state' (insert
by default)."
  (interactive)
  (piem-evil--focus-input t))

(defun piem-evil--enter-input-state ()
  "Enter the state named by `piem-evil-input-state'."
  (evil-change-state piem-evil-input-state))

(defun piem-evil--focus-input (append)
  "Focus the session input window and enter the configured input state.
When APPEND is non-nil, move point to the end of the input buffer."
  (let ((chat-buf (piem--get-chat-buffer))
        (input-buf (piem--get-input-buffer)))
    (unless (and (buffer-live-p chat-buf) (buffer-live-p input-buf))
      (user-error "No pi session for this buffer"))
    (if-let* ((input-win (get-buffer-window input-buf)))
        (select-window input-win)
      (piem--display-buffers chat-buf input-buf))
    (when (derived-mode-p 'piem-input-mode)
      (when append
        (goto-char (point-max)))
      (piem-evil--enter-input-state))))

(defun piem-evil-close-input ()
  "Close the session input window and select the chat window."
  (interactive)
  (when-let* ((input-buf (piem--get-input-buffer))
              (input-win (get-buffer-window input-buf)))
    (when (window-parent input-win)
      (delete-window input-win)
      (when-let* ((chat-buf (piem--get-chat-buffer))
                  (chat-win (get-buffer-window chat-buf)))
        (select-window chat-win)))))

;;;###autoload
(defun piem-evil-setup ()
  "Set up Evil integration for piem.
Set initial buffer states, install keybindings, and apply the user
options `piem-evil-chat-state',
`piem-evil-input-state',
`piem-evil-copy-raw-markdown', and
`piem-evil-disable-snipe'.  Safe to call more than once."
  (interactive)
  (unless (featurep 'evil)
    (user-error "piem-evil: Evil is not loaded"))
  (evil-set-initial-state 'piem-chat-mode
                          piem-evil-chat-state)
  (evil-set-initial-state 'piem-input-mode
                          piem-evil-input-state)
  (evil-define-key* 'motion piem-chat-mode-map
    "n" #'piem-next-message
    "p" #'piem-previous-message
    "f" #'piem-fork-at-point
    "?" #'piem-menu
    "q" #'piem-quit
    "i" #'piem-evil-insert-input
    "a" #'piem-evil-append-input
    (kbd "RET") #'piem-visit-file
    (kbd "TAB") #'piem-toggle-tool-section
    [tab] #'piem-toggle-tool-section)
  (evil-define-key* 'normal piem-input-mode-map
    (kbd "RET") #'piem-send
    "q" #'piem-evil-close-input
    "?" #'piem-menu)
  (when piem-evil-copy-raw-markdown
    (add-hook 'piem-chat-mode-hook
              #'piem-evil--copy-raw-markdown-in-chat))
  (when piem-evil-disable-snipe
    (add-hook 'evil-snipe-local-mode-hook
              #'piem-evil--maybe-disable-snipe)
    (add-hook 'evil-snipe-override-local-mode-hook
              #'piem-evil--maybe-disable-snipe)))

;; Activate on load when Evil is present.
(when (featurep 'evil)
  (piem-evil-setup))

(provide 'piem-evil)
;;; piem-evil.el ends here
