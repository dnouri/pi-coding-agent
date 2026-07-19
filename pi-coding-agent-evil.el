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
;; This file loads automatically when Evil is in use; set
;; `pi-coding-agent-evil-integration' to nil before loading
;; pi-coding-agent to opt out.  It can also be loaded explicitly:
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
;; file is also loaded from a `with-eval-after-load' form at the end
;; of pi-coding-agent.el, and requiring the top-level feature from
;; here would be a recursive require during that load.
(require 'pi-coding-agent-ui)
(require 'pi-coding-agent-input)
(require 'pi-coding-agent-menu)
(require 'evil)

;; Optional dependency, registered with when it loads.
(defvar evil-snipe-disabled-modes)

(defcustom pi-coding-agent-evil-chat-state 'motion
  "Initial Evil state for pi chat buffers.
The chat buffer is read-only; motion state provides navigation keys
while unbound keys fall through to the mode's own keymap."
  :type 'symbol
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-evil-input-state 'insert
  "Initial Evil state for pi input buffers."
  :type 'symbol
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-evil-disable-snipe t
  "When non-nil, disable `evil-snipe' in pi chat buffers.
evil-snipe's minor-mode keymaps take precedence over the chat mode's
own `f' binding (fork at point), so `pi-coding-agent-evil-setup' adds
`pi-coding-agent-chat-mode' to `evil-snipe-disabled-modes' once
evil-snipe loads; the same treatment `magit-mode' receives by
default.  Only affects chat buffers created afterwards.  Without
evil-snipe, fork stays on `f' while F, t, and T remain Evil's native
char-finding motions."
  :type 'boolean
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-evil-copy-raw-markdown t
  "When non-nil, yanking from the chat buffer copies raw Markdown.
`pi-coding-agent-evil-setup' sets `pi-coding-agent-copy-raw-markdown'
to t when this option is non-nil, so that `evil-yank' preserves code
fences and markup.  Set to nil before loading this file to keep the
upstream default of copying only visible text."
  :type 'boolean
  :group 'pi-coding-agent)

(defun pi-coding-agent-evil-insert-input ()
  "Focus the session input window and enter insert state.
Restore the session window layout when no input window is visible."
  (interactive)
  (pi-coding-agent-evil--focus-input nil))

(defun pi-coding-agent-evil-append-input ()
  "Focus the session input window at end of buffer and enter insert state."
  (interactive)
  (pi-coding-agent-evil--focus-input t))

(defun pi-coding-agent-evil--focus-input (append)
  "Focus the session input window and enter insert state.
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
      (evil-insert-state))))

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
`pi-coding-agent-evil-input-state', and
`pi-coding-agent-evil-copy-raw-markdown'.  Safe to call more than
once."
  (interactive)
  (require 'evil)
  (evil-set-initial-state 'pi-coding-agent-chat-mode
                          pi-coding-agent-evil-chat-state)
  (evil-set-initial-state 'pi-coding-agent-input-mode
                          pi-coding-agent-evil-input-state)
  (evil-define-key 'motion pi-coding-agent-chat-mode-map
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
  (evil-define-key 'normal pi-coding-agent-input-mode-map
    (kbd "RET") #'pi-coding-agent-send
    "q" #'pi-coding-agent-evil-close-input
    "?" #'pi-coding-agent-menu)
  (when pi-coding-agent-evil-copy-raw-markdown
    (setq pi-coding-agent-copy-raw-markdown t))
  (when pi-coding-agent-evil-disable-snipe
    (with-eval-after-load 'evil-snipe
      (add-to-list 'evil-snipe-disabled-modes 'pi-coding-agent-chat-mode))))

;; Activate on load.
(pi-coding-agent-evil-setup)

(provide 'pi-coding-agent-evil)
;;; pi-coding-agent-evil.el ends here
