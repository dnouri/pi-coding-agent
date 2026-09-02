;;; pi-coding-agent.el --- Deprecated aliases for piem -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Version: 3.0.0
;; Package-Requires: ((emacs "29.1"))

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

;; Compatibility shim for configurations predating the rename of
;; pi-coding-agent to piem (v3.0.0).  Requiring this feature loads piem
;; and defines deprecated aliases under the old names.  Everything
;; declared here is gone in 4.0.

;;; Code:

(require 'piem)

;;;; Command aliases

;;;###autoload
(define-obsolete-function-alias 'pi-coding-agent 'piem "3.0")
;;;###autoload
(define-obsolete-function-alias 'pi-coding-agent-toggle 'piem-toggle "3.0")
;;;###autoload
(define-obsolete-function-alias 'pi-coding-agent-open-session-file 'piem-open-session-file "3.0")
;;;###autoload
(define-obsolete-function-alias 'pi-coding-agent-new-session 'piem-new-session "3.0")
;;;###autoload
(define-obsolete-function-alias 'pi-coding-agent-reload 'piem-reload "3.0")
;;;###autoload
(define-obsolete-function-alias 'pi-coding-agent-session-browser 'piem-session-browser "3.0")
;;;###autoload
(define-obsolete-function-alias 'pi-coding-agent-tree-browser 'piem-tree-browser "3.0")
;;;###autoload
(define-obsolete-function-alias 'pi-coding-agent-install-grammars 'piem-install-grammars "3.0")
;;;###autoload
(define-obsolete-function-alias 'pi-coding-agent-attach-image 'piem-attach-image "3.0")
;;;###autoload
(define-obsolete-function-alias 'pi-coding-agent-evil-setup 'piem-evil-setup "3.0")

;;;; Variable aliases

;; User configurations that set an old variable name before this stub
;; loads would otherwise lose the setting: `defvaralias' overwrites an
;; existing old-name binding (with a warning) instead of transferring
;; it, and Customize entries queued on the old symbol never apply to
;; the new one.  Migrate each setting before aliasing so `setq' values
;; and queued `custom-set-variables' entries survive the rename, then
;; leave the old symbol unbound so `defvaralias' stays silent.  A
;; piem-name value set by the user wins over the old-name value.

(defun pi-coding-agent--untouched-default-p (variable)
  "Return non-nil if VARIABLE still has its defcustom default value.
VARIABLE counts as untouched when its default value is absent (the
defcustom has not run yet) or still equal to the standard value
recorded by `defcustom'."
  (let ((standard (get variable 'standard-value)))
    (if standard
        (and (default-boundp variable)
             (equal (default-value variable)
                    (eval (car standard) t)))
      (not (default-boundp variable)))))
(defun pi-coding-agent--migrated-value (old-name)
  "Return the user setting stored under OLD-NAME as a list, or nil.
Cover plain bindings set under the old name and Customize entries
queued by `custom-set-variables', which land as `saved-value' and
`theme-value' properties rather than as bindings while the
variable is still undefined."
  (cond ((default-boundp old-name)
         (list (default-value old-name)))
        ((get old-name 'saved-value)
         (list (eval (car (get old-name 'saved-value)) t)))
        ((assq 'user (get old-name 'theme-value))
         (list (eval (nth 2 (assq 'user (get old-name 'theme-value)))
                     t)))))

(defun pi-coding-agent--migrate-old-setting (old-name new-name)
  "Move a user setting from OLD-NAME to NEW-NAME before aliasing.
If NEW-NAME was itself set away from its defcustom default, the
new-name setting wins and the old one is discarded.  Otherwise the
old value and any queued Customize state under the old name move
to NEW-NAME.  OLD-NAME is left unbound so the subsequent
`define-obsolete-variable-alias' neither warns nor clobbers."
  (let ((value (pi-coding-agent--migrated-value old-name)))
    (when value
      (if (pi-coding-agent--untouched-default-p new-name)
          (progn
            (set-default new-name (car value))
            (dolist (prop '(saved-value saved-variable-comment
                                        theme-value))
              (when (get old-name prop)
                (put new-name prop (get old-name prop))
                (put old-name prop nil))))
        ;; NEW-NAME carries an explicit setting; drop the orphaned
        ;; old-name Customize state.
        (dolist (prop '(saved-value saved-variable-comment theme-value))
          (put old-name prop nil)))
      (makunbound old-name))))

(pi-coding-agent--migrate-old-setting
 'pi-coding-agent-executable 'piem-executable)
(define-obsolete-variable-alias 'pi-coding-agent-executable 'piem-executable "3.0")
(pi-coding-agent--migrate-old-setting
 'pi-coding-agent-project-trust-policy 'piem-project-trust-policy)
(define-obsolete-variable-alias 'pi-coding-agent-project-trust-policy 'piem-project-trust-policy "3.0")
(pi-coding-agent--migrate-old-setting
 'pi-coding-agent-rpc-timeout 'piem-rpc-timeout)
(define-obsolete-variable-alias 'pi-coding-agent-rpc-timeout 'piem-rpc-timeout "3.0")
(pi-coding-agent--migrate-old-setting
 'pi-coding-agent-input-window-height 'piem-input-window-height)
(define-obsolete-variable-alias 'pi-coding-agent-input-window-height 'piem-input-window-height "3.0")
(pi-coding-agent--migrate-old-setting
 'pi-coding-agent-quit-without-confirmation
 'piem-quit-without-confirmation)
(define-obsolete-variable-alias 'pi-coding-agent-quit-without-confirmation 'piem-quit-without-confirmation "3.0")
(pi-coding-agent--migrate-old-setting
 'pi-coding-agent-evil-integration 'piem-evil-integration)
(define-obsolete-variable-alias 'pi-coding-agent-evil-integration 'piem-evil-integration "3.0")
(pi-coding-agent--migrate-old-setting
 'pi-coding-agent-evil-chat-state 'piem-evil-chat-state)
(define-obsolete-variable-alias 'pi-coding-agent-evil-chat-state 'piem-evil-chat-state "3.0")
(pi-coding-agent--migrate-old-setting
 'pi-coding-agent-evil-input-state 'piem-evil-input-state)
(define-obsolete-variable-alias 'pi-coding-agent-evil-input-state 'piem-evil-input-state "3.0")

;;;; Face aliases

(define-obsolete-face-alias 'pi-coding-agent-timestamp 'piem-timestamp "3.0")
(define-obsolete-face-alias 'pi-coding-agent-tool-name 'piem-tool-name "3.0")
(define-obsolete-face-alias 'pi-coding-agent-tool-command 'piem-tool-command "3.0")
(define-obsolete-face-alias 'pi-coding-agent-tool-output 'piem-tool-output "3.0")
(define-obsolete-face-alias 'pi-coding-agent-tool-block 'piem-tool-block "3.0")
(define-obsolete-face-alias 'pi-coding-agent-tool-block-error 'piem-tool-block-error "3.0")
(define-obsolete-face-alias 'pi-coding-agent-diff-line-added 'piem-diff-line-added "3.0")
(define-obsolete-face-alias 'pi-coding-agent-diff-line-removed 'piem-diff-line-removed "3.0")
(define-obsolete-face-alias 'pi-coding-agent-collapsed-indicator 'piem-collapsed-indicator "3.0")
(define-obsolete-face-alias 'pi-coding-agent-model-name 'piem-model-name "3.0")
(define-obsolete-face-alias 'pi-coding-agent-activity-phase 'piem-activity-phase "3.0")
(define-obsolete-face-alias 'pi-coding-agent-retry-notice 'piem-retry-notice "3.0")
(define-obsolete-face-alias 'pi-coding-agent-error-notice 'piem-error-notice "3.0")
(define-obsolete-face-alias 'pi-coding-agent-session-name 'piem-session-name "3.0")
(define-obsolete-face-alias 'pi-coding-agent-session-message-count 'piem-session-message-count "3.0")
(define-obsolete-face-alias 'pi-coding-agent-session-age 'piem-session-age "3.0")
(define-obsolete-face-alias 'pi-coding-agent-session-thread-connector 'piem-session-thread-connector "3.0")
(define-obsolete-face-alias 'pi-coding-agent-session-group-header 'piem-session-group-header "3.0")
(define-obsolete-face-alias 'pi-coding-agent-tree-user 'piem-tree-user "3.0")
(define-obsolete-face-alias 'pi-coding-agent-tree-assistant 'piem-tree-assistant "3.0")
(define-obsolete-face-alias 'pi-coding-agent-tree-tool 'piem-tree-tool "3.0")
(define-obsolete-face-alias 'pi-coding-agent-tree-compaction 'piem-tree-compaction "3.0")
(define-obsolete-face-alias 'pi-coding-agent-tree-summary 'piem-tree-summary "3.0")
(define-obsolete-face-alias 'pi-coding-agent-tree-active 'piem-tree-active "3.0")
(define-obsolete-face-alias 'pi-coding-agent-tree-label 'piem-tree-label "3.0")
(define-obsolete-face-alias 'pi-coding-agent-tree-connector 'piem-tree-connector "3.0")

;;;; Customize group

(defgroup pi-coding-agent nil
  "Deprecated alias for the `piem' group."
  :group 'piem)

(provide 'pi-coding-agent)
;;; pi-coding-agent.el ends here
