;;; pi-coding-agent-grammars.el --- Tree-sitter grammar recipes  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
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

;; Tree-sitter grammar recipes and installation helpers for Emacs 29+.
;;
;; All built-in `-ts-mode's (python-ts-mode, bash-ts-mode, etc.) are
;; available on Emacs 29, 30, and 31.  The modes are there; the
;; *grammars* (compiled .so files) are what users need to install.
;;
;; On Emacs 29/30, `treesit-language-source-alist' starts empty — users
;; have no way to install grammars without knowing Git URLs.  This file
;; registers version-pinned recipes so `treesit-install-language-grammar'
;; works out of the box.
;;
;; On Emacs 31, `treesit-ensure-installed' handles grammars natively for
;; modes that register recipes.  Our entries serve as fallbacks for modes
;; that don't (add-to-list with APPEND preserves user/mode entries).

;;; Code:

(require 'treesit)
(require 'seq)

(defvar pi-coding-agent-grammar-recipes
  '((python      "https://github.com/tree-sitter/tree-sitter-python"       "v0.23.6")
    (javascript  "https://github.com/tree-sitter/tree-sitter-javascript"   "v0.23.1")
    (typescript  "https://github.com/tree-sitter/tree-sitter-typescript"   "v0.23.2" "typescript/src")
    (tsx         "https://github.com/tree-sitter/tree-sitter-typescript"   "v0.23.2" "tsx/src")
    (bash        "https://github.com/tree-sitter/tree-sitter-bash"         "v0.23.3")
    (json        "https://github.com/tree-sitter/tree-sitter-json"         "v0.24.8")
    (yaml        "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.0")
    (c           "https://github.com/tree-sitter/tree-sitter-c"            "v0.23.5")
    (cpp         "https://github.com/tree-sitter/tree-sitter-cpp"          "v0.23.4")
    (rust        "https://github.com/tree-sitter/tree-sitter-rust"         "v0.23.2")
    (go          "https://github.com/tree-sitter/tree-sitter-go"           "v0.23.4")
    (ruby        "https://github.com/tree-sitter/tree-sitter-ruby"         "v0.23.1")
    (css         "https://github.com/tree-sitter/tree-sitter-css"          "v0.23.2")
    (html        "https://github.com/tree-sitter/tree-sitter-html"         "v0.23.2")
    (java        "https://github.com/tree-sitter/tree-sitter-java"         "v0.23.5")
    (lua         "https://github.com/tree-sitter-grammars/tree-sitter-lua" "v0.3.0")
    (toml        "https://github.com/tree-sitter-grammars/tree-sitter-toml" "v0.7.0")
    (cmake       "https://github.com/uyha/tree-sitter-cmake"               "v0.5.0")
    (dockerfile  "https://github.com/camdencheek/tree-sitter-dockerfile"   "v0.2.0")
    (c-sharp     "https://github.com/tree-sitter/tree-sitter-c-sharp"      "v0.23.1")
    (elixir      "https://github.com/elixir-lang/tree-sitter-elixir"       "v0.3.4")
    (heex        "https://github.com/phoenixframework/tree-sitter-heex"    "v0.8.0")
    (gomod       "https://github.com/camdencheek/tree-sitter-go-mod"       "v1.1.0")
    (php         "https://github.com/tree-sitter/tree-sitter-php"          "v0.23.11" "php/src"))
  "Tree-sitter grammar recipes for code block languages.
Each entry is (LANG URL REVISION [SOURCE-DIR]).  Covers all
languages with built-in tree-sitter modes in Emacs 29+.")

;; Register recipes so `M-x treesit-install-language-grammar' works
;; on Emacs 29/30 (which ship with zero recipes).  Use APPEND so user
;; entries and Emacs 31 mode-registered entries take precedence.
(dolist (recipe pi-coding-agent-grammar-recipes)
  (add-to-list 'treesit-language-source-alist recipe t))

;;;; Detection

(defconst pi-coding-agent--essential-grammars '(markdown markdown-inline)
  "Grammars required for the chat buffer to render properly.")

(defun pi-coding-agent--missing-essential-grammars ()
  "Return list of essential grammars that are not installed."
  (seq-filter (lambda (lang)
                (not (treesit-language-available-p lang)))
              pi-coding-agent--essential-grammars))

(defun pi-coding-agent--missing-optional-grammars ()
  "Return list of recipe grammars that are not installed."
  (seq-filter (lambda (lang)
                (not (treesit-language-available-p lang)))
              (mapcar #'car pi-coding-agent-grammar-recipes)))

(defun pi-coding-agent--installed-optional-grammars ()
  "Return list of recipe grammars that are installed."
  (seq-filter #'treesit-language-available-p
              (mapcar #'car pi-coding-agent-grammar-recipes)))

;;;; Installation

(defun pi-coding-agent--install-grammars (grammars)
  "Install tree-sitter GRAMMARS, showing progress.
Returns the number of successfully installed grammars."
  (let ((total (length grammars))
        (idx 0))
    (condition-case err
        (dolist (lang grammars)
          (cl-incf idx)
          (message "[pi-coding-agent] Installing grammar %d/%d: %s..."
                   idx total lang)
          (treesit-install-language-grammar lang))
      (error
       (display-warning
        'pi-coding-agent
        (format "Failed to install grammar `%s': %s\n\
A C compiler (gcc or cc) is required.\n\
%d/%d grammars installed before failure."
                (nth (1- idx) grammars)
                (error-message-string err)
                (1- idx) total)
        :error)
       (setq idx (1- idx))))
    (when (> idx 0)
      (message "[pi-coding-agent] Installed %d/%d grammars." idx total))
    idx))

(defun pi-coding-agent--maybe-install-essential-grammars ()
  "Install essential grammars (markdown, markdown-inline) if missing.
These are required for the chat buffer — installs without prompting.
Warns clearly on failure."
  (unless noninteractive
    (let ((missing (pi-coding-agent--missing-essential-grammars)))
      (when missing
        (message "[pi-coding-agent] Installing essential grammars: %s"
                 (mapconcat #'symbol-name missing ", "))
        (let ((installed (pi-coding-agent--install-grammars missing)))
          (when (< installed (length missing))
            (display-warning
             'pi-coding-agent
             "Essential tree-sitter grammars (markdown, markdown-inline) \
could not be installed.\nThe chat buffer will not render properly.\n\
A C compiler is required.  Install gcc and restart Emacs."
             :error)))))))

(defcustom pi-coding-agent-grammar-declined-set nil
  "Grammars that were missing when the user declined installation.
When non-nil, the optional grammar prompt is suppressed unless new
grammars appear that are not in this set.  Managed automatically;
reset by setting to nil."
  :type '(repeat symbol)
  :group 'pi-coding-agent)

(defvar pi-coding-agent--grammar-prompt-done nil
  "Non-nil when the optional grammar prompt has been shown this session.")

(defun pi-coding-agent--new-missing-grammars ()
  "Return missing grammars not covered by a previous decline.
If the user never declined, all missing grammars are \"new\".
If they declined, only grammars not in the declined set are new."
  (let ((missing (pi-coding-agent--missing-optional-grammars)))
    (if pi-coding-agent-grammar-declined-set
        (seq-remove (lambda (g) (memq g pi-coding-agent-grammar-declined-set))
                    missing)
      missing)))

(defun pi-coding-agent--maybe-install-optional-grammars ()
  "Offer to install optional grammars for code highlighting.
Prompts at most once per Emacs session.  A decline is persisted: the
set of missing grammars is saved so the prompt does not reappear.
The prompt returns when new grammars are added to the recipe list
or the declined set is cleared."
  (unless (or noninteractive
              pi-coding-agent--grammar-prompt-done)
    (setq pi-coding-agent--grammar-prompt-done t)
    (let ((missing (pi-coding-agent--missing-optional-grammars))
          (new (pi-coding-agent--new-missing-grammars)))
      (when new
        (if (y-or-n-p
             (format "%d tree-sitter grammars missing; install for code\
 highlighting (M-x `pi-coding-agent-install-grammars' to do later)? "
                     (length missing)))
            (pi-coding-agent--install-grammars missing)
          (customize-save-variable 'pi-coding-agent-grammar-declined-set missing)
          (message "[pi-coding-agent] Skipped.  \
Run M-x pi-coding-agent-install-grammars anytime."))))))

;;;; Interactive Command

;;;###autoload
(defun pi-coding-agent-install-grammars ()
  "Show grammar status and install missing tree-sitter grammars.
Displays which grammars are installed and which are missing,
then offers to install the missing ones."
  (interactive)
  (let ((installed (pi-coding-agent--installed-optional-grammars))
        (missing (pi-coding-agent--missing-optional-grammars))
        (essential-missing (pi-coding-agent--missing-essential-grammars)))
    (if (and (null missing) (null essential-missing))
        (message "All %d tree-sitter grammars are installed. ✓"
                 (length installed))
      (let ((buf (get-buffer-create "*pi-coding-agent-grammars*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "pi-coding-agent Tree-sitter Grammars\n\
====================================\n\n"))
            (when essential-missing
              (insert (format "⚠ ESSENTIAL (required for chat rendering):\n"))
              (dolist (g essential-missing)
                (insert (format "  ✗ %s\n" g)))
              (insert "\n"))
            (when missing
              (insert (format "Missing (%d):\n" (length missing)))
              (dolist (g missing)
                (insert (format "  ✗ %s\n" g)))
              (insert "\n"))
            (when installed
              (insert (format "Installed (%d):\n" (length installed)))
              (dolist (g installed)
                (insert (format "  ✓ %s\n" g)))
              (insert "\n"))
            (insert "Press `i' to install missing grammars, `q' to close.\n"))
          (special-mode)
          (local-set-key "i" (lambda ()
                               (interactive)
                               (let ((to-install (append essential-missing missing)))
                                 (when to-install
                                   (pi-coding-agent--install-grammars to-install)
                                   (pi-coding-agent-install-grammars)))))
          (local-set-key "q" #'quit-window)
          (goto-char (point-min)))
        (pop-to-buffer buf)))))

(provide 'pi-coding-agent-grammars)
;;; pi-coding-agent-grammars.el ends here
