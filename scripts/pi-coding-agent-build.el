;;; pi-coding-agent-build.el --- Batch helpers for local build tasks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Helpers shared by batch scripts used from the Makefile and CI.
;; Keep build-specific behavior here so it is easy to test with ERT.

;;; Code:

(require 'cl-lib)
(require 'lisp-mnt)
(require 'package)
(require 'treesit)

(defvar pi-coding-agent-grammar-recipes nil
  "Grammar recipes provided by `pi-coding-agent-grammars'.")

(defconst pi-coding-agent-build-main-file
  (expand-file-name "../pi-coding-agent.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the package entry point used as build metadata source of truth.")

(defun pi-coding-agent-build--version-string (version)
  "Return VERSION list as a dotted string."
  (mapconcat #'number-to-string version "."))

(defun pi-coding-agent-build--read-package-requires ()
  "Return the `Package-Requires' header from the current buffer."
  (if (fboundp 'lm-package-requires)
      (lm-package-requires)
    (let ((header (lm-header "package-requires")))
      (unless header
        (error "Missing Package-Requires header in %s"
               pi-coding-agent-build-main-file))
      (car (read-from-string header)))))

(defun pi-coding-agent-build-package-requirements ()
  "Return non-Emacs package requirements from `pi-coding-agent.el'."
  (mapcar (pcase-lambda (`(,package ,version))
            (cons package (version-to-list version)))
          (cl-remove-if (lambda (requirement)
                          (eq (car requirement) 'emacs))
                        (with-temp-buffer
                          (insert-file-contents pi-coding-agent-build-main-file)
                          (pi-coding-agent-build--read-package-requires)))))

(defun pi-coding-agent-build--package-missing-p (package min-version)
  "Return non-nil when PACKAGE does not satisfy MIN-VERSION."
  (not (package-installed-p package min-version)))

(defun pi-coding-agent-build-install-deps (&optional requirements)
  "Install package REQUIREMENTS for local development.
REQUIREMENTS defaults to `pi-coding-agent-build-package-requirements'.
Signals an error if any dependency still does not satisfy its minimum
version after installation finishes."
  (let ((requirements (or requirements
                           (pi-coding-agent-build-package-requirements)))
        (missing nil))
    (setq package-install-upgrade-built-in t)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
    (dolist (requirement requirements)
      (pcase-let ((`(,package . ,min-version) requirement))
        (when (pi-coding-agent-build--package-missing-p package min-version)
          (message "Installing dependency %s >= %s..."
                   package
                   (pi-coding-agent-build--version-string min-version))
          (package-install package))))
    (dolist (requirement requirements)
      (pcase-let ((`(,package . ,min-version) requirement))
        (when (pi-coding-agent-build--package-missing-p package min-version)
          (push (format "%s >= %s"
                        package
                        (pi-coding-agent-build--version-string min-version))
                missing))))
    (when missing
      (error "Dependency install failed: %s"
             (mapconcat #'identity (nreverse missing) ", ")))
    (message "Dependencies installed")
    t))

(defun pi-coding-agent-build--default-grammars ()
  "Return all grammars needed by pi-coding-agent."
  (require 'pi-coding-agent-grammars)
  (mapcar #'car pi-coding-agent-grammar-recipes))

(defun pi-coding-agent-build-install-grammars (&optional grammars)
  "Install tree-sitter GRAMMARS used by pi-coding-agent.
GRAMMARS defaults to all recipes from `pi-coding-agent-grammar-recipes'.
Returns a plist with counts for already installed, newly installed,
failed, and total grammars.  Signals an error if any grammar fails."
  (let* ((grammars (or grammars (pi-coding-agent-build--default-grammars)))
         (already-installed 0)
         (installed 0)
         (failed 0)
         (failed-grammars nil)
         (result nil))
    (dolist (lang grammars)
      (if (treesit-language-available-p lang)
          (progn
            (message "Grammar %s: already installed" lang)
            (cl-incf already-installed))
        (message "Grammar %s: installing..." lang)
        (condition-case err
            (progn
              (treesit-install-language-grammar lang)
              (if (treesit-language-available-p lang)
                  (progn
                    (message "Grammar %s: installed successfully" lang)
                    (cl-incf installed))
                (message "Grammar %s: install returned but grammar not available" lang)
                (push lang failed-grammars)
                (cl-incf failed)))
          (error
           (message "Grammar %s: FAILED - %s" lang (error-message-string err))
           (push lang failed-grammars)
           (cl-incf failed)))))
    (setq result (list :already-installed already-installed
                       :installed installed
                       :failed failed
                       :total (length grammars)))
    (message "Grammars: already ready %d, installed %d, failed %d, total %d"
             already-installed installed failed (length grammars))
    (when failed-grammars
      (error "Grammar install failed: %s"
             (mapconcat #'symbol-name (nreverse failed-grammars) ", ")))
    result))

(provide 'pi-coding-agent-build)
;;; pi-coding-agent-build.el ends here
