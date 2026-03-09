;;; install-deps.el --- Install Emacs package dependencies -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Batch entry point used by the Makefile and CI to install package
;; dependencies from `pi-coding-agent.el'.
;;
;; Usage:
;;   Emacs --batch -Q -L . -l scripts/install-deps.el

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pi-coding-agent-build)

(pi-coding-agent-build-install-deps)

(provide 'install-deps)
;;; install-deps.el ends here
