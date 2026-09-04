;;; install-ts-grammars.el --- Install tree-sitter grammars for CI -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Batch entry point used by the Makefile and CI to install the
;; tree-sitter grammars needed by Pilish.
;;
;; Usage:
;;   Emacs --batch -Q -L . -l scripts/install-ts-grammars.el

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pilish-build)

(pilish-build-install-grammars)

(provide 'install-ts-grammars)
;;; install-ts-grammars.el ends here
