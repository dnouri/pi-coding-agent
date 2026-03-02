;;; install-ts-grammars.el --- Install tree-sitter grammars for CI  -*- lexical-binding: t; -*-
;;
;; Usage:
;;   emacs --batch -Q -L . -l scripts/install-ts-grammars.el
;;
;; Installs all tree-sitter grammars (essential + language) into
;; `user-emacs-directory'/tree-sitter/.  Recipes are registered by
;; md-ts-mode (markdown grammars) and pi-coding-agent-grammars (all
;; language grammars).  Requires a C compiler (gcc/cc).

;;; Code:

(require 'treesit)
(require 'md-ts-mode)
(require 'pi-coding-agent-grammars)

;; Essential grammars (markdown, markdown-inline) plus all language
;; grammars for code block highlighting parity with local dev.
(let ((all-grammars (append '(markdown markdown-inline)
                            (mapcar #'car pi-coding-agent-grammar-recipes)))
      (installed 0)
      (failed 0))
  (dolist (lang all-grammars)
    (if (treesit-language-available-p lang)
        (progn
          (message "Grammar %s: already installed" lang)
          (cl-incf installed))
      (message "Grammar %s: installing..." lang)
      (condition-case err
          (progn
            (treesit-install-language-grammar lang)
            (if (treesit-language-available-p lang)
                (progn
                  (message "Grammar %s: installed successfully" lang)
                  (cl-incf installed))
              (message "Grammar %s: install returned but grammar not available" lang)
              (cl-incf failed)))
        (error
         (message "Grammar %s: FAILED - %s" lang (error-message-string err))
         (cl-incf failed)))))
  (message "\nGrammars: %d installed, %d failed, %d total"
           installed failed (length all-grammars))
  (when (> failed 0)
    (error "%d grammar(s) failed to install" failed)))

(provide 'install-ts-grammars)
;;; install-ts-grammars.el ends here
