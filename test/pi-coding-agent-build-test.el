;;; pi-coding-agent-build-test.el --- Tests for batch build helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst pi-coding-agent-test-build--repo-root
  (expand-file-name ".." (file-name-directory load-file-name))
  "Repository root used by build script tests.")

(load (expand-file-name "scripts/pi-coding-agent-build.el"
                        pi-coding-agent-test-build--repo-root)
      t t)

(ert-deftest pi-coding-agent-test-build-package-requirements-follow-package-header ()
  "Read dependency versions from the package header, excluding Emacs itself."
  (should (equal '((transient . (0 9 0))
                   (md-ts-mode . (0 3 0)))
                 (pi-coding-agent-build-package-requirements))))

(ert-deftest pi-coding-agent-test-build-package-requirements-fallback-without-lm-package-requires ()
  "Read package requirements even when `lm-package-requires' is unavailable."
  (let ((pi-coding-agent-build-main-file
         (make-temp-file "pi-coding-agent-build" nil ".el"
                         ";;; demo.el --- demo -*- lexical-binding: t; -*-\n\
;; Package-Requires: ((emacs \"29.1\") (transient \"0.9.0\"))\n")))
    (unwind-protect
        (cl-letf (((symbol-function 'lm-package-requires) nil))
          (should (equal '((transient . (0 9 0)))
                         (pi-coding-agent-build-package-requirements))))
      (delete-file pi-coding-agent-build-main-file))))

(ert-deftest pi-coding-agent-test-build-install-deps-installs-missing-or-outdated-packages ()
  "Install only package dependencies that are missing or too old."
  (let ((package-install-upgrade-built-in nil)
        (package-archives nil)
        (package-archive-contents nil)
        (refreshed nil)
        (installed nil)
        (installed-state '((transient . nil)
                           (md-ts-mode . t))))
    (cl-letf (((symbol-function 'package-initialize) #'ignore)
              ((symbol-function 'package-refresh-contents)
               (lambda ()
                 (setq refreshed t)
                 (setq package-archive-contents '((transient) (md-ts-mode)))))
              ((symbol-function 'package-installed-p)
               (lambda (package &optional _min-version)
                 (alist-get package installed-state)))
              ((symbol-function 'package-install)
               (lambda (package)
                 (push package installed)
                 (setf (alist-get package installed-state) t))))
      (pi-coding-agent-build-install-deps
       '((transient . (0 9 0))
         (md-ts-mode . (0 3 0))))
      (should package-install-upgrade-built-in)
      (should refreshed)
      (should (equal '(transient) (nreverse installed)))
      (should (member '("melpa" . "https://melpa.org/packages/")
                      package-archives)))))

(ert-deftest pi-coding-agent-test-build-install-deps-errors-when-dependency-stays-missing ()
  "Signal an error when a required dependency is still unavailable."
  (let ((package-archives nil)
        (package-archive-contents '(dummy))
        (message-text nil))
    (cl-letf (((symbol-function 'package-initialize) #'ignore)
              ((symbol-function 'package-refresh-contents) #'ignore)
              ((symbol-function 'package-installed-p)
               (lambda (package &optional _min-version)
                 (eq package 'transient)))
              ((symbol-function 'package-install)
               (lambda (_package) nil)))
      (setq message-text
            (condition-case err
                (progn
                  (pi-coding-agent-build-install-deps
                   '((transient . (0 9 0))
                     (md-ts-mode . (0 3 0))))
                  nil)
              (error (error-message-string err))))
      (should (string-match-p "md-ts-mode" message-text)))))

(ert-deftest pi-coding-agent-test-build-install-grammars-reports-ready-and-installed-counts ()
  "Count already-ready and newly installed grammars separately."
  (let ((available '(markdown))
        (installed nil)
        (messages nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang available)))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (lang &optional _out-dir)
                 (push lang installed)
                 (push lang available)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (let ((result (pi-coding-agent-build-install-grammars '(markdown bash javascript))))
        (should (equal '(:already-installed 1 :installed 2 :failed 0 :total 3)
                       result))
        (should (equal '(bash javascript)
                       (sort installed
                             (lambda (left right)
                               (string-lessp (symbol-name left)
                                             (symbol-name right))))))
        (should (string-match-p "already ready 1, installed 2, failed 0, total 3"
                                (car messages)))))))

(ert-deftest pi-coding-agent-test-build-install-grammars-errors-when-a-grammar-fails ()
  "Signal an error when any requested grammar cannot be installed."
  (let ((available nil)
        (messages nil)
        (error-text nil))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (lang &rest _)
                 (memq lang available)))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir) nil))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (setq error-text
            (condition-case err
                (progn
                  (pi-coding-agent-build-install-grammars '(markdown))
                  nil)
              (error (error-message-string err))))
      (should (string-match-p "markdown" error-text))
      (should (string-match-p "failed 1" (car messages))))))

(ert-deftest pi-coding-agent-test-build-scripts-byte-compile-cleanly ()
  "Build helper and wrapper scripts byte-compile without warnings."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (scripts-dir (expand-file-name "scripts"
                                        pi-coding-agent-test-build--repo-root))
         (output-buffer (generate-new-buffer " *pi-coding-agent-build-compile*"))
         (exit-code
          (call-process emacs nil output-buffer nil
                        "--batch" "-Q"
                        "-L" pi-coding-agent-test-build--repo-root
                        "-L" scripts-dir
                        "--eval" "(require 'package)"
                        "--eval" "(package-initialize)"
                        "--eval"
                        (format "(setq load-path (cons %S load-path))"
                                pi-coding-agent-test-build--repo-root)
                        "--eval" "(setq byte-compile-error-on-warn t)"
                        "-f" "batch-byte-compile"
                        (expand-file-name "scripts/pi-coding-agent-build.el"
                                          pi-coding-agent-test-build--repo-root)
                        (expand-file-name "scripts/install-deps.el"
                                          pi-coding-agent-test-build--repo-root)
                        (expand-file-name "scripts/install-ts-grammars.el"
                                          pi-coding-agent-test-build--repo-root))))
    (unwind-protect
        (progn
          (should (eq 0 exit-code))
          (with-current-buffer output-buffer
            (should (equal "" (buffer-string)))))
      (kill-buffer output-buffer)
      (dolist (elc '("scripts/pi-coding-agent-build.elc"
                     "scripts/install-deps.elc"
                     "scripts/install-ts-grammars.elc"))
        (let ((path (expand-file-name elc pi-coding-agent-test-build--repo-root)))
          (when (file-exists-p path)
            (delete-file path)))))))

(ert-deftest pi-coding-agent-test-build-install-deps-script-delegates-to-helper ()
  "The dependency install wrapper should call the shared build helper."
  (let ((called nil))
    (cl-letf (((symbol-function 'pi-coding-agent-build-install-deps)
               (lambda (&rest _)
                 (setq called t)
                 t)))
      (load (expand-file-name "scripts/install-deps.el"
                              pi-coding-agent-test-build--repo-root)
            nil t)
      (should called))))

(ert-deftest pi-coding-agent-test-build-install-ts-grammars-script-delegates-to-helper ()
  "The grammar install wrapper should call the shared build helper."
  (let ((called nil))
    (cl-letf (((symbol-function 'pi-coding-agent-build-install-grammars)
               (lambda (&rest _)
                 (setq called t)
                 t)))
      (load (expand-file-name "scripts/install-ts-grammars.el"
                              pi-coding-agent-test-build--repo-root)
            nil t)
      (should called))))

(provide 'pi-coding-agent-build-test)
;;; pi-coding-agent-build-test.el ends here
