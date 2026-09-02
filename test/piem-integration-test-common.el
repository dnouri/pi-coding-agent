;;; piem-integration-test-common.el --- Shared helpers for integration contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared backend-selection and event-collection helpers for the integration
;; suite.  The same assertion bodies run against both the fake-pi protocol
;; double and the real pi CLI, with backend names visible in test output.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'piem)
(require 'piem-test-common)

(defconst piem-integration--default-fake-scenario "prompt-lifecycle"
  "Default fake-pi scenario for shared integration tests.")

(defconst piem-integration--all-backends '(fake real)
  "All backends that shared integration tests define explicitly.")

(defconst piem-integration--prompt-lifecycle-message
  "/no_think Say OK"
  "Terse lifecycle prompt kept intentionally short for the real backend.")

(defconst piem-integration--prompt-abort-message
  "/no_think Count from 1 to 100 slowly"
  "Abort contract prompt that should stream long enough to interrupt.")

(defconst piem-integration--prompt-session-materialize-message
  "/no_think Say: test"
  "Shortest proven prompt that materializes a real session file quickly.")

(defconst piem-integration--prompt-steering-initial-message
  "/no_think Say: working"
  "Initial prompt for steering-contract coverage.")

(defconst piem-integration--prompt-steering-queued-message
  "/no_think Say: queued-steer-test"
  "Queued steering prompt used to verify visible delivery ordering.")

(defvar piem-integration--backend nil
  "Backend plist for the currently running integration test.")

(defun piem-integration--enabled-backends ()
  "Return the integration backends enabled for this run.
The `PI_INTEGRATION_BACKENDS' environment variable accepts a comma- or
space-separated list like `fake,real'.  When unset, both backends run."
  (if-let* ((value (getenv "PI_INTEGRATION_BACKENDS"))
            (parts (split-string value "[[:space:]]*,[[:space:]]*\\|[[:space:]]+" t)))
      (mapcar #'intern parts)
    '(fake real)))

(defun piem-integration--backend-enabled-p (backend)
  "Return non-nil when BACKEND should run in this test invocation."
  (memq backend (piem-integration--enabled-backends)))

(defun piem-integration--backend-spec (backend &optional fake-scenario fake-extra-args)
  "Return a backend plist for BACKEND.
FAKE-SCENARIO and FAKE-EXTRA-ARGS apply only to the fake backend."
  (piem-test-backend-spec
   backend
   piem-integration--default-fake-scenario
   fake-scenario
   fake-extra-args))

(defun piem-integration--skip-unless-available (backend-spec)
  "Skip unless BACKEND-SPEC can run in the current environment."
  (unless (getenv "PI_RUN_INTEGRATION")
    (ert-skip "PI_RUN_INTEGRATION not set - opt-in required"))
  (let ((backend (plist-get backend-spec :name)))
    (unless (piem-integration--backend-enabled-p backend)
      (ert-skip (format "Backend %s disabled by PI_INTEGRATION_BACKENDS"
                        (plist-get backend-spec :label))))
    (pcase backend
      ('fake
       (unless (file-exists-p piem-test-fake-pi-script)
         (ert-skip (format "fake-pi script missing: %s"
                           piem-test-fake-pi-script)))
       (piem-test-python-executable))
      ('real
       (unless (executable-find (car (plist-get backend-spec :executable)))
         (ert-skip (format "%s executable not found"
                           (car (plist-get backend-spec :executable)))))))))

(defmacro piem-integration-with-backend (spec &rest body)
  "Run BODY with a process described by SPEC.
SPEC is (PROC BACKEND &rest OPTIONS).  OPTIONS accepts `:fake-scenario'
and `:fake-extra-args'.  Within BODY,
`piem-integration--backend' is bound to the backend plist and
`piem--event-handlers' receives events for PROC."
  (declare (indent 1) (debug t))
  (let* ((proc (nth 0 spec))
         (backend (nth 1 spec))
         (options (nthcdr 2 spec))
         (fake-scenario (plist-get options :fake-scenario))
         (fake-extra-args (plist-get options :fake-extra-args)))
    `(let* ((piem-integration--backend
             (piem-integration--backend-spec
              ,backend ,fake-scenario ,fake-extra-args))
            (piem-executable
             (plist-get piem-integration--backend :executable))
            (piem-extra-args
             (plist-get piem-integration--backend :extra-args)))
       (piem-integration--skip-unless-available
        piem-integration--backend)
       (ert-info ((format "backend: %s"
                          (plist-get piem-integration--backend :label)))
         (let ((,proc (piem--start-process default-directory))
               (piem--event-handlers nil))
           (process-put ,proc 'piem-display-handler
                        (lambda (event)
                          (dolist (handler piem--event-handlers)
                            (funcall handler event))))
           (unwind-protect
               (progn ,@body)
             (when (process-live-p ,proc)
               (delete-process ,proc))))))))

(defmacro piem-integration-deftest (spec docstring &rest body)
  "Define a shared integration contract from SPEC for fake and real backends.
SPEC is (NAME &rest OPTIONS), where OPTIONS accepts the same keywords as
`piem-integration-with-backend'.
Both backend variants are always defined; runtime environment filters decide
which ones execute or skip."
  (declare (indent 2) (debug t))
  (let ((name (car spec))
        (options (cdr spec))
        (tests nil))
    (dolist (backend piem-integration--all-backends)
      (push
       `(ert-deftest ,(intern (format "piem-integration-%s/%s"
                                      name backend)) ()
          ,docstring
          (piem-integration-with-backend
              (proc ',backend ,@options)
            ,@body))
       tests))
    `(progn ,@(nreverse tests))))

(defun piem-integration--rpc-until (proc command predicate &optional timeout poll-interval)
  "Send COMMAND to PROC until PREDICATE accepts the response.
Returns the last response, or nil if TIMEOUT expires first.
Each retry waits for one bounded response instead of spraying many
short-lived overlapping RPCs."
  (let* ((timeout (or timeout piem-test-rpc-timeout))
         (poll-interval (or poll-interval piem-test-poll-interval))
         (start (float-time))
         response)
    (while (and (< (- (float-time) start) timeout)
                (not (and response (funcall predicate response))))
      (let* ((elapsed (- (float-time) start))
             (remaining (max poll-interval (- timeout elapsed)))
             (attempt-timeout (min 1.0 remaining)))
        (setq response (piem--rpc-sync proc command attempt-timeout)))
      (unless (and response (funcall predicate response))
        (sleep-for poll-interval)))
    response))

(defun piem-integration--message-text (message)
  "Return the visible text content from MESSAGE."
  (let ((content (plist-get message :content)))
    (cond
     ((stringp content) content)
     ((and (vectorp content) (> (length content) 0))
      (mapconcat (lambda (part) (or (plist-get part :text) ""))
                 (append content nil)
                 ""))
     (t ""))))

(defun piem-integration--response-has-existing-session-file-p (response)
  "Return non-nil when RESPONSE data names a session file on disk."
  (let* ((data (plist-get response :data))
         (session-file (plist-get data :sessionFile)))
    (and session-file (file-exists-p session-file))))

(provide 'piem-integration-test-common)
;;; piem-integration-test-common.el ends here
