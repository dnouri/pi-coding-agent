;;; pi-coding-agent-gui-tests.el --- GUI integration tests for pi-coding-agent -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ERT tests that require a real Emacs GUI (windows, frames, scrolling).
;; Run with: make test-gui
;;
;; These tests focus on behavior that CANNOT be tested with unit tests:
;; - Real window scrolling during LLM streaming
;; - Auto-scroll vs scroll-preservation with actual content
;; - Tool invocation end-to-end
;;
;; Many behaviors (history, spacing, kill-buffer) are covered by unit tests.

;;; Code:

(require 'ert)
(require 'pi-coding-agent-gui-test-utils)

;;;; Session Tests

(ert-deftest pi-coding-agent-gui-test-session-starts ()
  "Test that pi session starts with proper layout."
  (pi-coding-agent-gui-test-with-session
    (should (pi-coding-agent-gui-test-session-active-p))
    (should (pi-coding-agent-gui-test-chat-window))
    (should (pi-coding-agent-gui-test-input-window))
    (should (pi-coding-agent-gui-test-verify-layout))))

;;;; Scroll Preservation Tests

(ert-deftest pi-coding-agent-gui-test-scroll-preserved-streaming ()
  "Test scroll position preserved during streaming response."
  (pi-coding-agent-gui-test-with-session
    (pi-coding-agent-gui-test-ensure-scrollable)
    (pi-coding-agent-gui-test-scroll-up 20)
    (should-not (pi-coding-agent-gui-test-at-end-p))
    (let ((line-before (pi-coding-agent-gui-test-top-line-number)))
      (should (> line-before 1))
      (pi-coding-agent-gui-test-send "Say: ok")
      (should (= line-before (pi-coding-agent-gui-test-top-line-number))))))

(ert-deftest pi-coding-agent-gui-test-scroll-preserved-tool-use ()
  "Test scroll position preserved when pi uses tools."
  (pi-coding-agent-gui-test-with-session
    (pi-coding-agent-gui-test-ensure-scrollable)
    (let ((test-file (pi-coding-agent-gui-test-create-temp-file "test.txt" "Hi\n")))
      (unwind-protect
          (progn
            (pi-coding-agent-gui-test-scroll-up 20)
            (should-not (pi-coding-agent-gui-test-at-end-p))
            (let ((line-before (pi-coding-agent-gui-test-top-line-number)))
              (should (> line-before 1))
              (pi-coding-agent-gui-test-send (format "Read %s" test-file))
              (should (= line-before (pi-coding-agent-gui-test-top-line-number)))))
        (pi-coding-agent-gui-test-delete-temp-file test-file)))))

(ert-deftest pi-coding-agent-gui-test-scroll-auto-when-at-end ()
  "Test auto-scroll when user is at end of buffer.
Also verifies window-point stayed at end across previous tests (shared session).
Regression: display-agent-end was leaving window-point behind point-max,
breaking auto-scroll for subsequent turns."
  (pi-coding-agent-gui-test-with-session
    ;; After previous tests, window-point should still be at end (following)
    ;; This catches the regression where display-agent-end left point behind
    (should (pi-coding-agent-gui-test-window-point-at-end-p))
    ;; Explicitly scroll to end (main test purpose) and verify auto-scroll works
    (pi-coding-agent-gui-test-scroll-to-end)
    (should (pi-coding-agent-gui-test-at-end-p))
    (pi-coding-agent-gui-test-send "Say: ok")
    (should (pi-coding-agent-gui-test-at-end-p))))

;;;; Window Management Tests

(ert-deftest pi-coding-agent-gui-test-window-both-visible ()
  "Test both chat and input windows are visible."
  (pi-coding-agent-gui-test-with-session
    (should (pi-coding-agent-gui-test-chat-window))
    (should (pi-coding-agent-gui-test-input-window))
    (should (window-live-p (pi-coding-agent-gui-test-chat-window)))
    (should (window-live-p (pi-coding-agent-gui-test-input-window)))))

(ert-deftest pi-coding-agent-gui-test-window-kill-both ()
  "Test killing chat buffer also kills input buffer."
  (pi-coding-agent-gui-test-with-fresh-session
    (let ((chat-buf (plist-get pi-coding-agent-gui-test--session :chat-buffer))
          (input-buf (plist-get pi-coding-agent-gui-test--session :input-buffer)))
      (should (buffer-live-p chat-buf))
      (should (buffer-live-p input-buf))
      (kill-buffer chat-buf)
      (should-not (buffer-live-p chat-buf))
      (should-not (buffer-live-p input-buf)))))

;;;; Content Tests

(ert-deftest pi-coding-agent-gui-test-content-tool-output-shown ()
  "Test that tool output appears in chat."
  (pi-coding-agent-gui-test-with-session
    (let ((test-file (pi-coding-agent-gui-test-create-temp-file "tool-test.txt" "XYZ123\n")))
      (unwind-protect
          (progn
            (pi-coding-agent-gui-test-send (format "Read the file %s" test-file))
            ;; Tool header proves tool was invoked
            (should (pi-coding-agent-gui-test-chat-matches "^read "))
            ;; File content should appear in tool output
            (should (pi-coding-agent-gui-test-chat-contains "XYZ123")))
        (pi-coding-agent-gui-test-delete-temp-file test-file)))))

(ert-deftest pi-coding-agent-gui-test-tool-overlay-bounded ()
  "Test that tool block overlay doesn't extend beyond tool output.
Regression test: overlay with rear-advance was extending to subsequent content."
  (pi-coding-agent-gui-test-with-session
    (let ((test-file (pi-coding-agent-gui-test-create-temp-file "overlay-test.txt" "BEFORE\n")))
      (unwind-protect
          (progn
            ;; Ask to read file AND say something after
            ;; Be explicit about using the tool - small models may skip it otherwise
            (pi-coding-agent-gui-test-send
             (format "Call the read tool on %s and show me its contents. After the tool output, say ENDMARKER." test-file))
            ;; Wait for both tool output and the text response
            (should (pi-coding-agent-gui-test-chat-contains "BEFORE"))
            (should (pi-coding-agent-gui-test-chat-contains "ENDMARKER"))
            ;; Now check: ENDMARKER should NOT be inside a tool-block overlay
            (with-current-buffer (plist-get pi-coding-agent-gui-test--session :chat-buffer)
              (goto-char (point-min))
              (when (search-forward "ENDMARKER" nil t)
                (let* ((pos (match-beginning 0))
                       (overlays (overlays-at pos))
                       (tool-overlay (seq-find
                                      (lambda (ov) (overlay-get ov 'pi-coding-agent-tool-block))
                                      overlays)))
                  (should-not tool-overlay)))))
        (pi-coding-agent-gui-test-delete-temp-file test-file)))))

;;;; Formatting Tests

(ert-deftest pi-coding-agent-gui-test-no-consecutive-blank-lines ()
  "Test that chat buffer never has two consecutive blank lines.
This test runs after other tests to check accumulated content from
multiple turns, tool uses, and streaming responses."
  (pi-coding-agent-gui-test-with-session
    (let* ((content (pi-coding-agent-gui-test-chat-content))
           (triple-newline-pos (string-match "\n\n\n" content)))
      (when triple-newline-pos
        ;; Show context around the problem for debugging
        (let* ((start (max 0 (- triple-newline-pos 50)))
               (end (min (length content) (+ triple-newline-pos 80)))
               (context (substring content start end))
               (context-visible (replace-regexp-in-string "\n" "↵\n" context)))
          (ert-fail (format "Found consecutive blank lines at position %d:\n%s"
                            triple-newline-pos context-visible))))
      (should-not triple-newline-pos))))

(provide 'pi-coding-agent-gui-tests)
;;; pi-coding-agent-gui-tests.el ends here
