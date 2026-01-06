;;; pi-gui-tests.el --- GUI integration tests for pi.el -*- lexical-binding: t; -*-

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
(require 'pi-gui-test-utils)

;;;; Session Tests

(ert-deftest pi-gui-test-session-starts ()
  "Test that pi session starts with proper layout."
  (pi-gui-test-with-session
    (should (pi-gui-test-session-active-p))
    (should (pi-gui-test-chat-window))
    (should (pi-gui-test-input-window))
    (should (pi-gui-test-verify-layout))))

;;;; Scroll Preservation Tests

(ert-deftest pi-gui-test-scroll-preserved-streaming ()
  "Test scroll position preserved during streaming response."
  (pi-gui-test-with-session
    (pi-gui-test-ensure-scrollable)
    (pi-gui-test-scroll-up 20)
    (should-not (pi-gui-test-at-end-p))
    (let ((line-before (pi-gui-test-top-line-number)))
      (should (> line-before 1))
      (pi-gui-test-send "Say: ok")
      (should (= line-before (pi-gui-test-top-line-number))))))

(ert-deftest pi-gui-test-scroll-preserved-tool-use ()
  "Test scroll position preserved when pi uses tools."
  (pi-gui-test-with-session
    (pi-gui-test-ensure-scrollable)
    (let ((test-file (pi-gui-test-create-temp-file "test.txt" "Hi\n")))
      (unwind-protect
          (progn
            (pi-gui-test-scroll-up 20)
            (should-not (pi-gui-test-at-end-p))
            (let ((line-before (pi-gui-test-top-line-number)))
              (should (> line-before 1))
              (pi-gui-test-send (format "Read %s" test-file))
              (should (= line-before (pi-gui-test-top-line-number)))))
        (pi-gui-test-delete-temp-file test-file)))))

(ert-deftest pi-gui-test-scroll-auto-when-at-end ()
  "Test auto-scroll when user is at end of buffer."
  (pi-gui-test-with-session
    (pi-gui-test-scroll-to-end)
    (should (pi-gui-test-at-end-p))
    (pi-gui-test-send "Say: ok")
    (should (pi-gui-test-at-end-p))))

;;;; Window Management Tests

(ert-deftest pi-gui-test-window-both-visible ()
  "Test both chat and input windows are visible."
  (pi-gui-test-with-session
    (should (pi-gui-test-chat-window))
    (should (pi-gui-test-input-window))
    (should (window-live-p (pi-gui-test-chat-window)))
    (should (window-live-p (pi-gui-test-input-window)))))

(ert-deftest pi-gui-test-window-kill-both ()
  "Test killing chat buffer also kills input buffer."
  (pi-gui-test-with-fresh-session
    (let ((chat-buf (plist-get pi-gui-test--session :chat-buffer))
          (input-buf (plist-get pi-gui-test--session :input-buffer)))
      (should (buffer-live-p chat-buf))
      (should (buffer-live-p input-buf))
      (kill-buffer chat-buf)
      (should-not (buffer-live-p chat-buf))
      (should-not (buffer-live-p input-buf)))))

;;;; Content Tests

(ert-deftest pi-gui-test-content-tool-output-shown ()
  "Test that tool output appears in chat."
  (pi-gui-test-with-session
    (let ((test-file (pi-gui-test-create-temp-file "tool-test.txt" "XYZ123\n")))
      (unwind-protect
          (progn
            (pi-gui-test-send (format "Read the file %s" test-file))
            ;; Tool header proves tool was invoked
            (should (pi-gui-test-chat-matches "^read "))
            ;; File content should appear in tool output
            (should (pi-gui-test-chat-contains "XYZ123")))
        (pi-gui-test-delete-temp-file test-file)))))

(ert-deftest pi-gui-test-tool-overlay-bounded ()
  "Test that tool block overlay doesn't extend beyond tool output.
Regression test: overlay with rear-advance was extending to subsequent content."
  (pi-gui-test-with-session
    (let ((test-file (pi-gui-test-create-temp-file "overlay-test.txt" "BEFORE\n")))
      (unwind-protect
          (progn
            ;; Ask to read file AND say something after
            ;; Be explicit about using the tool - small models may skip it otherwise
            (pi-gui-test-send
             (format "Use the read tool to show the contents of %s. After that, say ENDMARKER" test-file))
            ;; Wait for both tool output and the text response
            (should (pi-gui-test-chat-contains "BEFORE"))
            (should (pi-gui-test-chat-contains "ENDMARKER"))
            ;; Now check: ENDMARKER should NOT be inside a tool-block overlay
            (with-current-buffer (plist-get pi-gui-test--session :chat-buffer)
              (goto-char (point-min))
              (when (search-forward "ENDMARKER" nil t)
                (let* ((pos (match-beginning 0))
                       (overlays (overlays-at pos))
                       (tool-overlay (seq-find
                                      (lambda (ov) (overlay-get ov 'pi-tool-block))
                                      overlays)))
                  (should-not tool-overlay)))))
        (pi-gui-test-delete-temp-file test-file)))))

(provide 'pi-gui-tests)
;;; pi-gui-tests.el ends here
