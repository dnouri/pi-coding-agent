;;; pi-coding-agent-gui-tests.el --- GUI integration tests for pi-coding-agent -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ERT tests that require a real Emacs GUI (windows, frames, scrolling).
;; Run with: make test-gui [SELECTOR=pattern]
;;
;; These tests focus on behavior that CANNOT be tested with unit tests:
;; - Real window scrolling during streamed updates in a displayed buffer
;; - Auto-scroll vs scroll-preservation with deterministic fake scenarios
;; - Tool-block overlays and extension UI behavior through the subprocess seam
;;
;; Many behaviors (history, spacing, and linked-buffer teardown) are covered
;; more directly by unit tests.
;;
;; For quick fake-backed debugging with a visible window:
;;   ./test/run-gui-tests.sh pi-coding-agent-gui-test-scroll-auto-when-at-end

;;; Code:

(require 'ert)
(require 'pi-coding-agent-gui-test-utils)
(require 'pi-coding-agent-test-common)

(defun pi-coding-agent-gui-test--table-display-strings (beg end)
  "Return ordered table overlay display strings between BEG and END."
  (when-let ((buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (mapcar (lambda (ov) (overlay-get ov 'display))
              (sort (seq-filter
                     (lambda (ov) (overlay-get ov 'pi-coding-agent-table-display))
                     (overlays-in beg end))
                    (lambda (left right)
                      (< (overlay-start left) (overlay-start right))))))))

;;;; Session Tests

(ert-deftest pi-coding-agent-gui-test-session-starts ()
  "Test that a fake-backed pi session starts with proper layout."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "prompt-lifecycle")
    (should (pi-coding-agent-gui-test-session-active-p))
    (should (pi-coding-agent-gui-test-chat-window))
    (should (pi-coding-agent-gui-test-input-window))
    (should (pi-coding-agent-gui-test-verify-layout))))

;;;; Scroll Preservation Tests

(ert-deftest pi-coding-agent-gui-test-scroll-preserved-streaming ()
  "Test scroll position is preserved while a fake stream updates below."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "scrolling-text")
    (pi-coding-agent-gui-test-send "first turn")
    ;; The fake stream is usually tall enough already, but large frames can make
    ;; the buffer barely non-scrollable.  Top off with dummy lines so the test
    ;; exercises scroll preservation rather than frame geometry.
    (pi-coding-agent-gui-test-ensure-scrollable)
    (pi-coding-agent-gui-test-scroll-up 20)
    (should-not (pi-coding-agent-gui-test-at-end-p))
    (let ((line-before (pi-coding-agent-gui-test-top-line-number)))
      (should (> line-before 1))
      (pi-coding-agent-gui-test-send "second turn")
      (should (= line-before (pi-coding-agent-gui-test-top-line-number)))
      (should (pi-coding-agent-gui-test-chat-contains "Scroll line 24 for second turn")))))

(ert-deftest pi-coding-agent-gui-test-scroll-preserved-tool-use ()
  "Test scroll position is preserved while fake tool output arrives."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "tool-read")
    (pi-coding-agent-gui-test-ensure-scrollable)
    (pi-coding-agent-gui-test-scroll-up 20)
    (should-not (pi-coding-agent-gui-test-at-end-p))
    (let ((line-before (pi-coding-agent-gui-test-top-line-number)))
      (should (> line-before 1))
      (pi-coding-agent-gui-test-send "Use the fake read tool")
      (should (= line-before (pi-coding-agent-gui-test-top-line-number)))
      (should (pi-coding-agent-gui-test-chat-contains "fake tool output")))))

(ert-deftest pi-coding-agent-gui-test-scroll-auto-when-at-end ()
  "Test auto-scroll when user is at end across deterministic fake turns.
Regression: `pi-coding-agent--display-agent-end' must leave window-point at
buffer end so the next streamed turn still follows automatically."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "scrolling-text")
    (pi-coding-agent-gui-test-send "first turn")
    (should (pi-coding-agent-gui-test-window-point-at-end-p))
    (should (pi-coding-agent-gui-test-at-end-p))
    (pi-coding-agent-gui-test-send "second turn")
    (should (pi-coding-agent-gui-test-window-point-at-end-p))
    (should (pi-coding-agent-gui-test-at-end-p))
    (should (pi-coding-agent-gui-test-chat-contains "Scroll line 24 for second turn"))))

(ert-deftest pi-coding-agent-gui-test-table-resize-refreshes-hot-tail-only ()
  "Resizing the frame rewraps hot tables only and preserves scroll position."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "prompt-lifecycle")
    (let* ((chat-buf (plist-get pi-coding-agent-gui-test--session :chat-buffer))
           (frame (selected-frame))
           (orig-width (frame-width))
           (cold-table
            "| Feature | Notes |\n|---------|-------|\n| Cold history | This older table was wrapped at the original wide width and should stay frozen after resize |\n")
           (hot-table
            "| Feature | Notes |\n|---------|-------|\n| Hot tail | This recent table should rewrap when the window narrows so the columns remain readable |\n")
           cold-before
           hot-before
           cold-start
           hot-start)
      (unwind-protect
          (progn
            (with-current-buffer chat-buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "You · 10:00\n===========\n")
                (setq cold-start (point))
                (insert cold-table "\nAssistant\n=========\nRecent reply\n\nYou · 10:05\n===========\n")
                (setq hot-start (point))
                (insert hot-table)
                (dotimes (i 80)
                  (insert (format "filler line %d\n" i))))
              (font-lock-ensure)
              (let* ((chat-win (pi-coding-agent-gui-test-chat-window))
                     (initial-width (window-width chat-win)))
                (pi-coding-agent--decorate-tables-in-region
                 (point-min) (point-max) initial-width)
                (move-marker pi-coding-agent--hot-tail-start hot-start)
                (setq cold-before
                      (pi-coding-agent-gui-test--table-display-strings
                       cold-start hot-start)
                      hot-before
                      (pi-coding-agent-gui-test--table-display-strings
                       hot-start (point-max)))))
            (redisplay)
            (pi-coding-agent-gui-test-scroll-up 20)
            (let ((line-before (pi-coding-agent-gui-test-top-line-number)))
              (set-frame-size frame (- orig-width 30) (frame-height))
              (redisplay)
              (should (pi-coding-agent-test-wait-until
                       (lambda ()
                         (not (equal hot-before
                                     (pi-coding-agent-gui-test--table-display-strings
                                      hot-start (point-max)))))
                       2 0.05))
              (should (equal cold-before
                             (pi-coding-agent-gui-test--table-display-strings
                              cold-start hot-start)))
              (should (= line-before (pi-coding-agent-gui-test-top-line-number)))))
        (set-frame-size frame orig-width (frame-height))
        (redisplay)))))

;;;; Content Tests

(ert-deftest pi-coding-agent-gui-test-content-tool-output-shown ()
  "Test that fake-backed tool output appears in chat and in the tool block."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "tool-read")
    (pi-coding-agent-gui-test-send "Use the fake read tool")
    (should (pi-coding-agent-gui-test-chat-contains "read /tmp/fake-tool.txt"))
    (should (pi-coding-agent-gui-test-chat-text-in-tool-block-p "fake tool output"))
    (should (pi-coding-agent-gui-test-chat-contains "Tool finished"))))

(ert-deftest pi-coding-agent-gui-test-tool-overlay-bounded ()
  "Test that the tool overlay stops before later assistant text.
Regression: `pi-coding-agent--tool-overlay-finalize' must replace the
rear-advance overlay before assistant text continues after the tool block."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "tool-read")
    (pi-coding-agent-gui-test-send "Use the fake read tool")
    (with-current-buffer (plist-get pi-coding-agent-gui-test--session :chat-buffer)
      (goto-char (point-min))
      (search-forward "fake tool output")
      (let* ((tool-pos (match-beginning 0))
             (tool-overlay (seq-find
                            (lambda (ov) (overlay-get ov 'pi-coding-agent-tool-block))
                            (overlays-at tool-pos))))
        (should tool-overlay))
      (goto-char (point-min))
      (search-forward "Tool finished")
      (let* ((assistant-pos (match-beginning 0))
             (tool-overlay (seq-find
                            (lambda (ov) (overlay-get ov 'pi-coding-agent-tool-block))
                            (overlays-at assistant-pos))))
        (should-not tool-overlay)))))

;;;; Extension Command Tests

(ert-deftest pi-coding-agent-gui-test-extension-command-returns-to-idle ()
  "Fake extension command without a visible turn returns to idle immediately."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "extension-noop")
    (pi-coding-agent-gui-test-send "/test-noop" t)
    (should (pi-coding-agent-gui-test-wait-for-idle 2))))

(ert-deftest pi-coding-agent-gui-test-extension-custom-message-displayed ()
  "Fake extension command displays a custom message in chat."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "extension-message")
    (pi-coding-agent-gui-test-send "/test-message")
    (should (pi-coding-agent-gui-test-chat-contains "Test message from extension"))))

(ert-deftest pi-coding-agent-gui-test-extension-confirm-response-displayed ()
  "Fake extension confirm response triggers the displayed follow-up message."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "extension-confirm")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (pi-coding-agent-gui-test-send "/test-confirm")
      (should (pi-coding-agent-gui-test-chat-contains "CONFIRMED")))))

;;;; Tool Toggle Tests

(ert-deftest pi-coding-agent-gui-test-tool-toggle-expand-collapse-cycle ()
  "TAB expands, collapses, and re-expands in a displayed GUI buffer.
Regression for issue #166: collapsing from the [-] button placed cursor
at the overlay boundary (half-open interval), making the next TAB fall
through to `outline-cycle' instead of toggling.  Uses a standalone
chat-mode buffer to isolate the toggle logic from RPC timing."
  (let ((buf (get-buffer-create "*pi-gui-toggle-test*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pi-coding-agent-chat-mode)
          (pi-coding-agent--display-tool-start "read" '(:path "/tmp/test.txt"))
          (pi-coding-agent--display-tool-end "read" '(:path "/tmp/test.txt")
            `((:type "text"
               :text ,(mapconcat (lambda (n) (format "Line %02d of file" n))
                                 (number-sequence 1 20) "\n")))
            nil nil)
          (font-lock-ensure)
          (redisplay)
          ;; Initially collapsed
          (should (string-match-p "more lines)" (buffer-string)))
          (should-not (string-match-p "Line 20" (buffer-string)))
          ;; Expand from the collapsed indicator
          (goto-char (point-min))
          (search-forward "more lines)" nil t)
          (beginning-of-line)
          (pi-coding-agent-toggle-tool-section)
          (redisplay)
          (should (string-match-p "Line 20" (buffer-string)))
          ;; Navigate to [-] button and collapse
          (goto-char (point-min))
          (search-forward "[-]" nil t)
          (beginning-of-line)
          (pi-coding-agent-toggle-tool-section)
          (redisplay)
          (should (string-match-p "more lines)" (buffer-string)))
          (should-not (string-match-p "Line 20" (buffer-string)))
          ;; Re-expand must still work from current cursor position
          (pi-coding-agent-toggle-tool-section)
          (redisplay)
          (should (string-match-p "Line 20" (buffer-string))))
      (kill-buffer buf))))

;;;; Streaming Fontification Tests

(ert-deftest pi-coding-agent-gui-test-streaming-no-fences ()
  "Streaming write content shows no fence markers to the user.
Fences exist in the buffer for tree-sitter parsing, but
`md-ts-hide-markup' makes them invisible.  Uses a displayed
buffer (jit-lock active) to verify under real GUI conditions."
  (let ((buf (get-buffer-create "*pi-gui-fontify-test*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pi-coding-agent-chat-mode)
          (pi-coding-agent--handle-display-event '(:type "agent_start"))
          (pi-coding-agent--handle-display-event '(:type "message_start"))
          (pi-coding-agent--handle-display-event
           `(:type "message_update"
             :assistantMessageEvent (:type "toolcall_start" :contentIndex 0)
             :message (:role "assistant"
                       :content [(:type "toolCall" :id "call_1"
                                  :name "write"
                                  :arguments (:path "/tmp/test.py"))])))
          (redisplay)
          (pi-coding-agent-test--send-delta
           "write" '(:path "/tmp/test.py"
                     :content "def hello():\n    return 42\n"))
          (font-lock-ensure)
          ;; Fences are in the buffer (for tree-sitter) but invisible
          (let ((visible (pi-coding-agent--visible-text
                          (point-min) (point-max))))
            (should-not (string-match-p "```" visible)))
          ;; Content is present with syntax faces
          (goto-char (point-min))
          (should (search-forward "def" nil t))
          (let ((face (get-text-property (match-beginning 0) 'face)))
            (should (or (eq face 'font-lock-keyword-face)
                        (and (listp face)
                             (memq 'font-lock-keyword-face face))))))
      (kill-buffer buf))))

(provide 'pi-coding-agent-gui-tests)
;;; pi-coding-agent-gui-tests.el ends here
