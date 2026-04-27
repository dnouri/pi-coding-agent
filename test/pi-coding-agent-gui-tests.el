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

(defun pi-coding-agent-gui-test--thinking-scroll-lines (prefix count)
  "Return COUNT newline-separated lines starting with PREFIX."
  (mapconcat (lambda (n) (format "%s %02d" prefix n))
             (number-sequence 1 count)
             "\n"))

(defun pi-coding-agent-gui-test--thinking-scroll-messages ()
  "Return canonical history that exercises thinking-display scroll behavior."
  (vector
   (list :role "user"
         :content (vector (list :type "text" :text "Question one"))
         :timestamp 1704067200000)
   (list :role "assistant"
         :content (vector (list :type "text"
                                :text (concat
                                       (pi-coding-agent-gui-test--thinking-scroll-lines
                                        "Earlier assistant line" 18)
                                       "\n\nShort bridge.")))
         :timestamp 1704067200500)
   (list :role "user"
         :content (vector (list :type "text" :text "Question two"))
         :timestamp 1704067200800)
   (list :role "assistant"
         :content (vector
                   (list :type "text"
                         :text "Prelude line A\nPrelude line B\nPrelude line C")
                   (list :type "thinking"
                         :thinking (concat
                                    (pi-coding-agent-gui-test--thinking-scroll-lines
                                     "Thinking detail" 30)
                                    "\n\nConclusion thought"))
                   (list :type "text"
                         :text (concat "\n"
                                       (pi-coding-agent-gui-test--thinking-scroll-lines
                                        "Final answer line" 12))))
         :timestamp 1704067201000)))

(defun pi-coding-agent-gui-test--thinking-scroll-hidden-stub ()
  "Return the collapsed thinking stub used by the scroll-history fixture."
  (pi-coding-agent--thinking-hidden-stub
   (pi-coding-agent--thinking-normalize-text
    (concat
     (pi-coding-agent-gui-test--thinking-scroll-lines
      "Thinking detail" 30)
     "\n\nConclusion thought"))))

(defun pi-coding-agent-gui-test--render-thinking-scroll-history (buffer display-mode)
  "Render scroll-regression history into BUFFER using DISPLAY-MODE."
  (with-current-buffer buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--status 'idle
          pi-coding-agent--thinking-display display-mode
          pi-coding-agent--canonical-messages
          (pi-coding-agent-gui-test--thinking-scroll-messages))
    (pi-coding-agent--display-session-history pi-coding-agent--canonical-messages
                                              buffer)
    (font-lock-ensure)
    (redisplay)))

(defun pi-coding-agent-gui-test--window-visible-screen-lines (window)
  "Return how many screen lines WINDOW currently shows from buffer text."
  (count-screen-lines (window-start window) (window-end window t) nil window))

(defun pi-coding-agent-gui-test--window-point-row (window)
  "Return WINDOW point's screen-line row within the current viewport."
  (count-screen-lines (window-start window) (window-point window) nil window))

(defun pi-coding-agent-gui-test--window-current-line (window)
  "Return WINDOW point's current line without text properties."
  (with-selected-window window
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun pi-coding-agent-gui-test--window-thinking-block-order (window)
  "Return the completed-thinking block order at WINDOW point, or nil."
  (with-selected-window window
    (pi-coding-agent--thinking-block-at-pos (point))))

(defun pi-coding-agent-gui-test--window-substantially-filled-p (window)
  "Return non-nil when WINDOW shows nearly a full body of buffer text."
  (>= (pi-coding-agent-gui-test--window-visible-screen-lines window)
      (- (window-body-height window) 2)))

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

(ert-deftest pi-coding-agent-gui-test-thinking-display-toggle-keeps-tail-filled ()
  "Toggling completed thinking keeps a tail-following chat window filled.
After the toggle, a new streamed turn should still auto-scroll from the tail."
  (let ((buf (get-buffer-create "*pi-gui-thinking-tail*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pi-coding-agent-gui-test--render-thinking-scroll-history buf 'visible)
          (let ((win (selected-window)))
            (goto-char (point-max))
            (recenter -1)
            (redisplay)
            (should (>= (window-point win) (1- (point-max))))
            (should (pi-coding-agent-gui-test--window-substantially-filled-p win))
            (pi-coding-agent-toggle-thinking-display)
            (redisplay)
            (should (string-match-p
                     (regexp-quote
                      (pi-coding-agent-gui-test--thinking-scroll-hidden-stub))
                     (buffer-string)))
            (should (>= (window-point win) (1- (point-max))))
            (should (>= (window-end win t) (1- (point-max))))
            (should (pi-coding-agent-gui-test--window-substantially-filled-p win))
            (pi-coding-agent--display-agent-start)
            (pi-coding-agent--display-message-delta "Streaming tail line 01\n")
            (redisplay)
            (should (>= (window-point win) (1- (point-max))))
            (should (>= (window-end win t) (1- (point-max))))
            (pi-coding-agent--display-message-delta "Streaming tail line 02\n")
            (pi-coding-agent--display-agent-end)
            (redisplay)
            (should (>= (window-point win) (1- (point-max))))
            (should (>= (window-end win t) (1- (point-max))))
            (should (string-match-p "Streaming tail line 02" (buffer-string)))))
      (kill-buffer buf))))

(ert-deftest pi-coding-agent-gui-test-thinking-display-toggle-keeps-context-window-usable ()
  "Toggling completed thinking keeps a non-tail chat window usable.
Point should stay on the same logical thinking block and the window should
remain substantially filled even when the collapsed tail is shorter."
  (let ((buf (get-buffer-create "*pi-gui-thinking-context*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pi-coding-agent-gui-test--render-thinking-scroll-history buf 'visible)
          (let ((win (selected-window))
                block-before)
            (goto-char (point-min))
            (should (search-forward "Thinking detail 25" nil t))
            (beginning-of-line)
            (recenter 10)
            (redisplay)
            (setq block-before
                  (pi-coding-agent-gui-test--window-thinking-block-order win))
            (should block-before)
            (should (pi-coding-agent-gui-test--window-substantially-filled-p win))
            (should (equal "> Thinking detail 25"
                           (pi-coding-agent-gui-test--window-current-line win)))
            (pi-coding-agent-toggle-thinking-display)
            (redisplay)
            (should (string-match-p
                     (regexp-quote
                      (pi-coding-agent-gui-test--thinking-scroll-hidden-stub))
                     (buffer-string)))
            (should (equal (pi-coding-agent-gui-test--thinking-scroll-hidden-stub)
                           (pi-coding-agent-gui-test--window-current-line win)))
            (should (equal block-before
                           (pi-coding-agent-gui-test--window-thinking-block-order win)))
            (should (pi-coding-agent-gui-test--window-substantially-filled-p win))))
      (kill-buffer buf))))

(ert-deftest pi-coding-agent-gui-test-thinking-display-toggle-expands-same-block-from-hidden-stub ()
  "Expanding a hidden thinking stub restores the same logical block.
Point should land back on the same completed-thinking block rather than an
unrelated raw offset in the conversation."
  (let ((buf (get-buffer-create "*pi-gui-thinking-expand*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pi-coding-agent-gui-test--render-thinking-scroll-history buf 'hidden)
          (let ((win (selected-window))
                block-before)
            (goto-char (point-min))
            (should (search-forward
                     (pi-coding-agent-gui-test--thinking-scroll-hidden-stub)
                     nil t))
            (beginning-of-line)
            (recenter 10)
            (redisplay)
            (setq block-before
                  (pi-coding-agent-gui-test--window-thinking-block-order win))
            (should block-before)
            (should (equal (pi-coding-agent-gui-test--thinking-scroll-hidden-stub)
                           (pi-coding-agent-gui-test--window-current-line win)))
            (pi-coding-agent-toggle-thinking-display)
            (redisplay)
            (should (string-match-p "Thinking detail 25" (buffer-string)))
            (should (equal block-before
                           (pi-coding-agent-gui-test--window-thinking-block-order win)))
            (should (string-match-p
                     "^> Thinking detail [0-9][0-9]$"
                     (pi-coding-agent-gui-test--window-current-line win)))
            (should (pi-coding-agent-gui-test--window-substantially-filled-p win))))
      (kill-buffer buf))))

(ert-deftest pi-coding-agent-gui-test-thinking-display-toggle-restores-each-visible-window ()
  "Toggling completed thinking preserves each visible window's own context.
The tail window should stay at the end, while the context window keeps its
logical block and should not start following later streamed output."
  (let ((buf (get-buffer-create "*pi-gui-thinking-multi-window*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buf)
          (pi-coding-agent-gui-test--render-thinking-scroll-history buf 'visible)
          (let* ((tail-win (selected-window))
                 (context-win (split-window-below)))
            (set-window-buffer context-win buf)
            (with-selected-window tail-win
              (goto-char (point-max))
              (recenter -1)
              (redisplay))
            (with-selected-window context-win
              (goto-char (point-min))
              (should (search-forward "Thinking detail 25" nil t))
              (beginning-of-line)
              (recenter 8)
              (redisplay))
            (let ((context-line-before
                   (pi-coding-agent-gui-test--window-current-line context-win))
                  context-start-after-toggle)
              (with-selected-window tail-win
                (pi-coding-agent-toggle-thinking-display))
              (redisplay)
              (should (eq (selected-window) tail-win))
              (should (>= (window-point tail-win) (1- (with-current-buffer buf (point-max)))))
              (should (>= (window-end tail-win t)
                          (1- (with-current-buffer buf (point-max)))))
              (should (pi-coding-agent-gui-test--window-substantially-filled-p tail-win))
              (should (equal "> Thinking detail 25" context-line-before))
              (should (equal (pi-coding-agent-gui-test--thinking-scroll-hidden-stub)
                             (pi-coding-agent-gui-test--window-current-line context-win)))
              (should (pi-coding-agent-gui-test--window-substantially-filled-p context-win))
              (setq context-start-after-toggle (window-start context-win))
              (with-current-buffer buf
                (pi-coding-agent--display-agent-start)
                (pi-coding-agent--display-message-delta "Dual window tail line\n"))
              (redisplay)
              (should (>= (window-point tail-win) (1- (with-current-buffer buf (point-max)))))
              (should (>= (window-end tail-win t)
                          (1- (with-current-buffer buf (point-max)))))
              (should (= context-start-after-toggle (window-start context-win)))
              (should (equal (pi-coding-agent-gui-test--thinking-scroll-hidden-stub)
                             (pi-coding-agent-gui-test--window-current-line context-win))))))
      (kill-buffer buf))))

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
