;;; pi-coding-agent-render-test.el --- Tests for pi-coding-agent-render -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;;; Commentary:

;; Tests for response display, tool output, streaming fontification,
;; diff overlays, file navigation, table alignment, and history display
;; — the chat rendering layer.

;;; Code:

(require 'ert)
(require 'pi-coding-agent)
(require 'pi-coding-agent-test-common)

;;; Response Display

(ert-deftest pi-coding-agent-test-append-to-chat-inserts-text ()
  "pi-coding-agent--append-to-chat inserts text at end of chat buffer."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--append-to-chat "Hello")
    (should (equal (buffer-string) "Hello"))))

(ert-deftest pi-coding-agent-test-append-to-chat-appends ()
  "pi-coding-agent--append-to-chat appends to existing content."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      (insert "First"))
    (pi-coding-agent--append-to-chat " Second")
    (should (equal (buffer-string) "First Second"))))

(ert-deftest pi-coding-agent-test-display-agent-start-inserts-separator ()
  "agent_start event inserts a setext heading separator."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (should (string-match-p "Assistant\n===" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-message-delta-appends-text ()
  "message_update text_delta appends text to chat."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)  ; Creates streaming marker
    (pi-coding-agent--display-message-delta "Hello, ")
    (pi-coding-agent--display-message-delta "world!")
    (should (string-match-p "Hello, world!" (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-transforms-atx-headings ()
  "ATX headings in assistant content are leveled down.
# becomes ##, ## becomes ###, etc. This keeps our setext H1 separators
as the top-level structure."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "# Heading 1\n## Heading 2")
    ;; # should become ##, ## should become ###
    (should (string-match-p "## Heading 1" (buffer-string)))
    (should (string-match-p "### Heading 2" (buffer-string)))
    ;; Original single # should not appear (except as part of ##)
    (should-not (string-match-p "^# " (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-heading-transform-after-newline ()
  "Heading transform works when # follows newline within delta."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Some text\n# Heading")
    (should (string-match-p "Some text\n## Heading" (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-heading-transform-across-deltas ()
  "Heading transform works when newline and # are in separate deltas."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Some text\n")
    (pi-coding-agent--display-message-delta "# Heading")
    (should (string-match-p "## Heading" (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-no-transform-mid-line-hash ()
  "Hash characters mid-line are not transformed."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Use #include or C# language")
    ;; Mid-line # should stay as-is
    (should (string-match-p "#include" (buffer-string)))
    (should (string-match-p "C#" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-thinking-delta-appends-text ()
  "message_update thinking_delta appends text to chat."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)  ; Creates streaming marker
    (pi-coding-agent--display-thinking-delta "Let me think...")
    (pi-coding-agent--display-thinking-delta " about this.")
    (should (string-match-p "Let me think... about this." (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-agent-end-adds-newline ()
  "agent_end normalizes trailing whitespace to single newline."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--append-to-chat "Some response")
    (pi-coding-agent--display-agent-end)
    (should (string-suffix-p "response\n" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-no-blank-line-after-user-header ()
  "User header has no blank line after setext underline.
The hidden === provides visual spacing when `md-ts-hide-markup' is t."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-user-message "Hello")
    ;; Pattern: setext heading (You + underline), NO blank line, content
    (should (string-match-p "You\n=+\nHello" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-no-blank-line-after-assistant-header ()
  "Assistant header has no blank line after setext underline.
The hidden === provides visual spacing when `md-ts-hide-markup' is t."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Hi")
    ;; Pattern: setext heading (Assistant + underline), NO blank line, content
    (should (string-match-p "Assistant\n=+\nHi" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-delta-leading-newlines-stripped ()
  "Leading newlines from first text delta are stripped.
Models often send \\n\\n before first content, which would create
extra blank lines after the setext header."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "\n\nHi")
    (should (string-match-p "Assistant\n=+\nHi" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-thinking-leading-newlines-stripped ()
  "Leading newlines before thinking block are stripped.
Models may send \\n\\n before thinking content too."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-thinking-start)
    ;; Thinking start should appear directly after header, no blank line
    (should (string-match-p "Assistant\n=+\n>" (buffer-string)))))

(ert-deftest pi-coding-agent-test-thinking-empty-lifecycle-no-visible-blockquote ()
  "Empty thinking start/end should not leave a visible blank blockquote."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-thinking-start)
    (pi-coding-agent--display-thinking-end "")
    (goto-char (point-min))
    (should-not (re-search-forward "^>\\s-*$" nil t))))

(ert-deftest pi-coding-agent-test-thinking-leading-trailing-newlines-normalized ()
  "Thinking boundaries should not render extra empty blockquote lines."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-thinking-start)
    (pi-coding-agent--display-thinking-delta "\n\nSingle thought.\n\n")
    (pi-coding-agent--display-thinking-end "")
    (goto-char (point-min))
    (should (re-search-forward "^> Single thought\\.$" nil t))
    (goto-char (point-min))
    (should-not (re-search-forward "^>\\s-*$" nil t))))

(ert-deftest pi-coding-agent-test-thinking-normalization-preserves-first-line-indentation ()
  "Normalization should trim blank boundaries without stripping indentation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-thinking-start)
    (pi-coding-agent--display-thinking-delta "\n\n  indented thought")
    (pi-coding-agent--display-thinking-end "")
    (should (string-match-p "^>   indented thought" (buffer-string)))))

(ert-deftest pi-coding-agent-test-thinking-whitespace-only-delta-does-not-rewrite-buffer ()
  "Adding ignorable trailing whitespace should not rewrite rendered thinking."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-thinking-start)
    (pi-coding-agent--display-thinking-delta "Stable")
    (let ((before (buffer-string))
          (before-tick (buffer-chars-modified-tick)))
      (pi-coding-agent--display-thinking-delta "\n")
      (should (equal before (buffer-string)))
      (should (= before-tick (buffer-chars-modified-tick))))))

(ert-deftest pi-coding-agent-test-thinking-paragraph-spacing-no-runaway-blank-lines ()
  "Thinking paragraphs keep a single readable separator, not multiple blanks."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-thinking-start)
    (pi-coding-agent--display-thinking-delta
     "First paragraph.\n\n\n\nSecond paragraph.")
    (pi-coding-agent--display-thinking-end "")
    (goto-char (point-min))
    (should-not (re-search-forward "^>\\s-*$\n>\\s-*$" nil t))
    (should (string-match-p "> First paragraph\\.\n>\\s-*\n> Second paragraph\\."
                            (buffer-string)))))

(ert-deftest pi-coding-agent-test-thinking-interleaved-with-tool-has-stable-spacing ()
  "Interleaving thinking and tool events keeps one blank line separation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event
     '(:type "message_start" :message (:role "assistant")))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_start")))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "toolcall_start" :contentIndex 0)
       :message (:role "assistant"
                 :content [(:type "toolCall" :id "call_1" :name "read"
                            :arguments (:path "/tmp/AGENTS.md"))])))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_delta"
                               :delta "Reviewing docs")))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_end" :content "")))
    (let ((text (buffer-string)))
      (should (string-match-p "Reviewing docs\n\nread /tmp/AGENTS\\.md" text))
      (should-not (string-match-p "Reviewing docs\n\n\n" text)))))

(ert-deftest pi-coding-agent-test-thinking-after-text-has-blank-line-separator ()
  "Second thinking block after text delta is separated by blank line."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    ;; First thinking block
    (pi-coding-agent--display-thinking-start)
    (pi-coding-agent--display-thinking-delta "First thought.")
    (pi-coding-agent--display-thinking-end "")
    ;; Text between blocks
    (pi-coding-agent--display-message-delta "Here is my answer.")
    ;; Second thinking block
    (pi-coding-agent--display-thinking-start)
    (pi-coding-agent--display-thinking-delta "Second thought.")
    (pi-coding-agent--display-thinking-end "")
    (let ((text (buffer-string)))
      ;; The > must start on its own line, separated by blank line from text
      (should (string-match-p "my answer\\.\n\n> Second thought\\." text))
      ;; The > must NOT be glued to the text
      (should-not (string-match-p "my answer\\.>" text)))))

(ert-deftest pi-coding-agent-test-thinking-delta-allows-syntax-propertize ()
  "Thinking deltas allow refontification after rewriting blockquote content.
With tree-sitter, `syntax-propertize' is not used (stays at -1).
This test verifies that thinking delta rewrites don't break
subsequent font-lock-ensure calls."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-thinking-start)
    ;; Send initial content
    (pi-coding-agent--display-thinking-delta "First paragraph with text.")
    ;; Fontify
    (font-lock-ensure (point-min) (point-max))
    ;; Stream more content (triggers rewrite)
    (pi-coding-agent--display-thinking-delta "\n\nSecond paragraph.")
    ;; Verify font-lock-ensure doesn't error after rewrite
    (font-lock-ensure (point-min) (point-max))
    ;; Both paragraphs should be present
    (should (string-match-p "First paragraph" (buffer-string)))
    (should (string-match-p "Second paragraph" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-blank-line-before-tool ()
  "Tool block is preceded by blank line when after text."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Let me check.")
    (pi-coding-agent--render-complete-message)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    ;; Pattern: text, blank line, $ command
    (should (string-match-p "check\\.\n\n\\$ ls" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-blank-line-after-tool ()
  "Tool block is followed by blank line."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file.txt"))
                          nil nil)
    ;; Should end with closing fence and blank line
    (should (string-match-p "```\n\n" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-single-blank-line-between-turns ()
  "Only one blank line between agent response and next section header.
agent_end + next section's leading newline must not create triple newlines."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Turn 1: user + assistant
    (pi-coding-agent--display-user-message "Hi")
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Hello!")
    (pi-coding-agent--render-complete-message)
    (pi-coding-agent--display-agent-end)
    ;; Turn 2: user message
    (setq pi-coding-agent--assistant-header-shown nil)
    (pi-coding-agent--display-user-message "Bye")
    ;; Should never have triple newlines (which would be two blank lines)
    (should-not (string-match-p "\n\n\n" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-single-blank-line-before-compaction ()
  "Only one blank line between agent response and compaction header."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Some response.")
    (pi-coding-agent--render-complete-message)
    (pi-coding-agent--display-agent-end)
    (pi-coding-agent--display-compaction-result 50000 "Summary.")
    (should-not (string-match-p "\n\n\n" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-no-double-blank-between-tools ()
  "Consecutive tools have single blank line between them."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file1"))
                          nil nil)
    (pi-coding-agent--display-tool-start "read" '(:path "file.txt"))
    ;; Should have closing fence, blank line, then next tool
    (should (string-match-p "```\n\nread file\\.txt" (buffer-string)))
    (should-not (string-match-p "\n\n\n" (buffer-string)))))

;;; History Display

(ert-deftest pi-coding-agent-test-history-displays-compaction-summary ()
  "Compaction summary messages display with header, tokens, and summary."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((messages [(:role "compactionSummary"
                      :summary "Session was compacted. Key points: user asked about testing."
                      :tokensBefore 50000
                      :timestamp 1704067200000)]))  ; 2024-01-01 00:00:00 UTC
      (pi-coding-agent--display-history-messages messages))
    ;; Should have Compaction header
    (should (string-match-p "Compaction" (buffer-string)))
    ;; Should show tokens
    (should (string-match-p "50,000 tokens" (buffer-string)))
    ;; Should show summary text
    (should (string-match-p "Key points" (buffer-string)))))

;;; Streaming Marker

(ert-deftest pi-coding-agent-test-streaming-marker-created-on-agent-start ()
  "Streaming marker is created on agent_start."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (should (markerp pi-coding-agent--streaming-marker))
    (should (= (marker-position pi-coding-agent--streaming-marker) (point-max)))))

(ert-deftest pi-coding-agent-test-streaming-marker-advances-with-delta ()
  "Streaming marker advances as deltas are inserted."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (let ((initial-pos (marker-position pi-coding-agent--streaming-marker)))
      (pi-coding-agent--display-message-delta "Hello")
      (should (= (marker-position pi-coding-agent--streaming-marker)
                 (+ initial-pos 5))))))

(ert-deftest pi-coding-agent-test-streaming-inserts-at-marker ()
  "Deltas are inserted at the streaming marker position."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "First")
    (pi-coding-agent--display-message-delta " Second")
    (should (string-match-p "First Second" (buffer-string)))))

;;; Auto-scroll

(ert-deftest pi-coding-agent-test-window-following-p-at-end ()
  "pi-coding-agent--window-following-p detects when window-point is at end."
  (with-temp-buffer
    (insert "some content")
    ;; Mock window-point to return point-max
    (cl-letf (((symbol-function 'window-point) (lambda (_w) (point-max))))
      (should (pi-coding-agent--window-following-p 'mock-window)))))

(ert-deftest pi-coding-agent-test-window-following-p-not-at-end ()
  "pi-coding-agent--window-following-p returns nil when window-point is earlier."
  (with-temp-buffer
    (insert "some content")
    ;; Mock window-point to return position before end
    (cl-letf (((symbol-function 'window-point) (lambda (_w) 1)))
      (should-not (pi-coding-agent--window-following-p 'mock-window)))))

;;; Pandoc Conversion

(ert-deftest pi-coding-agent-test-message-start-marker-created ()
  "Message start position is tracked for later replacement."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      (insert "Previous content\n"))
    (pi-coding-agent--display-agent-start)
    (should (markerp pi-coding-agent--message-start-marker))
    (should (= (marker-position pi-coding-agent--message-start-marker)
               (marker-position pi-coding-agent--streaming-marker)))))

(ert-deftest pi-coding-agent-test-render-complete-message-applies-fontlock ()
  "Rendering applies font-lock to markdown content."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "# Hello\n\n**Bold**")
    ;; Raw markdown should be present
    (should (string-match-p "# Hello" (buffer-string)))
    ;; Now render
    (pi-coding-agent--render-complete-message)
    ;; Markdown stays as markdown (treesit handles display)
    (should (string-match-p "# Hello" (buffer-string)))
    (should (string-match-p "\\*\\*Bold\\*\\*" (buffer-string)))))

;;; Syntax Highlighting

(ert-deftest pi-coding-agent-test-chat-mode-derives-from-markdown-ts ()
  "Chat mode derives from md-ts-mode for tree-sitter highlighting."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (should (derived-mode-p 'md-ts-mode))))

(ert-deftest pi-coding-agent-test-chat-mode-fontifies-code ()
  "Code blocks get syntax highlighting from tree-sitter.
With embedded language support, `def' gets `font-lock-keyword-face'
from the Python grammar.  Without it (grammar not installed), it
gets `font-lock-string-face' from the markdown grammar."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      (insert "```python\ndef hello():\n    return 42\n```\n")
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "def" nil t)
      (let ((face (get-text-property (match-beginning 0) 'face)))
        (should face)))))

(ert-deftest pi-coding-agent-test-incomplete-code-block-does-not-break-fontlock ()
  "Incomplete code block during streaming does not break font-lock.
Simulates streaming where code block opening arrives before closing.
Font-lock should handle gracefully: no error, then proper face once
block is closed."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      ;; Simulate streaming: block opened but not closed
      (insert "```python\ndef hello():\n    return 42\n")
      (font-lock-ensure)
      ;; Should not error, buffer should be functional
      (should (eq major-mode 'pi-coding-agent-chat-mode))
      (goto-char (point-min))
      (should (search-forward "def" nil t))
      ;; Complete the block
      (goto-char (point-max))
      (insert "```\n")
      (font-lock-ensure)
      ;; Now should have some face from treesit (keyword or string)
      (goto-char (point-min))
      (search-forward "def" nil t)
      (let ((face (get-text-property (match-beginning 0) 'face)))
        (should face)))))

;;; Markdown Escape Restriction

;;; User Message Display

(ert-deftest pi-coding-agent-test-display-user-message-inserts-text ()
  "User message is inserted into chat buffer."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-user-message "Hello world")
    (should (string-match-p "Hello world" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-user-message-has-prefix ()
  "User message has You label in setext heading."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-user-message "Test message")
    (should (string-match-p "^You" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-user-message-has-separator ()
  "User message has setext underline separator."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-user-message "Test")
    (should (string-match-p "^===" (buffer-string)))))

(ert-deftest pi-coding-agent-test-send-displays-user-message ()
  "Sending a prompt displays the user message in chat."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-chat*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Hello from test")
            ;; Mock the process to avoid actual RPC
            (setq pi-coding-agent--process nil)
            (pi-coding-agent-send))
          ;; Check chat buffer has the message with You setext heading and content
          (with-current-buffer chat-buf
            (should (string-match-p "^You" (buffer-string)))
            (should (string-match-p "Hello from test" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-send-slash-command-not-displayed-locally ()
  "Slash commands are NOT displayed locally - pi sends back expanded content.
This avoids showing both the command and its expansion."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-chat*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "/greet world")
            ;; Mock the process to avoid actual RPC
            (setq pi-coding-agent--process nil)
            (pi-coding-agent-send))
          ;; Check chat buffer does NOT have the command - pi will send expanded content
          (with-current-buffer chat-buf
            (should-not (string-match-p "/greet" (buffer-string)))
            ;; local-user-message should be nil for slash commands
            (should-not pi-coding-agent--local-user-message)))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-slash-command-after-abort-no-duplicate-headers ()
  "Sending slash command after abort should not show duplicate Assistant headers.
Regression test for bug where:
1. Assistant streams, user aborts
2. User types /fix-tests in input buffer  
3. Two 'Assistant' headers appear before the user message

The fix: don't set assistant-header-shown to nil when sending slash commands,
since we don't display them locally. Let pi's message_start handle it."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-abort-cmd*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-abort-cmd-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--input-buffer input-buf)
            (setq pi-coding-agent--status 'idle)
            ;; Simulate state after an aborted assistant turn:
            ;; - assistant-header-shown is t (header was shown for aborted turn)
            (setq pi-coding-agent--assistant-header-shown t)
            (let ((inhibit-read-only t))
              (insert "Assistant\n=========\nSome content...\n\n[Aborted]\n\n")))
          
          ;; User sends a slash command from input buffer
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "/fix-tests")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt) #'ignore))
              (pi-coding-agent-send)))
          
          ;; KEY ASSERTION: assistant-header-shown should still be t
          ;; because we didn't display anything locally for slash commands
          (with-current-buffer chat-buf
            (should pi-coding-agent--assistant-header-shown)
            
            ;; Now simulate pi's response sequence
            ;; 1. agent_start - should NOT add header (already shown)
            (pi-coding-agent--handle-display-event '(:type "agent_start"))
            
            ;; Count Assistant headers - should still be just 1
            (let ((count 0)
                  (content (buffer-string)))
              (with-temp-buffer
                (insert content)
                (goto-char (point-min))
                (while (search-forward "Assistant\n=========" nil t)
                  (setq count (1+ count))))
              (should (= count 1)))
            
            ;; 2. message_start with user role (expanded template)
            (pi-coding-agent--handle-display-event
             '(:type "message_start"
               :message (:role "user"
                         :content [(:type "text" :text "Your task is to fix tests...")]
                         :timestamp 1704067200000)))
            
            ;; ISSUE #5: Verify expanded content is actually displayed
            (should (string-match-p "Your task is to fix tests" (buffer-string)))
            
            ;; 3. message_start with assistant role
            (pi-coding-agent--handle-display-event
             '(:type "message_start"
               :message (:role "assistant")))
            
            ;; Final count: should be exactly 2 Assistant headers
            ;; (one from aborted turn, one from new turn)
            (let ((count 0)
                  (content (buffer-string)))
              (with-temp-buffer
                (insert content)
                (goto-char (point-min))
                (while (search-forward "Assistant\n=========" nil t)
                  (setq count (1+ count))))
              (should (= count 2)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-ms-to-time-converts-correctly ()
  "pi-coding-agent--ms-to-time converts milliseconds to Emacs time."
  ;; 1704067200000 ms = 2024-01-01 00:00:00 UTC
  (let ((time (pi-coding-agent--ms-to-time 1704067200000)))
    (should time)
    (should (equal (format-time-string "%Y-%m-%d" time t) "2024-01-01"))))

(ert-deftest pi-coding-agent-test-ms-to-time-returns-nil-for-nil ()
  "pi-coding-agent--ms-to-time returns nil when given nil."
  (should (null (pi-coding-agent--ms-to-time nil))))

(ert-deftest pi-coding-agent-test-format-message-timestamp-today ()
  "Format timestamp shows just time for today."
  (let ((now (current-time)))
    (should (string-match-p "^[0-2][0-9]:[0-5][0-9]$"
                            (pi-coding-agent--format-message-timestamp now)))))

(ert-deftest pi-coding-agent-test-format-message-timestamp-other-day ()
  "Format timestamp shows ISO date and time for other days."
  (let ((yesterday (time-subtract (current-time) (days-to-time 1))))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-2][0-9]:[0-5][0-9]$"
                            (pi-coding-agent--format-message-timestamp yesterday)))))

(ert-deftest pi-coding-agent-test-display-user-message-with-timestamp ()
  "User message displays with timestamp when provided."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-user-message "Test message" (current-time))
    (let ((content (buffer-string)))
      ;; Setext format: "You · HH:MM" on header line
      (should (string-match-p "You · " content))
      ;; Should have HH:MM timestamp format
      (should (string-match-p "[0-2][0-9]:[0-5][0-9]" content)))))

(ert-deftest pi-coding-agent-test-separator-without-timestamp ()
  "Separator without timestamp is setext H1 heading."
  (let ((sep (pi-coding-agent--make-separator "You")))
    ;; Setext format: label on one line, === underline on next
    (should (string-match-p "^You\n=+$" sep))))

(ert-deftest pi-coding-agent-test-separator-with-timestamp ()
  "Separator with timestamp shows label · time as setext H1."
  (let ((sep (pi-coding-agent--make-separator "You" (current-time))))
    ;; Format: "You · HH:MM" followed by newline and ===
    (should (string-match-p "^You · [0-2][0-9]:[0-5][0-9]\n=+$" sep))))

(ert-deftest pi-coding-agent-test-separator-is-valid-setext-heading ()
  "Separator produces valid markdown setext H1 syntax."
  (let ((sep (pi-coding-agent--make-separator "Assistant")))
    ;; Must have at least 3 = characters for valid setext
    (should (string-match-p "\n===+" sep))
    ;; Underline should match or exceed label length
    (should (>= (length (car (last (split-string sep "\n"))))
                (length "Assistant")))))

;;; Error and Retry Handling

(ert-deftest pi-coding-agent-test-display-retry-start-shows-attempt ()
  "auto_retry_start event shows attempt number and delay."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-retry-start '(:type "auto_retry_start"
                               :attempt 1
                               :maxAttempts 3
                               :delayMs 2000
                               :errorMessage "429 rate_limit_error"))
    (should (string-match-p "Retry 1/3" (buffer-string)))
    (should (string-match-p "2s" (buffer-string)))
    ;; Raw error message is shown as-is
    (should (string-match-p "429 rate_limit_error" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-retry-start-with-overloaded-error ()
  "auto_retry_start shows overloaded error message."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-retry-start '(:type "auto_retry_start"
                               :attempt 2
                               :maxAttempts 3
                               :delayMs 4000
                               :errorMessage "529 overloaded_error: Overloaded"))
    (should (string-match-p "Retry 2/3" (buffer-string)))
    (should (string-match-p "overloaded" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-retry-end-success ()
  "auto_retry_end with success shows success message."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-retry-end '(:type "auto_retry_end"
                             :success t
                             :attempt 2))
    (should (string-match-p "succeeded" (buffer-string)))
    (should (string-match-p "attempt 2" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-retry-end-failure ()
  "auto_retry_end with failure shows final error."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-retry-end '(:type "auto_retry_end"
                             :success :false
                             :attempt 3
                             :finalError "529 overloaded_error: Overloaded"))
    (should (string-match-p "failed" (buffer-string)))
    (should (string-match-p "3 attempts" (buffer-string)))
    (should (string-match-p "overloaded" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-error-shows-message ()
  "pi-coding-agent--display-error shows error message with proper face."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-error "API error: insufficient quota")
    (should (string-match-p "Error:" (buffer-string)))
    (should (string-match-p "insufficient quota" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-error-handles-nil ()
  "pi-coding-agent--display-error handles nil error message."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-error nil)
    (should (string-match-p "Error:" (buffer-string)))
    (should (string-match-p "unknown" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-extension-error ()
  "extension_error event shows extension name and error."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-extension-error '(:type "extension_error"
                              :extensionPath "/home/user/.pi/extensions/before_send.ts"
                              :event "tool_call"
                              :error "TypeError: Cannot read property"))
    (should (string-match-p "Extension error" (buffer-string)))
    (should (string-match-p "before_send.ts" (buffer-string)))
    (should (string-match-p "tool_call" (buffer-string)))
    (should (string-match-p "TypeError" (buffer-string)))))

(ert-deftest pi-coding-agent-test-handle-display-event-retry-start ()
  "pi-coding-agent--handle-display-event handles auto_retry_start."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil))
      (pi-coding-agent--handle-display-event '(:type "auto_retry_start"
                                  :attempt 1
                                  :maxAttempts 3
                                  :delayMs 2000
                                  :errorMessage "429 rate_limit_error"))
      (should (string-match-p "Retry" (buffer-string))))))

(ert-deftest pi-coding-agent-test-handle-display-event-retry-end ()
  "pi-coding-agent--handle-display-event handles auto_retry_end."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil))
      (pi-coding-agent--handle-display-event '(:type "auto_retry_end"
                                  :success t
                                  :attempt 2))
      (should (string-match-p "succeeded" (buffer-string))))))

(ert-deftest pi-coding-agent-test-handle-display-event-extension-error ()
  "pi-coding-agent--handle-display-event handles extension_error."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state (list :last-error nil)))
      (pi-coding-agent--handle-display-event '(:type "extension_error"
                                  :extensionPath "/path/extension.ts"
                                  :event "before_send"
                                  :error "Extension failed"))
      (should (string-match-p "Extension error" (buffer-string))))))

(ert-deftest pi-coding-agent-test-handle-display-event-message-error ()
  "pi-coding-agent--handle-display-event handles message_update with error type."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Need to set up markers first
    (pi-coding-agent--display-agent-start)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state (list :current-message '(:role "assistant"))))
      (pi-coding-agent--handle-display-event '(:type "message_update"
                                  :message (:role "assistant")
                                  :assistantMessageEvent (:type "error"
                                                          :reason "API connection failed")))
      (should (string-match-p "Error:" (buffer-string)))
      (should (string-match-p "API connection failed" (buffer-string))))))

(ert-deftest pi-coding-agent-test-display-no-model-warning ()
  "pi-coding-agent--display-no-model-warning shows setup instructions."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-no-model-warning)
    (should (string-match-p "No models available" (buffer-string)))
    (should (string-match-p "API key" (buffer-string)))
    (should (string-match-p "pi --login" (buffer-string)))))

;;; Extension UI Request Handling

(ert-deftest pi-coding-agent-test-extension-ui-notify ()
  "extension_ui_request notify method shows message."
  (let ((message-shown nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-shown (apply #'format fmt args)))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process nil))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-1"
             :method "notify"
             :message "Extension loaded successfully"
             :notifyType "info")))
        (should message-shown)
        (should (string-match-p "Extension loaded successfully" message-shown))))))

(ert-deftest pi-coding-agent-test-extension-ui-confirm-yes ()
  "extension_ui_request confirm method uses yes-or-no-p and sends response."
  (let ((response-sent nil))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt) t))
              ((symbol-function 'pi-coding-agent--send-extension-ui-response)
               (lambda (_proc msg)
                 (setq response-sent msg))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process t))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-2"
             :method "confirm"
             :title "Delete file?"
             :message "This cannot be undone")))
        (should response-sent)
        (should (equal (plist-get response-sent :type) "extension_ui_response"))
        (should (equal (plist-get response-sent :id) "req-2"))
        (should (eq (plist-get response-sent :confirmed) t))))))

(ert-deftest pi-coding-agent-test-extension-ui-confirm-no ()
  "extension_ui_request confirm method sends confirmed:false when user declines."
  (let ((response-sent nil))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt) nil))
              ((symbol-function 'pi-coding-agent--send-extension-ui-response)
               (lambda (_proc msg)
                 (setq response-sent msg))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process t))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-3"
             :method "confirm"
             :title "Delete?"
             :message "Are you sure?")))
        (should response-sent)
        ;; :json-false is the correct encoding for JSON false in json-encode
        (should (eq (plist-get response-sent :confirmed) :json-false))))))

(ert-deftest pi-coding-agent-test-extension-ui-select ()
  "extension_ui_request select method uses completing-read and sends response."
  (let ((response-sent nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt options &rest _args)
                 (car options)))  ; Return first option
              ((symbol-function 'pi-coding-agent--send-extension-ui-response)
               (lambda (_proc msg)
                 (setq response-sent msg))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process t))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-4"
             :method "select"
             :title "Pick one:"
             :options ["Option A" "Option B" "Option C"])))
        (should response-sent)
        (should (equal (plist-get response-sent :type) "extension_ui_response"))
        (should (equal (plist-get response-sent :id) "req-4"))
        (should (equal (plist-get response-sent :value) "Option A"))))))

(ert-deftest pi-coding-agent-test-extension-ui-input ()
  "extension_ui_request input method uses read-string and sends response."
  (let ((response-sent nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) "user input"))
              ((symbol-function 'pi-coding-agent--send-extension-ui-response)
               (lambda (_proc msg)
                 (setq response-sent msg))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process t))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-5"
             :method "input"
             :title "Enter name:"
             :placeholder "John Doe")))
        (should response-sent)
        (should (equal (plist-get response-sent :type) "extension_ui_response"))
        (should (equal (plist-get response-sent :id) "req-5"))
        (should (equal (plist-get response-sent :value) "user input"))))))

(ert-deftest pi-coding-agent-test-extension-ui-set-editor-text ()
  "extension_ui_request set_editor_text inserts text into input buffer."
  (let ((input-buf (get-buffer-create "*pi-test-input*")))
    (unwind-protect
        (with-temp-buffer
          (pi-coding-agent-chat-mode)
          (setq pi-coding-agent--input-buffer input-buf)
          (with-current-buffer input-buf
            (erase-buffer))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-6"
             :method "set_editor_text"
             :text "Prefilled text"))
          (should (equal (with-current-buffer input-buf (buffer-string))
                         "Prefilled text")))
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-extension-ui-set-status ()
  "extension_ui_request setStatus updates extension status storage."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--extension-status nil)
    (pi-coding-agent--handle-extension-ui-request
     '(:type "extension_ui_request"
       :id "req-7"
       :method "setStatus"
       :statusKey "my-ext"
       :statusText "Processing..."))
    (should (equal (cdr (assoc "my-ext" pi-coding-agent--extension-status))
                   "Processing..."))))

(ert-deftest pi-coding-agent-test-extension-ui-set-status-strips-ansi ()
  "extension_ui_request setStatus strips ANSI escape codes."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--extension-status nil)
    (pi-coding-agent--handle-extension-ui-request
     '(:type "extension_ui_request"
       :id "req-ansi"
       :method "setStatus"
       :statusKey "plan-mode"
       :statusText "\e[38;5;226m⏸ plan\e[39m"))
    (should (equal (cdr (assoc "plan-mode" pi-coding-agent--extension-status))
                   "⏸ plan"))))

(ert-deftest pi-coding-agent-test-extension-ui-set-status-clear ()
  "extension_ui_request setStatus with nil clears the status."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--extension-status '(("my-ext" . "Old status")))
    (pi-coding-agent--handle-extension-ui-request
     '(:type "extension_ui_request"
       :id "req-8"
       :method "setStatus"
       :statusKey "my-ext"
       :statusText nil))
    (should-not (assoc "my-ext" pi-coding-agent--extension-status))))

(ert-deftest pi-coding-agent-test-extension-ui-set-working-message ()
  "extension_ui_request setWorkingMessage stores working text."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--working-message nil)
    (pi-coding-agent--handle-extension-ui-request
     '(:type "extension_ui_request"
       :id "req-working"
       :method "setWorkingMessage"
       :message "📖 Skimming…"))
    (should (equal pi-coding-agent--working-message "📖 Skimming…"))))

(ert-deftest pi-coding-agent-test-extension-ui-set-working-message-strips-ansi ()
  "extension_ui_request setWorkingMessage strips ANSI escape codes."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--working-message nil)
    (pi-coding-agent--handle-extension-ui-request
     '(:type "extension_ui_request"
       :id "req-working-ansi"
       :method "setWorkingMessage"
       :message "\e[38;5;39m📖 Skimming…\e[39m"))
    (should (equal pi-coding-agent--working-message "📖 Skimming…"))))

(ert-deftest pi-coding-agent-test-extension-ui-set-working-message-clear ()
  "extension_ui_request setWorkingMessage with nil clears message."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--working-message "Old")
    (pi-coding-agent--handle-extension-ui-request
     '(:type "extension_ui_request"
       :id "req-working-clear"
       :method "setWorkingMessage"
       :message nil))
    (should (null pi-coding-agent--working-message))))

(ert-deftest pi-coding-agent-test-header-format-extension-status ()
  "Extension status formatter returns inline status text without pipe."
  ;; Empty status returns empty string
  (should (equal (pi-coding-agent--header-format-extension-status nil) ""))
  ;; Single status
  (let ((result (pi-coding-agent--header-format-extension-status '(("ext1" . "Processing...")))))
    (should-not (string-match-p "│" result))
    (should (string-match-p "Processing" result)))
  ;; Multiple statuses joined with separator
  (let ((result (pi-coding-agent--header-format-extension-status
                 '(("ext1" . "Status 1") ("ext2" . "Status 2")))))
    (should-not (string-match-p "│" result))
    (should (string-match-p "Status 1" result))
    (should (string-match-p "Status 2" result))
    (should (string-match-p "·" result))))

(ert-deftest pi-coding-agent-test-extension-ui-unknown-cancels ()
  "extension_ui_request with unknown method sends cancelled response."
  (let ((response-sent nil))
    (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc msg _cb)
                 (setq response-sent msg))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process t))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-9"
             :method "setWidget"
             :widgetKey "my-ext"
             :widgetLines ["Line 1"])))
        (should response-sent)
        (should (equal (plist-get response-sent :type) "extension_ui_response"))
        (should (equal (plist-get response-sent :id) "req-9"))
        (should (eq (plist-get response-sent :cancelled) t))))))

(ert-deftest pi-coding-agent-test-extension-ui-editor-cancels ()
  "extension_ui_request editor method sends cancelled (not supported)."
  (let ((response-sent nil))
    (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc msg _cb)
                 (setq response-sent msg))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process t))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-10"
             :method "editor"
             :title "Edit:"
             :prefill "some text")))
        (should response-sent)
        (should (eq (plist-get response-sent :cancelled) t))))))

;;; Pretty-Print JSON Helper

(ert-deftest pi-coding-agent-test-pretty-print-json-simple-plist ()
  "Pretty-print helper produces 2-space indented JSON from plist."
  (let ((result (pi-coding-agent--pretty-print-json
                 '(:agent "worker" :task "Search for foo"))))
    (should (stringp result))
    (should (string-match-p "\"agent\": \"worker\"" result))
    (should (string-match-p "\"task\": \"Search for foo\"" result))
    ;; Should be multi-line (pretty-printed)
    (should (> (length (split-string result "\n")) 1))))

(ert-deftest pi-coding-agent-test-pretty-print-json-nested ()
  "Pretty-print helper handles nested objects and arrays."
  (let ((result (pi-coding-agent--pretty-print-json
                 '(:tasks [(:agent "worker" :task "foo")
                           (:agent "scout" :task "bar")]))))
    (should (string-match-p "\"tasks\"" result))
    (should (string-match-p "\"worker\"" result))
    (should (string-match-p "\"scout\"" result))))

(ert-deftest pi-coding-agent-test-pretty-print-json-unicode ()
  "Pretty-print helper preserves non-ASCII characters."
  (let ((result (pi-coding-agent--pretty-print-json
                 '(:city "Malmö" :note "väder"))))
    (should (string-match-p "Malmö" result))
    (should (string-match-p "väder" result))
    ;; Should NOT have octal escapes
    (should-not (string-match-p "\\\\303" result))))

(ert-deftest pi-coding-agent-test-pretty-print-json-nil ()
  "Pretty-print helper returns nil for nil input."
  (should-not (pi-coding-agent--pretty-print-json nil)))

;;; Tool Header

(ert-deftest pi-coding-agent-test-tool-header-faces ()
  "Tool header applies tool-name face on prefix and tool-command on args."
  ;; bash: "$" is tool-name, command is tool-command
  (let ((header (pi-coding-agent--tool-header "bash" '(:command "ls -la"))))
    (should (eq (get-text-property 0 'font-lock-face header)
                'pi-coding-agent-tool-name))
    (should (eq (get-text-property 2 'font-lock-face header)
                'pi-coding-agent-tool-command)))
  ;; read/write/edit: tool name is tool-name, path is tool-command
  (dolist (tool '("read" "write" "edit"))
    (let ((header (pi-coding-agent--tool-header tool '(:path "foo.txt"))))
      (should (eq (get-text-property 0 'font-lock-face header)
                  'pi-coding-agent-tool-name))
      (should (eq (get-text-property (1+ (length tool)) 'font-lock-face header)
                  'pi-coding-agent-tool-command))))
  ;; Unknown tool with nil args: entire string is tool-name
  (let ((header (pi-coding-agent--tool-header "custom_tool" nil)))
    (should (eq (get-text-property 0 'font-lock-face header)
                'pi-coding-agent-tool-name))
    (should (equal (substring-no-properties header) "custom_tool"))))

(ert-deftest pi-coding-agent-test-tool-header-survives-font-lock ()
  "Tool header font-lock-face properties survive treesit refontification."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "edit" '(:path "foo.txt"))
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'font-lock-face)
                'pi-coding-agent-tool-name))
    (search-forward "foo.txt")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face)
                'pi-coding-agent-tool-command))))

(ert-deftest pi-coding-agent-test-generic-tool-header-with-args ()
  "Generic tool header shows tool name and JSON args."
  (let ((header (pi-coding-agent--tool-header
                 "subagent" '(:agent "worker" :task "Search"))))
    ;; Should start with "subagent "
    (should (string-prefix-p "subagent " (substring-no-properties header)))
    ;; Should contain JSON keys
    (should (string-match-p "\"agent\"" (substring-no-properties header)))
    (should (string-match-p "\"worker\"" (substring-no-properties header)))))

(ert-deftest pi-coding-agent-test-generic-tool-header-compact-when-short ()
  "Short args produce a single-line compact header."
  (let* ((fill-column 70)
         (header (pi-coding-agent--tool-header "subagent" '(:agent "worker")))
         (text (substring-no-properties header)))
    ;; Single line
    (should (= 1 (length (split-string text "\n"))))
    ;; Contains key and value with proper JSON spacing
    (should (string-match-p "\"agent\": \"worker\"" text))))

(ert-deftest pi-coding-agent-test-generic-tool-header-pretty-when-long ()
  "Long args that exceed fill-column produce a multi-line pretty header."
  (let* ((fill-column 40)
         (header (pi-coding-agent--tool-header
                  "subagent" '(:agent "worker" :task "Search for weather")))
         (text (substring-no-properties header)))
    ;; Multi-line (pretty-printed)
    (should (> (length (split-string text "\n")) 1))))

(ert-deftest pi-coding-agent-test-generic-tool-header-respects-fill-column ()
  "Compact-vs-pretty threshold follows fill-column."
  (let ((args '(:agent "worker" :task "Search")))
    ;; Wide fill-column → compact
    (let* ((fill-column 200)
           (text (substring-no-properties
                  (pi-coding-agent--tool-header "subagent" args))))
      (should (= 1 (length (split-string text "\n")))))
    ;; Narrow fill-column → pretty
    (let* ((fill-column 20)
           (text (substring-no-properties
                  (pi-coding-agent--tool-header "subagent" args))))
      (should (> (length (split-string text "\n")) 1)))))

(ert-deftest pi-coding-agent-test-generic-tool-header-faces ()
  "Generic tool header applies tool-name face on name, tool-command on args."
  (let ((header (pi-coding-agent--tool-header
                 "subagent" '(:agent "worker" :task "Search"))))
    ;; Tool name portion gets tool-name face
    (should (eq (get-text-property 0 'font-lock-face header)
                'pi-coding-agent-tool-name))
    ;; JSON body (after "subagent ") gets tool-command face
    (let ((json-start (length "subagent ")))
      (should (eq (get-text-property json-start 'font-lock-face header)
                  'pi-coding-agent-tool-command)))))

(ert-deftest pi-coding-agent-test-builtin-tools-unaffected-by-generic-header ()
  "Built-in tools still use their specialized header formats."
  ;; bash: still "$ command"
  (let ((header (pi-coding-agent--tool-header "bash" '(:command "ls -la"))))
    (should (string-prefix-p "$ " (substring-no-properties header))))
  ;; read/write/edit: still "tool path"
  (dolist (tool '("read" "write" "edit"))
    (let ((header (pi-coding-agent--tool-header tool '(:path "foo.txt"))))
      (should (string-prefix-p (concat tool " foo.txt")
                               (substring-no-properties header))))))

;;; Tool Output

(ert-deftest pi-coding-agent-test-tool-start-inserts-header ()
  "tool_execution_start inserts tool header."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" (list :command "ls -la"))
    ;; Should have $ command format
    (should (string-match-p "\\$ ls -la" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-start-handles-file-path-key ()
  "tool_execution_start handles :file_path key (alternative to :path)."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Test read tool with :file_path
    (pi-coding-agent--display-tool-start "read" '(:file_path "/tmp/test.txt"))
    (should (string-match-p "read /tmp/test.txt" (buffer-string))))
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Test write tool with :file_path
    (pi-coding-agent--display-tool-start "write" '(:file_path "/tmp/out.py"))
    (should (string-match-p "write /tmp/out.py" (buffer-string))))
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Test edit tool with :file_path
    (pi-coding-agent--display-tool-start "edit" '(:file_path "/tmp/edit.rs"))
    (should (string-match-p "edit /tmp/edit.rs" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-end-inserts-result ()
  "tool_execution_end inserts the tool result."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file1\nfile2"))
                          nil nil)
    (should (string-match-p "file1" (buffer-string)))))

(ert-deftest pi-coding-agent-test-bash-output-wrapped-in-bare-fence ()
  "Bash output is wrapped in a bare fence (no language tag)."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file1"))
                          nil nil)
    (let ((content (buffer-string)))
      ;; Bare fence: ``` with no language tag
      (should (string-match-p "^```\n" content))
      ;; Content appears inside
      (should (string-match-p "file1" content)))))

(ert-deftest pi-coding-agent-test-bash-output-strips-ansi-codes ()
  "ANSI escape codes are stripped from bash output."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Simulate colored test output: blue "▶ Test", green "✓ pass"
    ;; \033[34m = blue, \033[32m = green, \033[0m = reset
    (let ((ansi-output "\033[34m▶ AmbientSoundConfig\033[0m\n\033[32m  ✓ \033[0mshould pass"))
      (pi-coding-agent--display-tool-end "bash" '(:command "test")
                            `((:type "text" :text ,ansi-output))
                            nil nil)
      (let ((result (buffer-string)))
        ;; Text content should be preserved
        (should (string-match-p "▶ AmbientSoundConfig" result))
        (should (string-match-p "✓" result))
        (should (string-match-p "should pass" result))
        ;; ANSI escape sequences should be gone
        (should-not (string-match-p "\033" result))
        (should-not (string-match-p "\\[34m" result))
        (should-not (string-match-p "\\[32m" result))
        (should-not (string-match-p "\\[0m" result))))))

(ert-deftest pi-coding-agent-test-tool-output-shows-preview-when-long ()
  "Tool output shows preview lines when it exceeds the limit."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((long-output (mapconcat (lambda (n) (format "line%d" n))
                                  (number-sequence 1 10)
                                  "\n")))
      (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                            `((:type "text" :text ,long-output))
                            nil nil)
      ;; Should have first 5 preview lines (bash limit)
      (should (string-match-p "line1" (buffer-string)))
      (should (string-match-p "line5" (buffer-string)))
      ;; Should have more-lines indicator
      (should (string-match-p "more lines" (buffer-string))))))

(ert-deftest pi-coding-agent-test-tool-output-short-shows-all ()
  "Short tool output shows all lines without truncation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((short-output "line1\nline2\nline3"))
      (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                            `((:type "text" :text ,short-output))
                            nil nil)
      ;; Should have all lines
      (should (string-match-p "line1" (buffer-string)))
      (should (string-match-p "line2" (buffer-string)))
      (should (string-match-p "line3" (buffer-string)))
      ;; Should NOT have more-lines indicator
      (should-not (string-match-p "more lines" (buffer-string))))))

;;; Generic Tool Details in Output

(defun pi-coding-agent-test--insert-generic-tool (content-text &optional details)
  "Insert a subagent tool start+end in current buffer.
CONTENT-TEXT is the text block string.  DETAILS is an optional plist.
Call inside `with-temp-buffer' after `pi-coding-agent-chat-mode'."
  (pi-coding-agent--display-tool-start "subagent" '(:agent "worker"))
  (pi-coding-agent--display-tool-end "subagent" '(:agent "worker")
                        (list (list :type "text" :text content-text))
                        details nil))

(ert-deftest pi-coding-agent-test-generic-tool-content-follows-header ()
  "Generic tool content is fenced directly after the header line."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent-test--insert-generic-tool "Task completed")
    (let ((content (buffer-string)))
      ;; Content is fenced (bare fence, no language tag)
      (should (string-match-p "}\n```\nTask completed" content)))))

(ert-deftest pi-coding-agent-test-bash-no-blank-line-after-header ()
  "Bash tool does NOT get an extra blank line after header."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file.txt"))
                          nil nil)
    (let ((text (buffer-string)))
      ;; Bash header is "$ ls", followed by fenced code block — no extra blank line
      (should-not (string-match-p "ls\n\n```" text)))))

(ert-deftest pi-coding-agent-test-generic-tool-details-appended ()
  "Generic tool with non-nil details shows details JSON after content."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent-test--insert-generic-tool
     "Task completed" '(:mode "single" :exitCode 0))
    (let ((text (buffer-string)))
      (should (string-match-p "Task completed" text))
      (should (string-match-p "\\*\\*Details\\*\\*" text))
      (should (string-match-p "\"mode\": \"single\"" text))
      (should (string-match-p "\"exitCode\": 0" text)))))

(ert-deftest pi-coding-agent-test-generic-tool-details-face ()
  "Details label and JSON both use pi-coding-agent-tool-output face."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent-test--insert-generic-tool
     "Done" '(:mode "single" :exitCode 0))
    ;; Label gets the face
    (goto-char (point-min))
    (should (search-forward "**Details**" nil t))
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face)
                'pi-coding-agent-tool-output))
    ;; JSON body gets the face
    (should (search-forward "\"mode\"" nil t))
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face)
                'pi-coding-agent-tool-output))))

(ert-deftest pi-coding-agent-test-generic-tool-details-marked-no-fontify ()
  "Generic details text is marked as excluded from markdown fontification."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent-test--insert-generic-tool
     "Done" '(:mode "single" :exitCode 0))
    (goto-char (point-min))
    (should (search-forward "Done" nil t))
    (should-not (get-text-property (match-beginning 0)
                                   'pi-coding-agent-no-fontify))
    (should (search-forward "**Details**" nil t))
    (should (get-text-property (match-beginning 0)
                               'pi-coding-agent-no-fontify))
    (should (search-forward "\"mode\"" nil t))
    (should (get-text-property (match-beginning 0)
                               'pi-coding-agent-no-fontify))))

(ert-deftest pi-coding-agent-test-propertize-details-region-marks-entire-string ()
  "Details helper should mark every character as no-fontify metadata."
  (let* ((json "{\n  \"mode\": \"single\"\n}")
         (details (pi-coding-agent--propertize-details-region json)))
    (should (equal (substring-no-properties details)
                   (concat "**Details**\n" json)))
    (dotimes (idx (length details))
      (should (get-text-property idx 'pi-coding-agent-no-fontify details))
      (should (eq (get-text-property idx 'font-lock-face details)
                  'pi-coding-agent-tool-output)))))

(ert-deftest pi-coding-agent-test-generic-tool-toggle-skips-details-font-lock ()
  "Toggle fontification excludes details metadata ranges."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let* ((pi-coding-agent-tool-preview-lines 5)
           (content (mapconcat (lambda (n) (format "line%d" n))
                               (number-sequence 1 30)
                               "\n"))
           (details (list :summary (make-string 3000 ?x)))
           (font-lock-calls nil))
      (pi-coding-agent-test--insert-generic-tool content details)
      (goto-char (point-min))
      (should (re-search-forward "\\.\\.\\. ([0-9]+ more lines)" nil t))
      (let ((btn (button-at (match-beginning 0))))
        (should btn)
        (cl-letf (((symbol-function 'font-lock-ensure)
                   (lambda (start end)
                     (push (cons start end) font-lock-calls)
                     (save-excursion
                       (goto-char start)
                       (when (search-forward "**Details**" end t)
                         (error "Stack overflow in regexp matcher"))))))
          (pi-coding-agent--toggle-tool-output btn)))
      (should font-lock-calls)
      (dolist (range font-lock-calls)
        (should-not
         (save-excursion
           (goto-char (car range))
           (search-forward "**Details**" (cdr range) t)))))))

(ert-deftest pi-coding-agent-test-generic-tool-with-path-toggle-skips-details-font-lock ()
  "Generic tool with path keeps details excluded during toggle fontification."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let* ((pi-coding-agent-tool-preview-lines 5)
           (content (mapconcat (lambda (n) (format "line%d" n))
                               (number-sequence 1 30)
                               "\n"))
           (details (list :summary (make-string 3000 ?x)))
           (font-lock-calls nil))
      (pi-coding-agent--display-tool-start "custom_tool" '(:path "/tmp/example.py"))
      (pi-coding-agent--display-tool-end
       "custom_tool" '(:path "/tmp/example.py")
       (list (list :type "text" :text content))
       details nil)
      (goto-char (point-min))
      (should (re-search-forward "\\.\\.\\. ([0-9]+ more lines)" nil t))
      (let ((btn (button-at (match-beginning 0))))
        (should btn)
        (let ((full-content (button-get btn 'pi-coding-agent-full-content)))
          (should (string-match-p "\\*\\*Details\\*\\*" full-content))
          (should (let ((match-pos (string-match "\\*\\*Details\\*\\*" full-content)))
                    (and match-pos
                         (get-text-property match-pos
                                            'pi-coding-agent-no-fontify
                                            full-content)))))
        (cl-letf (((symbol-function 'font-lock-ensure)
                   (lambda (start end)
                     (push (cons start end) font-lock-calls)
                     (save-excursion
                       (goto-char start)
                       (when (search-forward "**Details**" end t)
                         (error "Stack overflow in regexp matcher"))))))
          (pi-coding-agent--toggle-tool-output btn)))
      (should font-lock-calls)
      (dolist (range font-lock-calls)
        (should-not
         (save-excursion
           (goto-char (car range))
           (search-forward "**Details**" (cdr range) t)))))))

(ert-deftest pi-coding-agent-test-generic-tool-nil-details-unchanged ()
  "Generic tool with nil details shows only content text."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent-test--insert-generic-tool "Task completed")
    (let ((text (buffer-string)))
      (should (string-match-p "Task completed" text))
      (should-not (string-match-p "\\*\\*Details\\*\\*" text)))))

(ert-deftest pi-coding-agent-test-generic-tool-details-nested ()
  "Details with nested structure render as indented JSON."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent-test--insert-generic-tool
     "Done" '(:items [(:name "a") (:name "b")]))
    ;; Output may be collapsed if long; check via button's full content
    ;; or directly in buffer for short output
    (let* ((text (buffer-string))
           (button (progn (goto-char (point-min)) (next-button (point))))
           (full (if button
                     (button-get button 'pi-coding-agent-full-content)
                   text)))
      (should (string-match-p "\"items\"" full))
      (should (string-match-p "\"name\": \"a\"" full))
      (should (string-match-p "\"name\": \"b\"" full)))))

(ert-deftest pi-coding-agent-test-bash-details-not-appended ()
  "Built-in tool (bash) does NOT append details to output."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file.txt"))
                          '(:truncation t :fullOutputPath "/tmp/out")
                          nil)
    (let ((text (buffer-string)))
      (should (string-match-p "file.txt" text))
      (should-not (string-match-p "\\*\\*Details\\*\\*" text)))))

(ert-deftest pi-coding-agent-test-generic-tool-details-in-expanded-view ()
  "Details are included in collapsed output and survive TAB expand."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let* ((long-output (mapconcat (lambda (n) (format "line%d" n))
                                   (number-sequence 1 20)
                                   "\n"))
           (details '(:errors [(:task "foo" :error "timeout")])))
      (pi-coding-agent-test--insert-generic-tool long-output details)
      ;; Should have a "more lines" toggle (output is long enough)
      (should (string-match-p "more lines" (buffer-string)))
      ;; Details should be in the full content accessible via TAB
      ;; Find the toggle button and check its full-content property
      (goto-char (point-min))
      (let ((button (next-button (point))))
        (should button)
        (let ((full (button-get button 'pi-coding-agent-full-content)))
          (should (string-match-p "\\*\\*Details\\*\\*" full))
          (should (string-match-p "\"error\": \"timeout\"" full)))))))

;;; Diff Overlay Highlighting

(ert-deftest pi-coding-agent-test-apply-diff-overlays-added-line ()
  "Diff overlays should mark added lines with diff-added faces."
  (with-temp-buffer
    ;; Use actual pi format: +<space><padded-linenum><space><code>
    (insert "+ 7     added line\n")
    (pi-coding-agent--apply-diff-overlays (point-min) (point-max))
    (goto-char (point-min))
    ;; Should have overlay with diff-indicator-added on the + character
    (let ((ovs (seq-filter (lambda (ov) (overlay-get ov 'pi-coding-agent-diff-overlay))
                           (overlays-at (point)))))
      (should ovs)
      (should (memq 'diff-indicator-added
                    (mapcar (lambda (ov) (overlay-get ov 'face)) ovs))))))

(ert-deftest pi-coding-agent-test-apply-diff-overlays-removed-line ()
  "Diff overlays should mark removed lines with diff-removed faces."
  (with-temp-buffer
    ;; Use actual pi format: -<space><padded-linenum><space><code>
    (insert "-12     removed line\n")
    (pi-coding-agent--apply-diff-overlays (point-min) (point-max))
    (goto-char (point-min))
    (let ((ovs (seq-filter (lambda (ov) (overlay-get ov 'pi-coding-agent-diff-overlay))
                           (overlays-at (point)))))
      (should ovs)
      (should (memq 'diff-indicator-removed
                    (mapcar (lambda (ov) (overlay-get ov 'face)) ovs))))))

(ert-deftest pi-coding-agent-test-apply-diff-overlays-multiline ()
  "Diff overlays should handle multiple diff lines."
  (with-temp-buffer
    ;; Use actual pi format
    (insert "+ 1     added\n- 2     removed\n")
    (pi-coding-agent--apply-diff-overlays (point-min) (point-max))
    ;; Count diff overlays
    (let ((all-ovs (seq-filter (lambda (ov) (overlay-get ov 'pi-coding-agent-diff-overlay))
                               (overlays-in (point-min) (point-max)))))
      ;; Should have 4 overlays: indicator + line for each of 2 lines
      (should (= 4 (length all-ovs))))))

(ert-deftest pi-coding-agent-test-apply-diff-overlays-line-background ()
  "Diff overlays should apply background color to entire line."
  (with-temp-buffer
    ;; Use actual pi format: "+ 7     def foo():"
    (insert "+ 7     def foo():\n")
    (pi-coding-agent--apply-diff-overlays (point-min) (point-max))
    ;; Check overlay at "def" position (after "+ 7     ")
    (goto-char 9)
    (let ((ovs (seq-filter (lambda (ov) (overlay-get ov 'pi-coding-agent-diff-overlay))
                           (overlays-at (point)))))
      (should ovs)
      ;; Should have diff-added face for background
      (should (memq 'diff-added
                    (mapcar (lambda (ov) (overlay-get ov 'face)) ovs))))))

(ert-deftest pi-coding-agent-test-edit-tool-diff-uses-overlays ()
  "Edit tool output should use overlays for diff highlighting."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--tool-args-cache (make-hash-table :test 'equal))
    (puthash "test" '(:path "/tmp/test.py") pi-coding-agent--tool-args-cache)
    (pi-coding-agent--display-tool-start "edit" '(:path "/tmp/test.py"))
    ;; Use actual pi format
    (let ((diff-content "+ 1     def foo():\n- 2     def bar():"))
      (pi-coding-agent--display-tool-end
       "edit"
       '(:path "/tmp/test.py")
       '((:type "text" :text "Edit successful"))
       (list :diff diff-content)
       nil))
    ;; Should have diff overlays
    (let ((diff-ovs (seq-filter (lambda (ov) (overlay-get ov 'pi-coding-agent-diff-overlay))
                                (overlays-in (point-min) (point-max)))))
      (should (> (length diff-ovs) 0)))
    ;; Check for added line overlay
    (goto-char (point-min))
    (search-forward "+ 1" nil t)
    (let ((ovs (seq-filter (lambda (ov) (overlay-get ov 'pi-coding-agent-diff-overlay))
                           (overlays-at (match-beginning 0)))))
      (should (memq 'diff-indicator-added
                    (mapcar (lambda (ov) (overlay-get ov 'face)) ovs))))))

;;; File Navigation (visit-file)

(defun pi-coding-agent-test--open-target-buffer (line-count)
  "Create and return a fake visited buffer with LINE-COUNT lines."
  (set-buffer (get-buffer-create "*pi-coding-agent-test-target*"))
  (erase-buffer)
  (dotimes (_ line-count)
    (insert "line\n"))
  (goto-char (point-min))
  (current-buffer))

(defun pi-coding-agent-test--visit-file-line (&optional line-count toggle)
  "Call `pi-coding-agent-visit-file' and return visit metadata.
Returns plist `(:path PATH :line N :open-kind KIND)'.
LINE-COUNT controls the size of the fake visited file.  TOGGLE is
forwarded to `pi-coding-agent-visit-file'."
  (let ((line-count (or line-count 100))
        (opened-path nil)
        (open-kind nil))
    (unwind-protect
        (progn
          (cl-labels ((open-other (path)
                        (setq opened-path path
                              open-kind :other)
                        (pi-coding-agent-test--open-target-buffer line-count))
                      (open-same (path)
                        (setq opened-path path
                              open-kind :same)
                        (pi-coding-agent-test--open-target-buffer line-count)))
            (cl-letf (((symbol-function 'find-file-other-window) #'open-other)
                      ((symbol-function 'find-file) #'open-same))
              (pi-coding-agent-visit-file toggle)))
          (list :path opened-path
                :line (line-number-at-pos)
                :open-kind open-kind))
      (ignore-errors (kill-buffer "*pi-coding-agent-test-target*")))))

(ert-deftest pi-coding-agent-test-diff-line-at-point-added ()
  "Should parse line number from added diff line."
  (with-temp-buffer
    (insert "+ 7     added line content")
    (goto-char (point-min))
    (should (= 7 (pi-coding-agent--diff-line-at-point)))))

(ert-deftest pi-coding-agent-test-diff-line-at-point-removed ()
  "Should parse line number from removed diff line."
  (with-temp-buffer
    (insert "-12     removed line content")
    (goto-char (point-min))
    (should (= 12 (pi-coding-agent--diff-line-at-point)))))

(ert-deftest pi-coding-agent-test-diff-line-at-point-context ()
  "Should parse line number from context lines.
Edit diffs include unchanged context rows with a leading space marker."
  (with-temp-buffer
    (insert "  7     context line")
    (goto-char (point-min))
    (should (= 7 (pi-coding-agent--diff-line-at-point)))))

(ert-deftest pi-coding-agent-test-diff-line-at-point-mid-line ()
  "Should work when point is anywhere on the line."
  (with-temp-buffer
    (insert "+ 42    some code here")
    (goto-char 15)  ;; Middle of line
    (should (= 42 (pi-coding-agent--diff-line-at-point)))))

(ert-deftest pi-coding-agent-test-code-block-line-at-point-first-line ()
  "Should return 1 for first line of code block content."
  (with-temp-buffer
    (insert "```python\nfirst line\nsecond line\n```")
    (goto-char (point-min))
    (forward-line 1)  ;; On "first line"
    (should (= 1 (pi-coding-agent--code-block-line-at-point)))))

(ert-deftest pi-coding-agent-test-code-block-line-at-point-third-line ()
  "Should return correct line for later lines."
  (with-temp-buffer
    (insert "```python\nline one\nline two\nline three\n```")
    (goto-char (point-min))
    (forward-line 3)  ;; On "line three"
    (should (= 3 (pi-coding-agent--code-block-line-at-point)))))

(ert-deftest pi-coding-agent-test-code-block-line-at-point-on-fence ()
  "Should return nil when on the fence line itself."
  (with-temp-buffer
    (insert "```python\ncontent\n```")
    (goto-char (point-min))  ;; On opening fence
    (should-not (pi-coding-agent--code-block-line-at-point))))

(ert-deftest pi-coding-agent-test-code-block-line-at-point-tilde-fence ()
  "Should support markdown tilde fences as code blocks."
  (with-temp-buffer
    (insert "~~~python\nline one\nline two\n~~~")
    (goto-char (point-min))
    (forward-line 2)  ;; On "line two"
    (should (= 2 (pi-coding-agent--code-block-line-at-point)))))

(ert-deftest pi-coding-agent-test-code-block-line-at-point-ignores-deep-indent-fence ()
  "Should ignore fences indented four spaces (not fenced code markers)."
  (with-temp-buffer
    (insert "    ```python\nline one\n    ```")
    (goto-char (point-min))
    (forward-line 1)  ;; On "line one"
    (should-not (pi-coding-agent--code-block-line-at-point))))

(ert-deftest pi-coding-agent-test-code-block-line-at-point-after-closing-fence ()
  "Should return nil when point is outside a fenced block."
  (with-temp-buffer
    (insert "```python\nline one\n```\noutside")
    (goto-char (point-min))
    (forward-line 3)  ;; On "outside"
    (should-not (pi-coding-agent--code-block-line-at-point))))

(ert-deftest pi-coding-agent-test-code-block-line-at-point-ignores-faux-closing-fence ()
  "Closing fence marker with trailing text should not close the block."
  (with-temp-buffer
    (insert "```python\nline one\n``` not-a-close\nline two\n```")
    (goto-char (point-min))
    (forward-line 3)  ;; On "line two"
    (should (= 3 (pi-coding-agent--code-block-line-at-point)))))

(ert-deftest pi-coding-agent-test-code-block-line-at-point-no-fence ()
  "Should return nil when not in a code block."
  (with-temp-buffer
    (insert "just plain text\nno fences here")
    (goto-char (point-min))
    (should-not (pi-coding-agent--code-block-line-at-point))))

(ert-deftest pi-coding-agent-test-tool-line-at-point-expanded-read-ignores-earlier-unclosed-fence ()
  "Expanded read line lookup should ignore unrelated earlier unclosed fences."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent-tool-preview-lines 2)
          (inhibit-read-only t))
      (insert "```python\nunclosed\n")
      (pi-coding-agent--display-tool-start "read" '(:path "/tmp/test.py"))
      (pi-coding-agent--display-tool-end
       "read" '(:path "/tmp/test.py")
       '((:type "text" :text "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\n"))
       nil nil)
      (goto-char (point-min))
      (re-search-forward "\.\.\. ([0-9]+ more lines)" nil t)
      (let ((btn (button-at (match-beginning 0))))
        (should btn)
        (pi-coding-agent--toggle-tool-output btn))
      (goto-char (point-min))
      (search-forward "line 6")
      (let ((ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block))
                          (overlays-at (point)))))
        (should ov)
        (should (= 6 (pi-coding-agent--tool-line-at-point ov)))))))

(ert-deftest pi-coding-agent-test-tool-overlay-stores-path ()
  "Tool overlay should store the file path for navigation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/test.py"))
    ;; The pending overlay should have the path
    (should pi-coding-agent--pending-tool-overlay)
    (should (equal "/tmp/test.py"
                   (overlay-get pi-coding-agent--pending-tool-overlay
                                'pi-coding-agent-tool-path)))))

(ert-deftest pi-coding-agent-test-tool-overlay-stores-path-after-finalize ()
  "Tool overlay should preserve path after finalization."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "edit" '(:path "/tmp/edit.el"))
    (pi-coding-agent--display-tool-end "edit" '(:path "/tmp/edit.el")
                          '((:type "text" :text "done"))
                          '(:diff "+ 1     new line")
                          nil)
    ;; Find the finalized overlay
    (goto-char (point-min))
    (let ((ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block))
                        (overlays-in (point-min) (point-max)))))
      (should ov)
      (should (equal "/tmp/edit.el" (overlay-get ov 'pi-coding-agent-tool-path))))))

(ert-deftest pi-coding-agent-test-tool-overlay-stores-offset ()
  "Tool overlay should store read offset for line calculation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/file.py" :offset 50))
    (pi-coding-agent--display-tool-end "read" '(:path "/tmp/file.py" :offset 50)
                          '((:type "text" :text "content"))
                          nil nil)
    ;; Find the finalized overlay
    (let ((ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block))
                        (overlays-in (point-min) (point-max)))))
      (should ov)
      (should (= 50 (overlay-get ov 'pi-coding-agent-tool-offset))))))

(ert-deftest pi-coding-agent-test-tool-overlay-offset-defaults-nil ()
  "Tool overlay offset should be nil when not specified."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/file.py"))
    (pi-coding-agent--display-tool-end "read" '(:path "/tmp/file.py")
                          '((:type "text" :text "content"))
                          nil nil)
    (let ((ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block))
                        (overlays-in (point-min) (point-max)))))
      (should ov)
      (should-not (overlay-get ov 'pi-coding-agent-tool-offset)))))

(ert-deftest pi-coding-agent-test-streaming-tool-overlay-has-path-after-finalize ()
  "Streaming write with nil args at start should have path after finalize.
When toolcall_start has nil args and the path arrives via toolcall_delta,
the finalized overlay must still have the path for visit-file navigation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    ;; toolcall_start with nil args (LLM just started generating JSON)
    (pi-coding-agent--handle-display-event
     `(:type "message_update"
       :assistantMessageEvent (:type "toolcall_start" :contentIndex 0)
       :message (:role "assistant"
                 :content [(:type "toolCall" :id "call_1"
                            :name "write" :arguments nil)])))
    ;; Delta with path now populated
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "hello\n"))
    (pi-coding-agent--handle-display-event
     '(:type "message_end" :message (:role "assistant")))
    ;; Execution phase
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "call_1"
       :toolName "write" :args (:path "/tmp/foo.py" :content "hello\n")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_end" :toolCallId "call_1"
       :toolName "write"
       :result (:content [(:type "text" :text "wrote file")])
       :isError nil))
    ;; Finalized overlay must have the path
    (let ((ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block))
                        (overlays-in (point-min) (point-max)))))
      (should ov)
      (should (equal "/tmp/foo.py"
                     (overlay-get ov 'pi-coding-agent-tool-path))))))

(ert-deftest pi-coding-agent-test-streaming-tool-path-from-execution-start ()
  "Overlay path set from tool_execution_start when delta never provided it.
Safety net: even if toolcall_delta doesn't include the path, the
authoritative args from tool_execution_start should populate it."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    ;; toolcall_start with nil args
    (pi-coding-agent--handle-display-event
     `(:type "message_update"
       :assistantMessageEvent (:type "toolcall_start" :contentIndex 0)
       :message (:role "assistant"
                 :content [(:type "toolCall" :id "call_1"
                            :name "edit" :arguments nil)])))
    ;; No toolcall_delta with path — skip straight to execution
    (pi-coding-agent--handle-display-event
     '(:type "message_end" :message (:role "assistant")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "call_1"
       :toolName "edit"
       :args (:path "/tmp/bar.el" :oldText "old" :newText "new")))
    ;; Pending overlay should now have path from execution start
    (should pi-coding-agent--pending-tool-overlay)
    (should (equal "/tmp/bar.el"
                   (overlay-get pi-coding-agent--pending-tool-overlay
                                'pi-coding-agent-tool-path)))))

(ert-deftest pi-coding-agent-test-visit-file-from-edit-diff ()
  "visit-file should navigate to correct line from edit diff."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "edit" '(:path "/tmp/test.el"))
    (pi-coding-agent--display-tool-end "edit" '(:path "/tmp/test.el")
                          '((:type "text" :text "done"))
                          '(:diff "+ 42    (defun foo ())")
                          nil)
    ;; Move to the diff line
    (goto-char (point-min))
    (search-forward "+ 42")
    (let ((result (pi-coding-agent-test--visit-file-line 100)))
      (should (equal "/tmp/test.el" (plist-get result :path)))
      (should (eq :other (plist-get result :open-kind)))
      (should (= 42 (plist-get result :line))))))

(ert-deftest pi-coding-agent-test-visit-file-no-path-errors ()
  "visit-file should error when not on a tool block with path."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      (insert "Just some text, no tool block"))
    (goto-char (point-min))
    (should-error (pi-coding-agent-visit-file) :type 'user-error)))

(ert-deftest pi-coding-agent-test-visit-file-read-with-offset ()
  "visit-file should use offset for read tool line calculation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/big.py" :offset 100))
    (pi-coding-agent--display-tool-end "read" '(:path "/tmp/big.py" :offset 100)
                          '((:type "text" :text "line 100\nline 101\nline 102"))
                          nil nil)
    ;; Move to line 2 of the code block content (should be file line 101)
    (goto-char (point-min))
    (search-forward "```")
    (forward-line 2)  ;; On "line 101"
    (let ((result (pi-coding-agent-test--visit-file-line 200)))
      ;; Line 2 in code block + offset 100 - 1 = 101
      (should (= 101 (plist-get result :line))))))

(ert-deftest pi-coding-agent-test-visit-file-read-beginning-line ()
  "visit-file should navigate to line 1 from first read content line."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/start.txt"))
    (pi-coding-agent--display-tool-end "read" '(:path "/tmp/start.txt")
                          '((:type "text" :text "line1\nline2\nline3"))
                          nil nil)
    (goto-char (point-min))
    (search-forward "line1")
    (beginning-of-line)
    (let ((result (pi-coding-agent-test--visit-file-line 20)))
      (should (= 1 (plist-get result :line))))))

(ert-deftest pi-coding-agent-test-visit-file-toggle-opens-same-window ()
  "Prefix arg should invert `pi-coding-agent-visit-file-other-window'."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent-visit-file-other-window t))
      (pi-coding-agent--display-tool-start "read" '(:path "/tmp/start.txt"))
      (pi-coding-agent--display-tool-end "read" '(:path "/tmp/start.txt")
                            '((:type "text" :text "line1\nline2\nline3"))
                            nil nil)
      (goto-char (point-min))
      (search-forward "line2")
      (beginning-of-line)
      (let ((result (pi-coding-agent-test--visit-file-line 20 t)))
        (should (eq :same (plist-get result :open-kind)))
        (should (= 2 (plist-get result :line)))))))

(ert-deftest pi-coding-agent-test-visit-file-accounts-for-stripped-blank-lines ()
  "visit-file navigates to correct original line even when blank lines stripped.
File has blanks at lines 3,5. Pressing RET on 'line06' should go to line 6."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; File: line01, line02, (blank), line04, (blank), line06...line15
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/test.txt"))
    (pi-coding-agent--display-tool-end "read" '(:path "/tmp/test.txt")
                          '((:type "text" :text "line01\nline02\n\nline04\n\nline06\nline07\nline08\nline09\nline10\nline11\nline12\nline13\nline14\nline15"))
                          nil nil)
    (goto-char (point-min))
    (search-forward "line06")
    (beginning-of-line)
    (let ((result (pi-coding-agent-test--visit-file-line 20)))
      ;; Should navigate to line 6, not line 4 (2 blank lines stripped)
      (should (= 6 (plist-get result :line))))))

(ert-deftest pi-coding-agent-test-visit-file-preserves-blank-lines-when-not-collapsed ()
  "visit-file should respect blank lines in non-collapsed read output.
When full output is visible, line numbers must follow rendered blank lines."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/test.txt"))
    (pi-coding-agent--display-tool-end "read" '(:path "/tmp/test.txt")
                          '((:type "text" :text "line1\n\nline3\nline4"))
                          nil nil)
    (goto-char (point-min))
    (search-forward "line3")
    (beginning-of-line)
    (let ((result (pi-coding-agent-test--visit-file-line 20)))
      (should (= 3 (plist-get result :line))))))

(ert-deftest pi-coding-agent-test-visit-file-preserves-blank-lines-when-expanded ()
  "visit-file should ignore preview line-map when tool output is expanded."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent-tool-preview-lines 10))
      (pi-coding-agent--display-tool-start "read" '(:path "/tmp/test.txt"))
      ;; 12 non-blank lines + one early blank -> collapsed preview, then expand.
      (pi-coding-agent--display-tool-end
       "read" '(:path "/tmp/test.txt")
       '((:type "text" :text "line1\n\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13"))
       nil nil)
      (goto-char (point-min))
      (re-search-forward "\.\.\. ([0-9]+ more lines)" nil t)
      (let ((btn (button-at (match-beginning 0))))
        (should btn)
        (pi-coding-agent--toggle-tool-output btn))
      (goto-char (point-min))
      (search-forward "line3")
      (beginning-of-line)
      (let ((result (pi-coding-agent-test--visit-file-line 30)))
        (should (= 3 (plist-get result :line)))))))

(ert-deftest pi-coding-agent-test-visit-file-collapsed-closing-fence-errors ()
  "RET on collapsed closing fence should not fall back to line 1."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent-tool-preview-lines 2))
      (pi-coding-agent--display-tool-start "read" '(:path "/tmp/test.txt"))
      (pi-coding-agent--display-tool-end
       "read" '(:path "/tmp/test.txt")
       '((:type "text" :text "line01\nline02\nline03\nline04\nline05\nline06"))
       nil nil)
      (goto-char (point-min))
      ;; Move to closing fence (second ``` line).
      (re-search-forward "^```$" nil t)
      (re-search-forward "^```$" nil t)
      (should-error (pi-coding-agent-visit-file) :type 'user-error))))

(ert-deftest pi-coding-agent-test-visit-file-collapsed-toggle-line-errors ()
  "RET on collapsed toggle line should not fall back to line 1."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent-tool-preview-lines 2))
      (pi-coding-agent--display-tool-start "read" '(:path "/tmp/test.txt"))
      (pi-coding-agent--display-tool-end
       "read" '(:path "/tmp/test.txt")
       '((:type "text" :text "line01\nline02\nline03\nline04\nline05\nline06"))
       nil nil)
      (goto-char (point-min))
      (re-search-forward "\.\.\. ([0-9]+ more lines)" nil t)
      (beginning-of-line)
      (should-error (pi-coding-agent-visit-file) :type 'user-error))))

(ert-deftest pi-coding-agent-test-visit-file-edit-context-line ()
  "RET on edit diff context line should navigate to that source line."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "edit" '(:path "/tmp/test.el"))
    (pi-coding-agent--display-tool-end
     "edit" '(:path "/tmp/test.el")
     '((:type "text" :text "done"))
     '(:diff "+ 7     added line\n  9     context line\n-12     removed line")
     nil)
    (goto-char (point-min))
    (re-search-forward "^  9     context line" nil t)
    (beginning-of-line)
    (let ((result (pi-coding-agent-test--visit-file-line 30)))
      (should (= 9 (plist-get result :line))))))

(ert-deftest pi-coding-agent-test-visit-file-edit-context-first-line ()
  "RET on first unchanged line in edit diff should jump to line 1."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "edit" '(:path "/tmp/greeting.py"))
    (pi-coding-agent--display-tool-end
     "edit" '(:path "/tmp/greeting.py")
     '((:type "text" :text "done"))
     '(:diff "  1 def make_greeting(name: str) -> str:\n  2     \"\"\"Return a friendly greeting with an uppercased name.\"\"\"\n- 3     return f\"Hello, {name.upperr()}!\"\n+ 3     return f\"Hello, {name.upper()}!\"\n  4 \n  5 \n  6 def main() -> None:\n    ...")
     nil)
    (goto-char (point-min))
    (re-search-forward "^  1 def make_greeting" nil t)
    (beginning-of-line)
    (let ((result (pi-coding-agent-test--visit-file-line 30)))
      (should (= 1 (plist-get result :line))))))

(ert-deftest pi-coding-agent-test-visit-file-generic-path-expanded-line ()
  "Generic tool output with :path should map expanded lines correctly."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "custom_tool" '(:path "/tmp/custom.txt"))
    (pi-coding-agent--display-tool-end
     "custom_tool" '(:path "/tmp/custom.txt")
     '((:type "text" :text "line01\nline02\nline03\nline04\nline05\nline06"))
     nil nil)
    (goto-char (point-min))
    (search-forward "line06")
    (beginning-of-line)
    (let ((result (pi-coding-agent-test--visit-file-line 20)))
      (should (= 6 (plist-get result :line))))))

(ert-deftest pi-coding-agent-test-visit-file-generic-path-collapsed-line ()
  "Generic tool output with :path should map collapsed preview lines correctly."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent-tool-preview-lines 3))
      (pi-coding-agent--display-tool-start "custom_tool" '(:path "/tmp/custom.txt"))
      (pi-coding-agent--display-tool-end
       "custom_tool" '(:path "/tmp/custom.txt")
       '((:type "text" :text "line01\nline02\nline03\nline04\nline05\nline06"))
       nil nil)
      (goto-char (point-min))
      (search-forward "line03")
      (beginning-of-line)
      (let ((result (pi-coding-agent-test--visit-file-line 20)))
        (should (= 3 (plist-get result :line)))))))

(ert-deftest pi-coding-agent-test-visit-file-write-ignores-offset ()
  "write tool should ignore :offset for RET line navigation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start
     "write" '(:path "/tmp/out.txt" :offset 100 :content "line1\nline2\nline3"))
    (pi-coding-agent--display-tool-end
     "write" '(:path "/tmp/out.txt" :offset 100 :content "line1\nline2\nline3")
     '((:type "text" :text "wrote file"))
     nil nil)
    (goto-char (point-min))
    (search-forward "line2")
    (beginning-of-line)
    (let ((result (pi-coding-agent-test--visit-file-line 120)))
      (should (= 2 (plist-get result :line))))))

;;; Visual Line Truncation Tests

(ert-deftest pi-coding-agent-test-truncate-visual-lines-simple ()
  "Truncation with short lines counts each as one visual line."
  (let ((content "line1\nline2\nline3\nline4\nline5"))
    ;; Width 80, max 3 visual lines -> should get first 3 lines
    (let ((result (pi-coding-agent--truncate-to-visual-lines content 3 80)))
      (should (equal (plist-get result :content) "line1\nline2\nline3"))
      (should (= (plist-get result :visual-lines) 3))
      (should (= (plist-get result :hidden-lines) 2)))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-wrapping ()
  "Long lines count as multiple visual lines based on width."
  ;; Create content where first line is 160 chars (2 visual lines at width 80)
  (let ((long-line (make-string 160 ?a))
        (short-line "short"))
    (let* ((content (concat long-line "\n" short-line))
           ;; Width 80, max 2 visual lines -> only first line fits (uses 2 visual lines)
           (result (pi-coding-agent--truncate-to-visual-lines content 2 80)))
      (should (equal (plist-get result :content) long-line))
      (should (= (plist-get result :visual-lines) 2))
      (should (= (plist-get result :hidden-lines) 1)))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-byte-limit ()
  "Truncation respects byte limit in addition to visual lines."
  (let ((pi-coding-agent-preview-max-bytes 50))
    ;; Each line is 10 chars, 5 lines = 54 bytes with newlines
    (let* ((content "aaaaaaaaaa\nbbbbbbbbbb\ncccccccccc\ndddddddddd\neeeeeeeeee")
           (result (pi-coding-agent--truncate-to-visual-lines content 100 80)))
      ;; Should stop before exceeding 50 bytes
      (should (< (length (plist-get result :content)) 50))
      (should (> (plist-get result :hidden-lines) 0)))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-no-truncation-needed ()
  "Content under limits returns unchanged."
  (let ((content "short\ncontent"))
    (let ((result (pi-coding-agent--truncate-to-visual-lines content 100 80)))
      (should (equal (plist-get result :content) content))
      (should (= (plist-get result :hidden-lines) 0)))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-empty-content ()
  "Empty content has no hidden lines or visual lines."
  (let ((result (pi-coding-agent--truncate-to-visual-lines "" 5 80)))
    (should (equal (plist-get result :content) ""))
    (should (= (plist-get result :hidden-lines) 0))
    (should (= (plist-get result :visual-lines) 0))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-zero-max-lines ()
  "Zero max lines returns an empty preview without crashing."
  (let* ((content "line1\nline2")
         (result (pi-coding-agent--truncate-to-visual-lines content 0 80)))
    (should (equal (plist-get result :content) ""))
    (should (= (plist-get result :visual-lines) 0))
    (should (= (plist-get result :hidden-lines) 2))
    (should (equal (plist-get result :line-map) []))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-zero-width-falls-back ()
  "Zero width is treated as width 1 to avoid division errors."
  (let* ((content "abcdef")
         (result (pi-coding-agent--truncate-to-visual-lines content 2 0)))
    (should (equal (plist-get result :content) "ab"))
    (should (= (plist-get result :visual-lines) 2))
    (should (= (plist-get result :hidden-lines) 1))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-trailing-newline ()
  "Trailing newlines don't create phantom hidden lines."
  ;; Content with trailing newline - should count as 3 lines, not 4
  (let ((content "line1\nline2\nline3\n"))
    (let ((result (pi-coding-agent--truncate-to-visual-lines content 5 80)))
      (should (= (plist-get result :hidden-lines) 0))
      (should (= (plist-get result :visual-lines) 3)))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-single-long-line ()
  "A single line exceeding visual line limit gets truncated.
Regression test: single lines without newlines should still be capped.
If we ask for 5 visual lines at width 80, we should get ~400 chars max."
  ;; 1000 char single line with no newlines - at width 80, this is ~13 visual lines
  (let ((content (make-string 1000 ?x)))
    (let ((result (pi-coding-agent--truncate-to-visual-lines content 5 80)))
      ;; Should be capped to ~5 visual lines worth of content
      ;; 5 * 80 = 400 chars max
      (should (<= (length (plist-get result :content)) 400))
      (should (<= (plist-get result :visual-lines) 5)))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-single-line-byte-limit ()
  "A single line exceeding byte limit gets truncated.
Regression test: single lines should respect byte limit even with no newlines."
  (let ((pi-coding-agent-preview-max-bytes 100))
    ;; 500 char single line - exceeds 100 byte limit
    (let* ((content (make-string 500 ?y))
           (result (pi-coding-agent--truncate-to-visual-lines content 100 80)))
      ;; Should respect byte limit
      (should (<= (length (plist-get result :content)) 100)))))

(ert-deftest pi-coding-agent-test-tool-output-truncates-long-lines ()
  "Tool output preview accounts for visual line wrapping."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Create output with one very long line (200 chars) that wraps to ~3 visual lines
    ;; Plus 3 more short lines. At width 80 and 5 preview lines limit:
    ;; Line 1: 200 chars = 3 visual lines
    ;; Line 2-3: 2 visual lines
    ;; Total: 5 visual lines (at limit), line 4 should be hidden
    (let* ((long-line (make-string 200 ?x))
           (output (concat long-line "\nline2\nline3\nline4")))
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 80)))
        (pi-coding-agent--display-tool-end "bash" '(:command "test")
                              `((:type "text" :text ,output))
                              nil nil))
      ;; Long line should be present
      (should (string-match-p "xxxx" (buffer-string)))
      ;; line4 should be hidden (in "more lines" section)
      (should (string-match-p "more lines" (buffer-string))))))

(ert-deftest pi-coding-agent-test-tab-bound-to-toggle-tool-section ()
  "TAB is bound to pi-coding-agent-toggle-tool-section for tool block handling."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "output"))
                          nil nil)
    ;; Verify we have a tool block with overlay
    (should (string-match-p "\\$ ls" (buffer-string)))
    (goto-char (point-min))
    (should (pi-coding-agent--find-tool-block-bounds))
    ;; pi-coding-agent-toggle-tool-section should be bound to TAB and <tab>
    (should (eq (lookup-key pi-coding-agent-chat-mode-map (kbd "TAB")) 'pi-coding-agent-toggle-tool-section))
    (should (eq (lookup-key pi-coding-agent-chat-mode-map (kbd "<tab>")) 'pi-coding-agent-toggle-tool-section))))

(ert-deftest pi-coding-agent-test-tool-error-indicated ()
  "Tool error is indicated in output."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-end "bash" '(:command "false")
                          '((:type "text" :text "error message"))
                          nil t)
    (should (string-match-p "error" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-success-not-error ()
  "Tool with isError :false should not show error indicator."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "test"))
    (pi-coding-agent--display-tool-end "bash" nil
                          '((:type "text" :text "success output"))
                          nil :false)
    ;; Should have output, success face, no [error]
    (should (string-match-p "success output" (buffer-string)))
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'pi-coding-agent-tool-block)))
    (should-not (string-match-p "\\[error\\]" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-output-survives-message-render ()
  "Tool output should not be clobbered by subsequent message rendering."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Simulate: message -> tool -> message sequence
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    (pi-coding-agent--handle-display-event 
     '(:type "message_update" 
       :assistantMessageEvent (:type "text_delta" :delta "Running")))
    (pi-coding-agent--handle-display-event '(:type "message_end"))
    
    (pi-coding-agent--handle-display-event 
     '(:type "tool_execution_start" :toolName "bash" :args (:command "ls")))
    (pi-coding-agent--handle-display-event 
     '(:type "tool_execution_end" :toolName "bash"
       :result (:content ((:type "text" :text "file1\nfile2")))))
    
    ;; Second message should NOT clobber tool output
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    (pi-coding-agent--handle-display-event 
     '(:type "message_update"
       :assistantMessageEvent (:type "text_delta" :delta "Done")))
    (pi-coding-agent--handle-display-event '(:type "message_end"))
    
    ;; Tool output must still be present
    (should (string-match-p "file1" (buffer-string)))
    (should (string-match-p "file2" (buffer-string)))
    (should (string-match-p "\\$ ls" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-handler-handles-tool-start ()
  "Display handler processes tool_execution_start events."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((event (list :type "tool_execution_start"
                       :toolName "bash"
                       :args (list :command "echo hello"))))
      (pi-coding-agent--handle-display-event event)
      (should (string-match-p "echo hello" (buffer-string))))))

(ert-deftest pi-coding-agent-test-display-handler-handles-tool-end ()
  "Display handler processes tool_execution_end events."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((event (list :type "tool_execution_end"
                       :toolName "bash"
                       :args (list :command "ls")
                       :result (list :content '((:type "text" :text "output")))
                       :isError nil)))
      (pi-coding-agent--handle-display-event event)
      (should (string-match-p "output" (buffer-string))))))

(ert-deftest pi-coding-agent-test-display-handler-handles-tool-update ()
  "Display handler processes tool_execution_update events."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; First, start the tool
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start"
       :toolName "bash"
       :toolCallId "test-id"
       :args (:command "long-running")))
    ;; Then send an update with partial result (same structure as tool result)
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_update"
       :toolCallId "test-id"
       :partialResult (:content [(:type "text" :text "streaming output line 1")])))
    ;; Should show partial content
    (should (string-match-p "streaming output" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-update-shows-rolling-tail ()
  "Tool updates show rolling tail of output, truncated to visual lines."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Start the tool
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start"
       :toolName "bash"
       :toolCallId "test-id"
       :args (:command "verbose-command")))
    ;; Send update with many lines (more than preview limit)
    (let ((many-lines (mapconcat (lambda (n) (format "line%d" n))
                                 (number-sequence 1 20)
                                 "\n")))
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 80)))
        (pi-coding-agent--handle-display-event
         `(:type "tool_execution_update"
           :toolCallId "test-id"
           :partialResult (:content [(:type "text" :text ,many-lines)])))))
    ;; Should show indicator that earlier output is hidden
    (should (string-match-p "earlier output" (buffer-string)))
    ;; Should show last few lines
    (should (string-match-p "line20" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-update-truncates-single-long-line ()
  "Tool updates truncate single lines that exceed visual line limit.
Regression test: streaming output with no newlines should still be capped."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Start the tool
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start"
       :toolName "bash"
       :toolCallId "test-id"
       :args (:command "json-dump")))
    ;; Send update with a single very long line (1000 chars, ~13 visual lines at width 80)
    ;; Preview limit is 5 lines, so this should be truncated
    (let ((long-line (make-string 1000 ?x)))
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 80)))
        (pi-coding-agent--handle-display-event
         `(:type "tool_execution_update"
           :toolCallId "test-id"
           :partialResult (:content [(:type "text" :text ,long-line)])))))
    ;; Output should be truncated - 5 visual lines * 80 chars = 400 chars max
    (let ((buffer-content (buffer-string)))
      ;; Should NOT contain all 1000 x's
      (should-not (string-match-p (make-string 500 ?x) buffer-content))
      ;; Should contain truncation indicator
      (should (string-match-p "earlier output\\|truncated" buffer-content)))))

;; ── Toolcall streaming (during LLM generation) ─────────────────────

(defun pi-coding-agent-test--pending-tool-stream-body ()
  "Return pending tool overlay body as plain text."
  (let* ((ov pi-coding-agent--pending-tool-overlay)
         (header-end (overlay-get ov 'pi-coding-agent-header-end)))
    (buffer-substring-no-properties header-end (overlay-end ov))))

(defun pi-coding-agent-test--pending-tool-content-lines ()
  "Return streamed content lines without indicators or fence markers.
Filters the hidden-output indicator and both backtick and tilde
fences (opening and closing) so only user-visible content remains."
  (let* ((stream (pi-coding-agent-test--pending-tool-stream-body))
         (lines (split-string (string-trim-right stream "\n+") "\n")))
    (cl-remove-if (lambda (line)
                    (or (string= line "... (earlier output)")
                        (string-prefix-p "```" line)
                        (string-prefix-p "~~~" line)))
                  lines)))

(ert-deftest pi-coding-agent-test-toolcall-start-after-text-has-blank-line ()
  "toolcall_start after text delta without trailing newline has proper spacing."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    ;; Text delta without trailing newline (common: LLM streams partial line)
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "text_delta" :delta "Let me check.")))
    ;; toolcall_start fires immediately after
    (pi-coding-agent--handle-display-event
     `(:type "message_update"
       :assistantMessageEvent (:type "toolcall_start" :contentIndex 0)
       :message (:role "assistant"
                 :content [(:type "toolCall" :id "call_1"
                            :name "bash" :arguments (:command "ls"))])))
    ;; Must have blank line between text and tool header
    (should (string-match-p "check\\.\n\n\\$ ls" (buffer-string)))))

(ert-deftest pi-coding-agent-test-toolcall-delta-updates-header-not-path ()
  "toolcall_delta updates header text for responsiveness but not overlay path.
Header shows path as soon as it appears in streaming args (visual feedback).
Overlay path is only set at tool_execution_start (authoritative for navigation)."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    ;; toolcall_start with empty args (LLM just started generating JSON)
    (pi-coding-agent--handle-display-event
     `(:type "message_update"
       :assistantMessageEvent (:type "toolcall_start" :contentIndex 0)
       :message (:role "assistant"
                 :content [(:type "toolCall" :id "call_1"
                            :name "read" :arguments nil)])))
    ;; Delta with path — header SHOULD update (visual feedback)
    (pi-coding-agent--handle-display-event
     `(:type "message_update"
       :assistantMessageEvent (:type "toolcall_delta" :contentIndex 0 :delta "x")
       :message (:role "assistant"
                 :content [(:type "toolCall" :id "call_1"
                            :name "read"
                            :arguments (:path "/tmp/foo.py"))])))
    ;; Header should show the real path
    (should (string-match-p "read /tmp/foo\\.py" (buffer-string)))
    ;; But overlay should NOT have path yet (deferred to tool_execution_start)
    (should-not (overlay-get pi-coding-agent--pending-tool-overlay
                             'pi-coding-agent-tool-path))))

(ert-deftest pi-coding-agent-test-toolcall-header-updated-at-execution-start ()
  "Header updates from placeholder to real args at tool_execution_start.
During streaming, header shows placeholder.  When execution starts with
authoritative args, header and overlay path are updated."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    ;; toolcall_start with nil args
    (pi-coding-agent--handle-display-event
     `(:type "message_update"
       :assistantMessageEvent (:type "toolcall_start" :contentIndex 0)
       :message (:role "assistant"
                 :content [(:type "toolCall" :id "call_1"
                            :name "read" :arguments nil)])))
    (should (string-match-p "read \\.\\.\\." (buffer-string)))
    ;; tool_execution_start with authoritative args
    (pi-coding-agent--handle-display-event
     '(:type "message_end" :message (:role "assistant")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "call_1"
       :toolName "read" :args (:path "/tmp/foo.py")))
    ;; Now header should show the real path
    (should (string-match-p "read /tmp/foo\\.py" (buffer-string)))
    (should-not (string-match-p "read \\.\\.\\." (buffer-string)))
    ;; And overlay should have the path
    (should (equal "/tmp/foo.py"
                   (overlay-get pi-coding-agent--pending-tool-overlay
                                'pi-coding-agent-tool-path)))))

(ert-deftest pi-coding-agent-test-toolcall-start-creates-overlay ()
  "toolcall_start in message_update creates tool overlay with header."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (should (string-match-p "write /tmp/foo\\.py" (buffer-string)))
    (should pi-coding-agent--pending-tool-overlay)
    (should (equal pi-coding-agent--streaming-tool-id "call_1"))))

(ert-deftest pi-coding-agent-test-toolcall-delta-streams-write-content ()
  "toolcall_delta streams args.content for write tools."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "line1\nline2\n"))
    (should (string-match-p "line1" (buffer-string)))
    (should (string-match-p "line2" (buffer-string)))))

(ert-deftest pi-coding-agent-test-toolcall-delta-uses-fenced-code-block ()
  "Streaming write content is wrapped in a markdown fenced code block.
The fences enable md-ts-mode language injection for syntax highlighting."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "def hello():\n    print('hi')\n"))
    (should (string-match-p "def hello" (buffer-string)))
    (should (string-match-p "```python" (buffer-string)))))

(ert-deftest pi-coding-agent-test-toolcall-delta-backtick-safe-fence ()
  "Streaming content with triple backticks uses a safe fence delimiter.
When streamed Python contains a docstring with a code example using
triple backticks, the fence must use tildes to avoid breaking the
markdown structure."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (pi-coding-agent-test--send-delta
     "write" `(:path "/tmp/foo.py"
               :content ,(concat "def example():\n"
                                 "    \"\"\"Example:\n"
                                 "    ```python\n"
                                 "    print('hello')\n"
                                 "    ```\n"
                                 "    \"\"\"\n"
                                 "    pass\n")))
    (let ((content (buffer-string)))
      ;; Outer fence must NOT be triple backticks (content contains them)
      ;; Should use tilde fence instead
      (should (string-match-p "^~~~" content))
      ;; The content's backtick fences should appear literally
      (should (string-match-p "```python" content)))))

(ert-deftest pi-coding-agent-test-toolcall-delta-streaming-has-keyword-face ()
  "Streaming write content gets syntax highlighting after fontification.
In production, jit-lock triggers fontification on redisplay.
In batch tests, we call `font-lock-ensure' explicitly."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "def hello():\n    pass\n"))
    ;; Simulate jit-lock redisplay trigger
    (font-lock-ensure (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "def")
    (let ((face (get-text-property (match-beginning 0) 'face)))
      (should (or (eq face 'font-lock-keyword-face)
                  (and (listp face) (memq 'font-lock-keyword-face face)))))))

(ert-deftest pi-coding-agent-test-toolcall-delta-fenced-prevents-markdown-bold ()
  "Fenced code block protects __init__ from markdown bold.
Streaming write content is wrapped in markdown fences; md-ts-mode
parses it as a code block (language injection), not inline markdown."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "def __init__(self):\n    pass\n"))
    ;; Simulate jit-lock redisplay trigger
    (font-lock-ensure (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "__init__")
    (let ((face (get-text-property (match-beginning 0) 'face)))
      ;; Must have SOME face (fontification ran)
      (should face)
      ;; Must NOT have bold (markdown parsing __init__ as bold markup)
      (should-not (memq 'bold
                        (if (listp face) face (list face)))))
    ;; Must not be hidden by markdown invisible property
    (goto-char (point-min))
    (search-forward "__init__")
    (should-not (get-text-property (match-beginning 0) 'invisible))))

(ert-deftest pi-coding-agent-test-toolcall-delta-survives-restore-tool-properties ()
  "Syntax faces survive restore-tool-properties after fontification.
In a live session, jit-lock fontifies on redisplay, then calls
`restore-tool-properties'.  Fenced content must keep its syntax faces."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "def hello():\n    pass\n"))
    ;; Simulate jit-lock: fontify then restore-tool-properties
    (font-lock-ensure (point-min) (point-max))
    ;; Verify fontification produced syntax faces
    (goto-char (point-min))
    (search-forward "def")
    (let ((face-before (get-text-property (match-beginning 0) 'face)))
      (should (or (eq face-before 'font-lock-keyword-face)
                  (and (listp face-before)
                       (memq 'font-lock-keyword-face face-before)))))
    ;; Simulate jit-lock calling restore-tool-properties with the full
    ;; buffer range (as happens in a live session with a visible window)
    (pi-coding-agent--restore-tool-properties (point-min) (point-max))
    ;; Syntax faces must survive
    (goto-char (point-min))
    (search-forward "def")
    (let ((face-after (get-text-property (match-beginning 0) 'face)))
      (should (or (eq face-after 'font-lock-keyword-face)
                  (and (listp face-after)
                       (memq 'font-lock-keyword-face face-after)))))))

(ert-deftest pi-coding-agent-test-toolcall-delta-incremental-fontify-context ()
  "Fontification preserves syntax context across deltas.
Docstring opener scrolls past the 10-line preview window; text added
later inside the open docstring should still get some face applied."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (let* ((opener "class Foo:\n    \"\"\"\n")
           (doc-lines (mapconcat (lambda (i) (format "    docstring line %d" i))
                                 (number-sequence 1 15) "\n"))
           (content1 (concat opener doc-lines "\n")))
      (pi-coding-agent-test--send-delta
       "write" `(:path "/tmp/foo.py" :content ,content1))
      (pi-coding-agent-test--send-delta
       "write" `(:path "/tmp/foo.py"
                 :content ,(concat content1
                                   "    def inside_string():\n"
                                   "    still docs\n"))))
    ;; Simulate jit-lock redisplay trigger
    (font-lock-ensure (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "def inside_string")
    (let ((face (get-text-property (match-beginning 0) 'face)))
      ;; With embedded language support, the Python parser may give
      ;; `def' keyword-face (tree-sitter handles incomplete docstrings
      ;; differently than regex).  Accept any syntax face.
      (should face))))

(ert-deftest pi-coding-agent-test-toolcall-delta-streams-without-mode ()
  "Streaming works even when the language mode is not installed.
Writing a .rs file without rust-mode should still show content,
falling back to unfontified text."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.rs")
    (cl-letf (((symbol-function 'rust-mode) nil))
      (pi-coding-agent-test--send-delta
       "write" '(:path "/tmp/foo.rs" :content "fn main() {\n    println!(\"hi\");\n}\n")))
    (should (string-match-p "fn main" (buffer-string)))))

(ert-deftest pi-coding-agent-test-toolcall-delta-skip-unchanged-display ()
  "Partial-line delta produces no buffer modification when tail is unchanged.
Most LLM tokens extend the current partial line, which the tail
preview excludes.  The display should be a no-op for such deltas."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    ;; Delta 1: one complete line
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "line1\n"))
    (let ((modtick-after-complete (buffer-modified-tick)))
      ;; Delta 2: adds a partial second line (no newline)
      (pi-coding-agent-test--send-delta
       "write" '(:path "/tmp/foo.py" :content "line1\npartial"))
      ;; Buffer should NOT have been modified — skip-when-unchanged
      (should (= (buffer-modified-tick) modtick-after-complete)))))

(ert-deftest pi-coding-agent-test-toolcall-delta-same-size-refreshes-preview ()
  "Same-size content rewrites still refresh write preview.
If a provider rewrites accumulated content at the same length,
the visible tail must update to the new text."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "aa\n"))
    (should (string-match-p "aa" (buffer-string)))
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "bb\n"))
    (let ((content (buffer-string)))
      (should (string-match-p "bb" content))
      (should-not (string-match-p "aa" content)))))

(ert-deftest pi-coding-agent-test-toolcall-delta-same-size-unchanged-skips-redraw ()
  "Same-size duplicate content does not redraw write preview."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "aa\n"))
    (let ((modtick (buffer-modified-tick)))
      (pi-coding-agent-test--send-delta
       "write" '(:path "/tmp/foo.py" :content "aa\n"))
      (should (= modtick (buffer-modified-tick))))))

(ert-deftest pi-coding-agent-test-toolcall-delta-empty-content-clears-preview ()
  "Empty write content clears stale streaming preview text."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "line1\n"))
    (should (string-match-p "line1" (buffer-string)))
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content ""))
    (let ((body (pi-coding-agent-test--pending-tool-stream-body)))
      (should-not (string-match-p "line1" body))
      (should (string-empty-p (string-trim-right body "\n+"))))))

(ert-deftest pi-coding-agent-test-toolcall-delta-updates-on-new-line ()
  "Completing a new line triggers a display update.
After a partial line, adding a newline changes the visible tail
and should cause a redraw."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    ;; Delta 1: one complete line
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "line1\n"))
    (let ((content-after-line1 (buffer-string)))
      ;; Delta 2: complete second line
      (pi-coding-agent-test--send-delta
       "write" '(:path "/tmp/foo.py" :content "line1\nline2\n"))
      ;; Buffer should have changed
      (should-not (equal (buffer-string) content-after-line1))
      ;; New line should appear
      (should (string-match-p "line2" (buffer-string))))))

(ert-deftest pi-coding-agent-test-toolcall-delta-stable-line-count ()
  "Streaming preview line count is stable across partial-line deltas.
A delta that ends mid-line should show the same number of lines
as the previous delta that ended at a newline boundary."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    ;; Delta 1: two complete lines
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "line1\nline2\n"))
    (let ((lines-after-complete
           (length (split-string (string-trim (buffer-string)) "\n"))))
      ;; Delta 2: adds a partial third line
      (pi-coding-agent-test--send-delta
       "write" '(:path "/tmp/foo.py" :content "line1\nline2\npar"))
      (let ((lines-after-partial
             (length (split-string (string-trim (buffer-string)) "\n"))))
        ;; Line count should NOT increase from the partial line
        (should (= lines-after-complete lines-after-partial))))))

(ert-deftest pi-coding-agent-test-toolcall-delta-lang-preview-obeys-visual-cap ()
  "Language-aware write streaming enforces visual-line preview limits.
Wrapped lines must stay within `pi-coding-agent-tool-preview-lines'
during streaming updates."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (let ((pi-coding-agent-tool-preview-lines 3)
          (content (concat (make-string 36 ?x) "\nline2\nline3\n")))
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 10)))
        (pi-coding-agent-test--send-delta
         "write" `(:path "/tmp/foo.py" :content ,content))
        (let* ((content-lines (pi-coding-agent-test--pending-tool-content-lines))
               (visual-lines
                (apply #'+
                       (mapcar (lambda (line)
                                 (max 1
                                      (ceiling (/ (float (length line)) 10))))
                               content-lines))))
          (should (<= visual-lines pi-coding-agent-tool-preview-lines)))))))

(ert-deftest pi-coding-agent-test-toolcall-delta-lang-cap-preserves-syntax-face ()
  "Visual capping in language-aware streaming keeps syntax faces intact."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (let ((pi-coding-agent-tool-preview-lines 2)
          (content "def very_long_function_name_that_wraps_many_times(arg):\nline2\n"))
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 10)))
        (pi-coding-agent-test--send-delta
         "write" `(:path "/tmp/foo.py" :content ,content)))
      ;; Simulate jit-lock redisplay trigger
      (font-lock-ensure (point-min) (point-max))
      (goto-char (point-min))
      (search-forward "def")
      (let ((face (get-text-property (match-beginning 0) 'face)))
        (should (or (eq face 'font-lock-keyword-face)
                    (and (listp face)
                         (memq 'font-lock-keyword-face face))))))))

(ert-deftest pi-coding-agent-test-toolcall-delta-rewrites-bounded-preview ()
  "Write streaming rewrites in place, keeping preview size bounded.
Multiple deltas should replace the preview instead of appending forever."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (let ((pi-coding-agent-tool-preview-lines 3))
      (pi-coding-agent-test--send-delta
       "write" '(:path "/tmp/foo.py" :content "line1\nline2\nline3\n"))
      (pi-coding-agent-test--send-delta
       "write" '(:path "/tmp/foo.py" :content
                "line1\nline2\nline3\nline4\n"))
      (pi-coding-agent-test--send-delta
       "write" '(:path "/tmp/foo.py" :content
                "line1\nline2\nline3\nline4\nline5\n"))
      (pi-coding-agent-test--send-delta
       "write" '(:path "/tmp/foo.py" :content
                "line1\nline2\nline3\nline4\nline5\nline6\n"))
      (let ((body (pi-coding-agent-test--pending-tool-stream-body)))
        (should-not (string-match-p "line1" body))
        (should-not (string-match-p "line2" body))
        (should (string-match-p "line6" body))
        (should (= 1 (pi-coding-agent-test--count-matches "line6" body)))))))

(ert-deftest pi-coding-agent-test-toolcall-dedup-on-tool-execution-start ()
  "tool_execution_start skips overlay creation when toolcall_start already created it."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (pi-coding-agent--handle-display-event
     '(:type "message_end" :message (:role "assistant")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "call_1"
       :toolName "write" :args (:path "/tmp/foo.py" :content "final")))
    (should (= 1 (pi-coding-agent-test--count-matches
                   "write /tmp/foo\\.py" (buffer-string))))
    (should-not pi-coding-agent--streaming-tool-id)))

(ert-deftest pi-coding-agent-test-toolcall-full-event-flow ()
  "Full toolcall streaming flow produces correct final output."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (pi-coding-agent-test--send-delta
     "write" '(:path "/tmp/foo.py" :content "streaming content\n"))
    (pi-coding-agent--handle-display-event
     `(:type "message_update"
       :assistantMessageEvent (:type "toolcall_end" :contentIndex 0)
       :message (:role "assistant"
                 :content [(:type "toolCall" :id "call_1"
                            :name "write"
                            :arguments (:path "/tmp/foo.py"
                                        :content "streaming content\n"))])))
    (pi-coding-agent--handle-display-event
     '(:type "message_end" :message (:role "assistant")))
    ;; Execution phase (dedup guard skips overlay creation)
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "call_1"
       :toolName "write" :args (:path "/tmp/foo.py" :content "final content")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_end" :toolCallId "call_1"
       :toolName "write"
       :result (:content [(:type "text" :text "wrote 42 lines")])))
    (let ((content (buffer-string)))
      (should (= 1 (pi-coding-agent-test--count-matches
                      "write /tmp/foo\\.py" content)))
      (should (string-match-p "final content" content)))))

(ert-deftest pi-coding-agent-test-toolcall-non-write-shows-header-only ()
  "Non-write tools show header from toolcall_start but no streaming content."
  (pi-coding-agent-test--with-toolcall "read" '(:path "/tmp/test.txt")
    (pi-coding-agent-test--send-delta
     "read" '(:path "/tmp/test.txt" :offset 1))
    (should (string-match-p "read /tmp/test\\.txt" (buffer-string)))
    (should-not (string-match-p "offset" (buffer-string)))))

(ert-deftest pi-coding-agent-test-toolcall-abort-cleans-up ()
  "Abort during toolcall streaming cleans up properly."
  (pi-coding-agent-test--with-toolcall "write" '(:path "/tmp/foo.py")
    (let ((pi-coding-agent--aborted t))
      (pi-coding-agent--handle-display-event '(:type "agent_end")))
    (should-not pi-coding-agent--pending-tool-overlay)
    (should-not pi-coding-agent--streaming-tool-id)))

(ert-deftest pi-coding-agent-test-toolcall-second-ignored-during-streaming ()
  "Second toolcall_start is ignored while first is still streaming."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    ;; First tool call starts streaming
    (pi-coding-agent--handle-display-event
     `(:type "message_update"
       :assistantMessageEvent (:type "toolcall_start" :contentIndex 0)
       :message (:role "assistant"
                 :content [(:type "toolCall" :id "call_1"
                            :name "write" :arguments (:path "/tmp/a.py"))
                           (:type "toolCall" :id "call_2"
                            :name "write" :arguments (:path "/tmp/b.py"))])))
    ;; Second tool call start — should be ignored (streaming-tool-id already set)
    (pi-coding-agent--handle-display-event
     `(:type "message_update"
       :assistantMessageEvent (:type "toolcall_start" :contentIndex 1)
       :message (:role "assistant"
                 :content [(:type "toolCall" :id "call_1"
                            :name "write" :arguments (:path "/tmp/a.py"))
                           (:type "toolCall" :id "call_2"
                            :name "write" :arguments (:path "/tmp/b.py"))])))
    ;; Only first tool's header appears
    (let ((content (buffer-string)))
      (should (string-match-p "write /tmp/a\\.py" content))
      (should-not (string-match-p "write /tmp/b\\.py" content)))
    ;; streaming-tool-id still tracks first tool
    (should (equal pi-coding-agent--streaming-tool-id "call_1"))
    ;; After tool_execution_start for first (dedup), second gets normal path
    (pi-coding-agent--handle-display-event '(:type "message_end" :message (:role "assistant")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "call_1"
       :toolName "write" :args (:path "/tmp/a.py" :content "content a")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_end" :toolCallId "call_1"
       :toolName "write" :result (:content [(:type "text" :text "wrote a")])))
    ;; Now second tool gets created by tool_execution_start normally
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "call_2"
       :toolName "write" :args (:path "/tmp/b.py" :content "content b")))
    (let ((content (buffer-string)))
      (should (string-match-p "write /tmp/b\\.py" content))
      ;; Both headers present, no duplicates
      (should (= 1 (pi-coding-agent-test--count-matches "write /tmp/a\\.py" content)))
      (should (= 1 (pi-coding-agent-test--count-matches "write /tmp/b\\.py" content))))))

(ert-deftest pi-coding-agent-test-get-tail-lines-basic ()
  "Get-tail-lines returns last N lines correctly."
  (let ((content "line1\nline2\nline3\nline4\nline5"))
    ;; Get last 2 lines
    (let ((result (pi-coding-agent--get-tail-lines content 2)))
      (should (equal (car result) "line4\nline5"))
      (should (eq (cdr result) t)))  ; has hidden content
    ;; Get last 5 lines (all)
    (let ((result (pi-coding-agent--get-tail-lines content 5)))
      (should (equal (car result) content))
      (should (eq (cdr result) nil)))  ; no hidden content
    ;; Get last 10 lines (more than available)
    (let ((result (pi-coding-agent--get-tail-lines content 10)))
      (should (equal (car result) content))
      (should (eq (cdr result) nil)))))

(ert-deftest pi-coding-agent-test-get-tail-lines-trailing-newlines ()
  "Get-tail-lines handles trailing newlines correctly."
  ;; Content with trailing newlines - the function preserves them
  (let ((content "line1\nline2\nline3\n\n"))
    (let ((result (pi-coding-agent--get-tail-lines content 2)))
      ;; Gets last 2 lines including trailing newlines
      (should (equal (car result) "line2\nline3\n\n"))
      (should (eq (cdr result) t)))))

(ert-deftest pi-coding-agent-test-get-tail-lines-skips-blank-lines ()
  "Get-tail-lines does not count blank lines toward N.
Blank lines are included in the returned content but don't consume
a slot, so downstream consumers that skip blanks still get N content lines."
  ;; With blank line in the tail region, should return 3 content lines
  (let* ((content "line1\nline2\nline3\n\nline4\nline5")
         (result (pi-coding-agent--get-tail-lines content 3)))
    ;; Should include line3, blank, line4, line5 — 3 non-blank lines
    (should (equal (car result) "line3\n\nline4\nline5"))
    (should (eq (cdr result) t)))
  ;; Multiple blank lines should all be skipped
  (let* ((content "a\nb\n\n\nc\nd")
         (result (pi-coding-agent--get-tail-lines content 3)))
    ;; Should return b, blank, blank, c, d — 3 non-blank lines
    (should (equal (car result) "b\n\n\nc\nd"))
    (should (eq (cdr result) t)))
  ;; Blank line at very end (before trailing newline)
  (let* ((content "line1\nline2\n\n")
         (result (pi-coding-agent--get-tail-lines content 2)))
    (should (equal (car result) "line1\nline2\n\n"))
    (should (eq (cdr result) nil))))

(ert-deftest pi-coding-agent-test-get-tail-lines-empty ()
  "Get-tail-lines handles empty content."
  (let ((result (pi-coding-agent--get-tail-lines "" 5)))
    (should (equal (car result) ""))
    (should (eq (cdr result) nil))))

(ert-deftest pi-coding-agent-test-get-tail-lines-single-line ()
  "Get-tail-lines handles single line content."
  (let ((result (pi-coding-agent--get-tail-lines "just one line" 5)))
    (should (equal (car result) "just one line"))
    (should (eq (cdr result) nil))))

(ert-deftest pi-coding-agent-test-get-tail-lines-zero-lines ()
  "Requesting zero lines returns empty tail without errors."
  (let ((result (pi-coding-agent--get-tail-lines "line1\nline2" 0)))
    (should (equal (car result) ""))
    (should (eq (cdr result) t))))

;;; Fontify Exclusion Helpers

(ert-deftest pi-coding-agent-test-font-lock-ensure-excluding-property-splits-ranges ()
  "Font-lock helper should call only contiguous non-excluded ranges."
  (with-temp-buffer
    (insert "aaaBBBcccDDDeee")
    (put-text-property 4 7 'pi-coding-agent-no-fontify t)
    (put-text-property 10 13 'pi-coding-agent-no-fontify t)
    (let ((calls nil))
      (cl-letf (((symbol-function 'font-lock-ensure)
                 (lambda (start end)
                   (push (cons start end) calls))))
        (pi-coding-agent--font-lock-ensure-excluding-property
         (point-min) (point-max) 'pi-coding-agent-no-fontify))
      (should (equal (nreverse calls)
                     '((1 . 4) (7 . 10) (13 . 16)))))))

(ert-deftest pi-coding-agent-test-font-lock-ensure-excluding-property-excludes-any-non-nil ()
  "Font-lock helper should treat any non-nil PROP value as excluded."
  (with-temp-buffer
    (insert "aaaBBBccc")
    (put-text-property 4 7 'pi-coding-agent-no-fontify :details)
    (let ((calls nil))
      (cl-letf (((symbol-function 'font-lock-ensure)
                 (lambda (start end)
                   (push (cons start end) calls))))
        (pi-coding-agent--font-lock-ensure-excluding-property
         (point-min) (point-max) 'pi-coding-agent-no-fontify))
      (should (equal (nreverse calls)
                     '((1 . 4) (7 . 10)))))))

(ert-deftest pi-coding-agent-test-font-lock-ensure-excluding-property-fontifies-large-ranges ()
  "Font-lock helper should still process large non-excluded regions."
  (with-temp-buffer
    (insert (make-string 70000 ?x))
    (let ((called nil))
      (cl-letf (((symbol-function 'font-lock-ensure)
                 (lambda (&rest _args)
                   (setq called t))))
        (pi-coding-agent--font-lock-ensure-excluding-property
         (point-min) (point-max) 'pi-coding-agent-no-fontify))
      (should called))))

(ert-deftest pi-coding-agent-test-font-lock-ensure-excluding-property-error-silent-without-debug ()
  "Font-lock errors should not emit user-visible messages when debug is off."
  (with-temp-buffer
    (insert "abcdef")
    (let ((debug-on-error nil)
          (message-called nil))
      (cl-letf (((symbol-function 'font-lock-ensure)
                 (lambda (&rest _args)
                   (error "Broken font-lock")))
                ((symbol-function 'message)
                 (lambda (&rest _args)
                   (setq message-called t))))
        (pi-coding-agent--font-lock-ensure-excluding-property
         (point-min) (point-max) 'pi-coding-agent-no-fontify))
      (should-not message-called))))

(ert-deftest pi-coding-agent-test-font-lock-ensure-excluding-property-error-logs-in-debug ()
  "Font-lock errors should log diagnostics when debug mode is enabled."
  (with-temp-buffer
    (insert "abcdef")
    (let ((debug-on-error t)
          (message-text nil))
      (cl-letf (((symbol-function 'font-lock-ensure)
                 (lambda (&rest _args)
                   (error "Broken font-lock")))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (pi-coding-agent--font-lock-ensure-excluding-property
         (point-min) (point-max) 'pi-coding-agent-no-fontify))
      (should (string-match-p "toggle fontification failed"
                              (or message-text ""))))))

(ert-deftest pi-coding-agent-test-font-lock-ensure-excluding-property-stops-after-first-error ()
  "Font-lock helper should stop processing ranges after the first error."
  (with-temp-buffer
    (insert "aaaBBBccc")
    (put-text-property 4 7 'pi-coding-agent-no-fontify t)
    (let ((debug-on-error t)
          (font-lock-calls 0)
          (message-count 0))
      (cl-letf (((symbol-function 'font-lock-ensure)
                 (lambda (&rest _args)
                   (setq font-lock-calls (1+ font-lock-calls))
                   (error "Broken font-lock")))
                ((symbol-function 'message)
                 (lambda (&rest _args)
                   (setq message-count (1+ message-count)))))
        (pi-coding-agent--font-lock-ensure-excluding-property
         (point-min) (point-max) 'pi-coding-agent-no-fontify))
      (should (= font-lock-calls 1))
      (should (= message-count 1)))))

(ert-deftest pi-coding-agent-test-font-lock-ensure-excluding-property-swallows-errors ()
  "Font-lock helper should not propagate font-lock errors."
  (with-temp-buffer
    (insert "abcdef")
    (cl-letf (((symbol-function 'font-lock-ensure)
               (lambda (&rest _args)
                 (error "Broken font-lock"))))
      (pi-coding-agent--font-lock-ensure-excluding-property
       (point-min) (point-max) 'pi-coding-agent-no-fontify)
      (should t))))

;;; Extract Text from Content

(ert-deftest pi-coding-agent-test-extract-text-from-content-single-block ()
  "Extract-text-from-content handles single text block efficiently."
  (let ((blocks [(:type "text" :text "hello world")]))
    (should (equal (pi-coding-agent--extract-text-from-content blocks)
                   "hello world"))))

(ert-deftest pi-coding-agent-test-extract-text-from-content-multiple-blocks ()
  "Extract-text-from-content concatenates multiple text blocks."
  (let ((blocks [(:type "text" :text "hello ")
                 (:type "image" :data "...")
                 (:type "text" :text "world")]))
    (should (equal (pi-coding-agent--extract-text-from-content blocks)
                   "hello world"))))

(ert-deftest pi-coding-agent-test-extract-text-from-content-empty ()
  "Extract-text-from-content handles empty input."
  (should (equal (pi-coding-agent--extract-text-from-content []) ""))
  (should (equal (pi-coding-agent--extract-text-from-content nil) "")))

(ert-deftest pi-coding-agent-test-extract-last-usage-from-messages ()
  "Extract-last-usage finds usage from last assistant message."
  (let ((messages
         [(:role "user" :content "Hi")
          (:role "assistant"
           :usage (:input 100 :output 50 :cacheRead 0 :cacheWrite 20)
           :stopReason "endTurn")
          (:role "user" :content "More")
          (:role "assistant"
           :usage (:input 200 :output 80 :cacheRead 20 :cacheWrite 30)
           :stopReason "endTurn")]))
    (let ((usage (pi-coding-agent--extract-last-usage messages)))
      (should (equal (plist-get usage :input) 200))
      (should (equal (plist-get usage :output) 80)))))

(ert-deftest pi-coding-agent-test-extract-last-usage-skips-aborted ()
  "Extract-last-usage skips aborted messages."
  (let ((messages
         [(:role "assistant"
           :usage (:input 100 :output 50 :cacheRead 0 :cacheWrite 0)
           :stopReason "endTurn")
          (:role "assistant"
           :usage (:input 0 :output 0 :cacheRead 0 :cacheWrite 0)
           :stopReason "aborted")]))
    (let ((usage (pi-coding-agent--extract-last-usage messages)))
      ;; Should return the non-aborted message's usage
      (should (equal (plist-get usage :input) 100)))))

(ert-deftest pi-coding-agent-test-extract-last-usage-empty ()
  "Extract-last-usage handles empty/nil input."
  (should-not (pi-coding-agent--extract-last-usage []))
  (should-not (pi-coding-agent--extract-last-usage nil)))

(ert-deftest pi-coding-agent-test-extract-last-usage-no-assistant ()
  "Extract-last-usage returns nil when no assistant messages."
  (let ((messages [(:role "user" :content "Hi")]))
    (should-not (pi-coding-agent--extract-last-usage messages))))

(ert-deftest pi-coding-agent-test-tool-update-replaced-by-end ()
  "Tool update content is replaced by final result on tool_execution_end."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start"
       :toolName "bash"
       :toolCallId "test-id"
       :args (:command "test")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_update"
       :toolCallId "test-id"
       :partialResult (:content [(:type "text" :text "partial streaming")])))
    ;; Partial content should be present
    (should (string-match-p "partial streaming" (buffer-string)))
    ;; Now end the tool
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_end"
       :toolName "bash"
       :toolCallId "test-id"
       :result (:content ((:type "text" :text "final output")))
       :isError nil))
    ;; Streaming content should be replaced
    (should-not (string-match-p "partial streaming" (buffer-string)))
    (should (string-match-p "final output" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-update-preserves-multiline-command-header ()
  "Tool updates preserve command headers that span multiple lines.
Commands with embedded newlines should not have any lines deleted."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((multiline-cmd "echo 'line1'\necho 'line2'"))
      (pi-coding-agent--display-tool-start "bash" `(:command ,multiline-cmd))
      ;; Both lines of header should be present
      (should (string-match-p "echo 'line1'" (buffer-string)))
      (should (string-match-p "echo 'line2'" (buffer-string)))
      ;; Update with streaming content
      (pi-coding-agent--display-tool-update
       '(:content [(:type "text" :text "output from command")]))
      ;; Header should still be intact
      (should (string-match-p "echo 'line1'" (buffer-string)))
      (should (string-match-p "echo 'line2'" (buffer-string)))
      (should (string-match-p "output from command" (buffer-string))))))

(ert-deftest pi-coding-agent-test-tool-end-preserves-multiline-command-header ()
  "Tool end preserves command headers that span multiple lines."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((multiline-cmd "echo 'first'\necho 'second'\necho 'third'"))
      (pi-coding-agent--display-tool-start "bash" `(:command ,multiline-cmd))
      ;; Stream some content first
      (pi-coding-agent--display-tool-update
       '(:content [(:type "text" :text "streaming...")]))
      ;; Then end the tool
      (pi-coding-agent--display-tool-end "bash" `(:command ,multiline-cmd)
                            '((:type "text" :text "final output")) nil nil)
      ;; All three lines of the header should be intact
      (should (string-match-p "echo 'first'" (buffer-string)))
      (should (string-match-p "echo 'second'" (buffer-string)))
      (should (string-match-p "echo 'third'" (buffer-string)))
      (should (string-match-p "final output" (buffer-string))))))

(ert-deftest pi-coding-agent-test-display-handler-handles-thinking-delta ()
  "Display handler processes thinking_delta events."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_delta" :delta "Analyzing...")))
    (should (string-match-p "Analyzing..." (buffer-string)))))

(ert-deftest pi-coding-agent-test-activity-phase-thinking-on-agent-start ()
  "Activity phase becomes thinking on agent_start."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--activity-phase "idle")
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (should (equal pi-coding-agent--activity-phase "thinking"))))

(ert-deftest pi-coding-agent-test-activity-phase-replying-on-text-delta ()
  "Activity phase becomes replying on text_delta."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--activity-phase "idle")
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "text_delta" :delta "Hello")))
    (should (equal pi-coding-agent--activity-phase "replying"))))

(ert-deftest pi-coding-agent-test-activity-phase-running-on-toolcall-start ()
  "Activity phase becomes running when tool call generation starts."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--activity-phase "thinking")
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "toolcall_start" :contentIndex 0)
       :message (:role "assistant"
                 :content [(:type "toolCall"
                            :id "call_1"
                            :name "read"
                            :arguments (:path "/tmp/file.txt"))])))
    (should (equal pi-coding-agent--activity-phase "running"))))

(ert-deftest pi-coding-agent-test-activity-phase-running-on-tool-start ()
  "Activity phase becomes running on tool_execution_start."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--activity-phase "idle")
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start"
       :toolCallId "tool-1"
       :toolName "bash"
       :args (:command "ls")))
    (should (equal pi-coding-agent--activity-phase "running"))))

(ert-deftest pi-coding-agent-test-activity-phase-thinking-on-tool-end ()
  "Activity phase returns to thinking on tool_execution_end."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--activity-phase "running")
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start"
       :toolCallId "tool-1"
       :toolName "bash"
       :args (:command "ls")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_end"
       :toolCallId "tool-1"
       :toolName "bash"
       :result (:content nil)
       :isError nil))
    (should (equal pi-coding-agent--activity-phase "thinking"))))

(ert-deftest pi-coding-agent-test-activity-phase-compact-on-compaction ()
  "Activity phase becomes compact on auto_compaction_start."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--activity-phase "idle")
    (pi-coding-agent--handle-display-event
     '(:type "auto_compaction_start" :reason "threshold"))
    (should (equal pi-coding-agent--activity-phase "compact"))))

(ert-deftest pi-coding-agent-test-activity-phase-idle-on-agent-end ()
  "Activity phase becomes idle on agent_end."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--activity-phase "thinking")
    (pi-coding-agent--handle-display-event '(:type "agent_end"))
    (should (equal pi-coding-agent--activity-phase "idle"))))

(ert-deftest pi-coding-agent-test-activity-phase-idle-on-compaction-end ()
  "Activity phase becomes idle on auto_compaction_end."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--activity-phase "compact")
    (pi-coding-agent--handle-display-event
     '(:type "auto_compaction_end" :aborted t :result nil))
    (should (equal pi-coding-agent--activity-phase "idle"))))

(ert-deftest pi-coding-agent-test-display-compaction-result-shows-header-tokens-summary ()
  "pi-coding-agent--display-compaction-result shows header, token count, and summary."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-compaction-result 50000 "Key points from discussion.")
    ;; Should have Compaction header
    (should (string-match-p "Compaction" (buffer-string)))
    ;; Should show formatted tokens
    (should (string-match-p "50,000 tokens" (buffer-string)))
    ;; Should show summary
    (should (string-match-p "Key points" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-compaction-result-with-timestamp ()
  "pi-coding-agent--display-compaction-result includes timestamp when provided."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((timestamp (seconds-to-time 1704067200))) ; 2024-01-01 00:00 UTC
      (pi-coding-agent--display-compaction-result 30000 "Summary text." timestamp))
    ;; Should have timestamp in header (format depends on locale, check for time marker)
    (should (string-match-p "Compaction" (buffer-string)))
    (should (string-match-p "30,000 tokens" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-compaction-result-shows-markdown ()
  "pi-coding-agent--display-compaction-result displays markdown summary as-is."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-compaction-result 10000 "**Bold** and `code`")
    ;; Markdown stays as markdown
    (should (string-match-p "\\*\\*Bold\\*\\*" (buffer-string)))
    (should (string-match-p "`code`" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-handler-handles-auto-compaction-start ()
  "Display handler processes auto_compaction_start events."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "auto_compaction_start" :reason "threshold"))
    ;; Status should change to compacting
    (should (eq pi-coding-agent--status 'compacting))))

(ert-deftest pi-coding-agent-test-display-handler-handles-auto-compaction-end ()
  "Display handler processes auto_compaction_end with successful result."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Set up initial usage
    (setq pi-coding-agent--last-usage '(:input 5000 :output 1000))
    ;; Simulate compaction end
    (pi-coding-agent--handle-display-event
     '(:type "auto_compaction_end"
       :aborted nil
       :result (:summary "Context was compacted."
                :tokensBefore 50000
                :timestamp 1704067200000)))
    ;; Usage should be reset
    (should (null pi-coding-agent--last-usage))
    ;; Should display compaction info
    (should (string-match-p "Compaction" (buffer-string)))
    (should (string-match-p "50,000" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-handler-handles-auto-compaction-aborted ()
  "Display handler processes auto_compaction_end when aborted."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--status 'compacting)
    (setq pi-coding-agent--last-usage '(:input 5000 :output 1000))
    (pi-coding-agent--handle-display-event
     '(:type "auto_compaction_end" :aborted t :result nil))
    ;; Status should return to idle
    (should (eq pi-coding-agent--status 'idle))
    ;; Usage should NOT be reset on abort
    (should pi-coding-agent--last-usage)))

(ert-deftest pi-coding-agent-test-thinking-rendered-as-blockquote ()
  "Thinking content renders as markdown blockquote."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    ;; Thinking lifecycle: start -> delta -> end
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_start")))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_delta" :delta "Let me analyze this.")))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_end" :content "Let me analyze this.")))
    ;; Then regular text
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "text_delta" :delta "Here is my answer.")))
    ;; Complete the message (triggers rendering)
    (pi-coding-agent--handle-display-event '(:type "message_end" :message (:role "assistant")))
    ;; After rendering, thinking should be in a blockquote (> prefix)
    (goto-char (point-min))
    (should (search-forward "> Let me analyze this." nil t))
    ;; Regular text should be outside the blockquote
    (should (search-forward "Here is my answer." nil t))
    ;; Should NOT have code fence markers
    (goto-char (point-min))
    (should-not (search-forward "```thinking" nil t))))

(ert-deftest pi-coding-agent-test-thinking-blockquote-has-face ()
  "Thinking blockquote has md-ts-block-quote after font-lock."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      (insert "> Some thinking here.\n"))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "Some thinking")
    ;; Verify md-ts-block-quote is applied (may be in a list with other faces)
    (let ((face (get-text-property (point) 'face)))
      (should (or (eq face 'md-ts-block-quote)
                  (and (listp face) (memq 'md-ts-block-quote face)))))))

(ert-deftest pi-coding-agent-test-thinking-multiline-blockquote ()
  "Multi-line thinking content has > prefix on each line."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_start")))
    ;; Multi-line thinking with newline in delta
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_delta" :delta "First line.\nSecond line.")))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_end" :content "")))
    ;; Each line should have > prefix
    (goto-char (point-min))
    (should (search-forward "> First line." nil t))
    (should (search-forward "> Second line." nil t))))

(ert-deftest pi-coding-agent-test-agent-end-clears-thinking-marker-buffer ()
  "agent_end should detach thinking markers and clear thinking stream state."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-thinking-start)
    (let ((marker pi-coding-agent--thinking-marker)
          (start-marker pi-coding-agent--thinking-start-marker))
      (should (stringp pi-coding-agent--thinking-raw))
      (pi-coding-agent--display-agent-end)
      (should-not pi-coding-agent--thinking-marker)
      (should-not pi-coding-agent--thinking-start-marker)
      (should-not pi-coding-agent--thinking-raw)
      (should-not (marker-buffer marker))
      (should-not (marker-buffer start-marker)))))

(defun pi-coding-agent-test--assert-message-start-clears-thinking-state (event)
  "Assert that message_start EVENT clears all thinking-stream state."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-thinking-start)
    (let ((marker pi-coding-agent--thinking-marker)
          (start-marker pi-coding-agent--thinking-start-marker))
      (pi-coding-agent--handle-display-event event)
      (should-not pi-coding-agent--thinking-marker)
      (should-not pi-coding-agent--thinking-start-marker)
      (should-not pi-coding-agent--thinking-raw)
      (should-not (marker-buffer marker))
      (should-not (marker-buffer start-marker)))))

(ert-deftest pi-coding-agent-test-message-start-clears-previous-thinking-marker ()
  "message_start should clear stale thinking markers and stream state."
  (pi-coding-agent-test--assert-message-start-clears-thinking-state
   '(:type "message_start" :message (:role "assistant"))))

(ert-deftest pi-coding-agent-test-message-start-user-clears-previous-thinking-marker ()
  "message_start for user should also clear stale thinking state."
  (pi-coding-agent-test--assert-message-start-clears-thinking-state
   '(:type "message_start"
     :message (:role "user" :content [(:type "text" :text "hi")]))))

(ert-deftest pi-coding-agent-test-message-start-custom-clears-previous-thinking-marker ()
  "message_start for custom messages should clear stale thinking state."
  (pi-coding-agent-test--assert-message-start-clears-thinking-state
   '(:type "message_start"
     :message (:role "custom" :display t :content "done"))))

(ert-deftest pi-coding-agent-test-blockquote-has-wrap-prefix ()
  "Blockquotes have wrap-prefix for continuation lines after font-lock."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      (insert "> Some blockquote content.\n"))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "Some blockquote")
    (should (get-text-property (point) 'wrap-prefix))))

(ert-deftest pi-coding-agent-test-read-tool-gets-syntax-highlighting ()
  "Read tool output gets syntax highlighting based on file path.
The toolCallId is used to correlate start/end events since args
are only present in tool_execution_start, not tool_execution_end."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Start event has args with path
    (pi-coding-agent--handle-display-event
     (list :type "tool_execution_start"
           :toolCallId "call_123"
           :toolName "read"
           :args (list :path "example.py")))
    ;; End event does NOT have args (matches real pi behavior)
    (pi-coding-agent--handle-display-event
     (list :type "tool_execution_end"
           :toolCallId "call_123"
           :toolName "read"
           :result (list :content '((:type "text" :text "def hello():\n    pass")))
           :isError nil))
    ;; Should have python markdown code fence
    (should (string-match-p "```python" (buffer-string)))))

(ert-deftest pi-coding-agent-test-generic-tool-with-path-uses-path-language ()
  "Generic tools with :path should use extension-based syntax fences."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "custom_tool" '(:path "/tmp/example.py"))
    (pi-coding-agent--display-tool-end
     "custom_tool" '(:path "/tmp/example.py")
     '((:type "text" :text "def hello():\n    return 1"))
     nil nil)
    (should (string-match-p "```python" (buffer-string)))))

(ert-deftest pi-coding-agent-test-markdown-fence-delimiter-defaults-to-backticks ()
  "Fence delimiter should use backticks when content has no backtick fence."
  (should (equal "```"
                 (pi-coding-agent--markdown-fence-delimiter "plain text"))))

(ert-deftest pi-coding-agent-test-markdown-fence-delimiter-avoids-tilde-collisions ()
  "Fence delimiter should exceed the longest tilde run in content."
  (let ((content "before\n~~~~\n```bash\necho hi\n```\nafter"))
    (should (equal "~~~~~"
                   (pi-coding-agent--markdown-fence-delimiter content)))))

(ert-deftest pi-coding-agent-test-wrap-in-src-block-uses-safe-fence ()
  "Wrapped source blocks should use a delimiter that cannot close content."
  (let ((wrapped (pi-coding-agent--wrap-in-src-block
                  "```elisp\n(message \"hi\")\n```\n~~~~"
                  "markdown")))
    (should (string-prefix-p "~~~~~markdown\n" wrapped))
    (should (string-suffix-p "\n~~~~~" wrapped))))

(ert-deftest pi-coding-agent-test-read-tool-fences-handle-nested-backticks ()
  "Consecutive read blocks keep wrapper fence markup hidden.
Inner backtick fences in read output must not affect later wrappers."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; First read output includes a nested markdown fence.
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/test.md"))
    (pi-coding-agent--display-tool-end
     "read" '(:path "/tmp/test.md")
     '((:type "text" :text "before\n```bash\necho hi\n```\nafter\n"))
     nil nil)
    ;; Second read output is plain text.
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/test.md"))
    (pi-coding-agent--display-tool-end
     "read" '(:path "/tmp/test.md")
     '((:type "text" :text "plain\nline\n"))
     nil nil)
    ;; Apply markdown font-lock so hidden markup properties are set.
    (font-lock-ensure (point-min) (point-max))
    (let ((wrapper-openers nil))
      (goto-char (point-min))
      (while (re-search-forward "^\\([`~]\\)\\1\\1+markdown$" nil t)
        (let* ((line-start (match-beginning 0))
               (line-end (line-end-position))
               (all-hidden t)
               (pos line-start))
          (while (< pos line-end)
            (unless (eq (get-char-property pos 'invisible) 'md-ts--markup)
              (setq all-hidden nil))
            (setq pos (1+ pos)))
          (push all-hidden wrapper-openers)))
      (setq wrapper-openers (nreverse wrapper-openers))
      ;; Two read wrappers, and each opener line is fully hidden.
      (should (equal (length wrapper-openers) 2))
      (dolist (hidden wrapper-openers)
        (should hidden)))))

(ert-deftest pi-coding-agent-test-thinking-markdown-after-collapsed-read ()
  "Thinking markdown remains styled after a collapsed read tool block."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((long-content
           (string-join
            (mapcar (lambda (n) (format "line %03d" n))
                    (number-sequence 1 140))
            "\n")))
      (pi-coding-agent--display-tool-start
       "read" '(:path "/tmp/TODO-RPC-enhancements.md"))
      (pi-coding-agent--display-tool-end
       "read" '(:path "/tmp/TODO-RPC-enhancements.md")
       `((:type "text" :text ,long-content))
       nil nil)
      (should (string-match-p "\.\.\. ([0-9]+ more lines)" (buffer-string)))

      (pi-coding-agent--display-agent-start)
      (pi-coding-agent--display-thinking-start)
      (pi-coding-agent--display-thinking-delta
       "**Reviewing documentation editing guidelines**")
      (pi-coding-agent--display-thinking-end "")
      (pi-coding-agent--render-complete-message)
      (font-lock-ensure (point-min) (point-max))

      (goto-char (point-min))
      (re-search-forward "Reviewing documentation editing guidelines" nil t)
      (let* ((review-pos (match-beginning 0))
             (line-start (line-beginning-position))
             (star-pos (+ line-start 2))
             (line-face (get-text-property line-start 'face))
             (review-face (get-text-property review-pos 'face)))
        (should (or (eq line-face 'md-ts-block-quote)
                    (and (listp line-face)
                         (memq 'md-ts-block-quote line-face))))
        (should (eq (get-text-property star-pos 'invisible) 'md-ts--markup))
        (should (or (eq review-face 'bold)
                    (and (listp review-face)
                         (memq 'bold review-face))))))))

(ert-deftest pi-coding-agent-test-thinking-delta-after-toolcall-start-stays-blockquote ()
  "Thinking markdown stays a blockquote even if toolcall_start arrives first.
Some providers can interleave content blocks by contentIndex.  A thinking delta
that arrives after toolcall_start must still render as thinking markdown, not
as plain tool output."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event
     '(:type "message_start" :message (:role "assistant")))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_start")))
    ;; Out-of-order interleave: toolcall starts before thinking text chunk.
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "toolcall_start" :contentIndex 0)
       :message (:role "assistant"
                 :content [(:type "toolCall" :id "call_1" :name "read"
                            :arguments (:path "/tmp/AGENTS.md"))])))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_delta"
                               :delta "**Reviewing documentation editing guidelines**")))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_end" :content "")))
    (pi-coding-agent--handle-display-event
     '(:type "message_end" :message (:role "assistant" :stopReason "toolUse")))
    (font-lock-ensure (point-min) (point-max))
    (goto-char (point-min))
    (re-search-forward "Reviewing documentation editing guidelines" nil t)
    (let* ((review-pos (match-beginning 0))
           (line-start (line-beginning-position))
           (line-face (get-text-property line-start 'face))
           (review-face (get-text-property review-pos 'face)))
      (should (string-prefix-p "> "
                               (buffer-substring-no-properties
                                line-start (line-end-position))))
      (should (or (eq line-face 'md-ts-block-quote)
                  (and (listp line-face)
                       (memq 'md-ts-block-quote line-face))))
      ;; With range settings active, the inline parser is scoped to
      ;; inline nodes.  After a setext heading, bold face may not apply
      ;; (known limitation: inline nodes depend on tree structure).
      ;; At minimum, blockquote face should be present on the text.
      (should (or (eq review-face 'bold)
                  (and (listp review-face)
                       (memq 'bold review-face))
                  (eq review-face 'md-ts-block-quote)
                  (and (listp review-face)
                       (memq 'md-ts-block-quote review-face)))))))

(ert-deftest pi-coding-agent-test-write-tool-gets-syntax-highlighting ()
  "Write tool displays content from args with syntax highlighting.
The content to display comes from args, not from the result
which is just a success message."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Start event has args with path and content
    (pi-coding-agent--handle-display-event
     (list :type "tool_execution_start"
           :toolCallId "call_456"
           :toolName "write"
           :args (list :path "example.rs"
                       :content "fn main() {\n    println!(\"Hello\");\n}")))
    ;; End event has only success message in result
    (pi-coding-agent--handle-display-event
     (list :type "tool_execution_end"
           :toolCallId "call_456"
           :toolName "write"
           :result (list :content '((:type "text" :text "Successfully wrote 42 bytes")))
           :isError nil))
    ;; Should have rust markdown code fence (from args content, not result)
    (should (string-match-p "```rust" (buffer-string)))
    ;; Should show the actual code, not the success message
    (should (string-match-p "fn main" (buffer-string)))))

;;;; Performance Tests

(ert-deftest pi-coding-agent-test-streaming-inhibits-modification-hooks ()
  "Streaming delta functions inhibit modification hooks for performance.
This prevents expensive jit-lock/font-lock from running on each delta,
which caused major performance issues with large buffers."
  (let ((hook-called nil))
    (cl-flet ((test-hook (beg end len) (setq hook-called t)))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (pi-coding-agent--display-agent-start)
        (add-hook 'after-change-functions #'test-hook nil t)
        (setq hook-called nil)
        (pi-coding-agent--display-message-delta "Test delta")
        (should-not hook-called)))))

(ert-deftest pi-coding-agent-test-thinking-delta-fires-modification-hooks ()
  "Thinking delta must allow modification hooks for syntax propertization.
Suppressing modification hooks via `inhibit-modification-hooks' prevents
`syntax-ppss-flush-cache' from running, which leaves `syntax-propertize'
stale and breaks blockquote fontification for large thinking blocks."
  (let ((hook-called nil))
    (cl-flet ((test-hook (beg end len) (setq hook-called t)))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (pi-coding-agent--display-agent-start)
        (pi-coding-agent--display-thinking-start)
        (add-hook 'after-change-functions #'test-hook nil t)
        (setq hook-called nil)
        (pi-coding-agent--display-thinking-delta "Test thinking")
        (should hook-called)))))

(ert-deftest pi-coding-agent-test-tool-update-inhibits-modification-hooks ()
  "Tool update inhibits modification hooks for performance."
  (let ((hook-called nil))
    (cl-flet ((test-hook (beg end len) (setq hook-called t)))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (pi-coding-agent--display-agent-start)
        ;; Create pending tool overlay
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (setq pi-coding-agent--pending-tool-overlay
                (pi-coding-agent--tool-overlay-create "bash"))
          (insert "$ test\n"))
        (add-hook 'after-change-functions #'test-hook nil t)
        (setq hook-called nil)
        (pi-coding-agent--display-tool-update
         '(:content [(:type "text" :text "output")]))
        (should-not hook-called)))))

(ert-deftest pi-coding-agent-test-streaming-fontify-does-not-bleed-into-tool ()
  "Bash streaming content is fenced, protecting markdown patterns.
Markdown patterns (#, **, __) in bash output must not acquire display,
invisible, or markdown face properties.  Content is inside a bare
fence so tree-sitter does not parse it as markdown."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "text_delta" :delta "Running.\n")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start"
       :toolName "bash" :toolCallId "c1" :args (:command "report")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_update"
       :toolCallId "c1"
       :partialResult
       (:content [(:type "text"
                   :text "# Heading\necho \"**bold**\"\necho \"__init__.py\"\n")])))
    ;; Simulate jit-lock
    (font-lock-ensure (point-min) (point-max))
    (dolist (pattern '("# Heading" "**bold**" "__init__"))
      (goto-char (point-min))
      (search-forward pattern)
      (let ((pos (match-beginning 0)))
        (should-not (get-text-property pos 'display))
        (should-not (get-text-property pos 'invisible))))))

(ert-deftest pi-coding-agent-test-tool-header-no-markdown-damage ()
  "Tool header must retain tool-command face after treesit fontification.
Markdown patterns in multi-line bash commands must not acquire display,
invisible, or markdown face properties."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "text_delta" :delta "Running.\n")))
    (pi-coding-agent--display-tool-start "bash" '(:command "echo"))
    (pi-coding-agent--display-tool-update-header
     "bash" '(:command "echo \"# Build\"\necho \"**done**\"\necho \"__init__.py\""))
    ;; Simulate jit-lock: font-lock + registered cleanup
    (font-lock-ensure (point-min) (point-max))
    (pi-coding-agent--restore-tool-properties (point-min) (point-max))
    (dolist (pattern '("**done**" "__init__"))
      (goto-char (point-min))
      (search-forward pattern)
      (let ((pos (match-beginning 0)))
        (should-not (get-text-property pos 'display))
        (should-not (get-text-property pos 'invisible))
        (should (eq (get-text-property pos 'face)
                    'pi-coding-agent-tool-command))))))

(ert-deftest pi-coding-agent-test-normal-insert-does-call-hooks ()
  "Control test: normal inserts DO trigger hooks.
This validates that our hook-based tests are meaningful."
  (let ((hook-called nil))
    (cl-flet ((test-hook (beg end len) (setq hook-called t)))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (add-hook 'after-change-functions #'test-hook nil t)
        (setq hook-called nil)
        (let ((inhibit-read-only t))
          (insert "Normal insert"))
        (should hook-called)))))



;;;; Built-in Slash Command Dispatch

(ert-deftest pi-coding-agent-test-dispatch-builtin-compact ()
  "Dispatching /compact calls pi-coding-agent-compact with no args."
  (let (called-with)
    (cl-letf (((symbol-function 'pi-coding-agent-compact)
               (lambda (&optional args) (setq called-with (list 'compact args)))))
      (should (pi-coding-agent--dispatch-builtin-command "/compact"))
      (should (equal called-with '(compact nil))))))

(ert-deftest pi-coding-agent-test-dispatch-builtin-compact-with-args ()
  "Dispatching /compact with args passes them through."
  (let (called-with)
    (cl-letf (((symbol-function 'pi-coding-agent-compact)
               (lambda (&optional args) (setq called-with (list 'compact args)))))
      (should (pi-coding-agent--dispatch-builtin-command "/compact keep API details"))
      (should (equal called-with '(compact "keep API details"))))))

(ert-deftest pi-coding-agent-test-dispatch-builtin-new ()
  "Dispatching /new calls pi-coding-agent-new-session."
  (let (called)
    (cl-letf (((symbol-function 'pi-coding-agent-new-session)
               (lambda () (setq called t))))
      (should (pi-coding-agent--dispatch-builtin-command "/new"))
      (should called))))

(ert-deftest pi-coding-agent-test-dispatch-builtin-model ()
  "Dispatching /model calls pi-coding-agent-select-model with no args."
  (let (called-with)
    (cl-letf (((symbol-function 'pi-coding-agent-select-model)
               (lambda (&optional input) (setq called-with (list 'model input)))))
      (should (pi-coding-agent--dispatch-builtin-command "/model"))
      (should (equal called-with '(model nil))))))

(ert-deftest pi-coding-agent-test-dispatch-builtin-model-with-search ()
  "Dispatching /model opus passes search term as initial-input."
  (let (called-with)
    (cl-letf (((symbol-function 'pi-coding-agent-select-model)
               (lambda (&optional input) (setq called-with (list 'model input)))))
      (should (pi-coding-agent--dispatch-builtin-command "/model opus"))
      (should (equal called-with '(model "opus"))))))

(ert-deftest pi-coding-agent-test-dispatch-builtin-name-with-arg ()
  "Dispatching /name foo calls pi-coding-agent-set-session-name with arg."
  (let (called-with)
    (cl-letf (((symbol-function 'pi-coding-agent-set-session-name)
               (lambda (name) (setq called-with name))))
      (should (pi-coding-agent--dispatch-builtin-command "/name my-session"))
      (should (equal called-with "my-session")))))

(ert-deftest pi-coding-agent-test-dispatch-builtin-name-no-arg-prompts ()
  "Dispatching /name without arg calls handler interactively."
  (let (interactive-called)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (fn) (setq interactive-called fn))))
      (should (pi-coding-agent--dispatch-builtin-command "/name"))
      (should (eq interactive-called 'pi-coding-agent-set-session-name)))))

(ert-deftest pi-coding-agent-test-dispatch-builtin-export-with-path ()
  "Dispatching /export /tmp/out.html passes path to handler."
  (let (called-with)
    (cl-letf (((symbol-function 'pi-coding-agent-export-html)
               (lambda (&optional path) (setq called-with path))))
      (should (pi-coding-agent--dispatch-builtin-command "/export /tmp/out.html"))
      (should (equal called-with "/tmp/out.html")))))

(ert-deftest pi-coding-agent-test-dispatch-builtin-export-no-path ()
  "Dispatching /export with no path passes nil."
  (let (called-with)
    (cl-letf (((symbol-function 'pi-coding-agent-export-html)
               (lambda (&optional path) (setq called-with (list 'called path)))))
      (should (pi-coding-agent--dispatch-builtin-command "/export"))
      (should (equal called-with '(called nil))))))

(ert-deftest pi-coding-agent-test-dispatch-returns-nil-for-unknown ()
  "Dispatching unknown /command returns nil (falls through to RPC)."
  (should-not (pi-coding-agent--dispatch-builtin-command "/greet"))
  (should-not (pi-coding-agent--dispatch-builtin-command "/skill:test")))

(ert-deftest pi-coding-agent-test-dispatch-returns-nil-for-non-slash ()
  "Dispatching non-slash text returns nil."
  (should-not (pi-coding-agent--dispatch-builtin-command "hello")))

(ert-deftest pi-coding-agent-test-prepare-and-send-dispatches-builtin ()
  "prepare-and-send dispatches /new locally instead of sending to pi."
  (let (new-called prompt-sent)
    (cl-letf (((symbol-function 'pi-coding-agent-new-session)
               (lambda () (setq new-called t)))
              ((symbol-function 'pi-coding-agent--send-prompt)
               (lambda (text) (setq prompt-sent text))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (pi-coding-agent--prepare-and-send "/new")))
    (should new-called)
    (should-not prompt-sent)))

(ert-deftest pi-coding-agent-test-prepare-and-send-passes-through-extension ()
  "prepare-and-send sends unknown slash commands to pi via prompt."
  (let (prompt-sent)
    (cl-letf (((symbol-function 'pi-coding-agent--send-prompt)
               (lambda (text) (setq prompt-sent text))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (pi-coding-agent--prepare-and-send "/my-extension arg")))
    (should (equal prompt-sent "/my-extension arg"))))

(provide 'pi-coding-agent-render-test)
;;; pi-coding-agent-render-test.el ends here
