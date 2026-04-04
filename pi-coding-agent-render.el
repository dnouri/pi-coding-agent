;;; pi-coding-agent-render.el --- Chat rendering and tool display -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pi-coding-agent

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Rendering module for pi-coding-agent: streaming chat display, tool output,
;; and fontification.
;;
;; This module handles everything that appears in the chat buffer:
;; - Streaming message display (text deltas, thinking blocks)
;; - Tool call output (overlay creation, streaming preview, toggle)
;; - Event dispatching (handle-display-event)
;; - Streaming fontification (incremental syntax highlighting)
;; - Diff overlay highlighting
;; - Compaction display
;; - File navigation from tool blocks
;; - Session history display and rendering

;;; Code:

(require 'pi-coding-agent-ui)
(require 'pi-coding-agent-table)
(require 'cl-lib)
(require 'ansi-color)

;; Forward references for functions in other modules
(declare-function pi-coding-agent-compact "pi-coding-agent-menu" (&optional custom-instructions))

;;;; Response Display

(defun pi-coding-agent--display-user-message (text &optional timestamp)
  "Display user message TEXT in the chat buffer.
If TIMESTAMP (Emacs time value) is provided, display it in the header."
  (let ((start (with-current-buffer (pi-coding-agent--get-chat-buffer) (point-max))))
    (pi-coding-agent--append-to-chat
     (concat "\n" (pi-coding-agent--make-separator "You" timestamp) "\n"
             text "\n"))
    (with-current-buffer (pi-coding-agent--get-chat-buffer)
      (pi-coding-agent--decorate-tables-in-region start (point-max)))))

(defun pi-coding-agent--display-agent-start ()
  "Display separator for new agent turn.
Only shows the Assistant header once per prompt, even during retries.
Note: status is set to `streaming' by the event handler."
  (pi-coding-agent--set-aborted nil)  ; Reset abort flag for new turn
  ;; Only show header if not already shown for this prompt.
  (unless pi-coding-agent--assistant-header-shown
    (pi-coding-agent--append-to-chat
     (concat "\n" (pi-coding-agent--make-separator "Assistant") "\n"))
    (setq pi-coding-agent--assistant-header-shown t))
  ;; Create markers at current end position
  ;; message-start-marker: where content begins (for later replacement)
  ;; streaming-marker: where new deltas are inserted
  (pi-coding-agent--set-message-start-marker (copy-marker (point-max) nil))
  (pi-coding-agent--set-streaming-marker (copy-marker (point-max) t))
  ;; Reset streaming parse state - content starts at line beginning, outside code/thinking block
  (setq pi-coding-agent--line-parse-state 'line-start)
  (setq pi-coding-agent--in-code-block nil)
  (setq pi-coding-agent--in-thinking-block nil)
  (pi-coding-agent--set-activity-phase "thinking"))

(defun pi-coding-agent--process-streaming-char (char state in-block)
  "Process CHAR with current STATE and IN-BLOCK flag.
Returns (NEW-STATE . NEW-IN-BLOCK).
STATE is one of: `line-start', `fence-1', `fence-2', `mid-line'."
  (pcase state
    ('line-start
     (cond
      ((eq char ?`) (cons 'fence-1 in-block))
      ((eq char ?\n) (cons 'line-start in-block))
      (t (cons 'mid-line in-block))))
    ('fence-1
     (cond
      ((eq char ?`) (cons 'fence-2 in-block))
      ((eq char ?\n) (cons 'line-start in-block))
      (t (cons 'mid-line in-block))))
    ('fence-2
     (cond
      ((eq char ?`) (cons 'mid-line (not in-block)))  ; Toggle code block!
      ((eq char ?\n) (cons 'line-start in-block))     ; Was just ``
      (t (cons 'mid-line in-block))))                 ; Was inline ``x
    ('mid-line
     (if (eq char ?\n)
         (cons 'line-start in-block)
       (cons 'mid-line in-block)))))

(defun pi-coding-agent--transform-delta (delta)
  "Transform DELTA for display, handling code blocks and heading levels.
Uses and updates buffer-local state variables for parse state.
Returns the transformed string.

Performance: Uses a two-pass approach.  First checks if transformation
is needed (rare), then only does the work when necessary.  The common
case of no headings is O(n) with no allocations."
  (let ((state pi-coding-agent--line-parse-state)
        (in-block pi-coding-agent--in-code-block)
        (len (length delta))
        (needs-transform nil)
        (i 0))
    ;; First pass: check if any transformation is needed and track state
    ;; Also collect positions where we need to insert extra #
    (let ((insert-positions nil))
      (while (< i len)
        (let ((char (aref delta i)))
          ;; Check if we need to add # at this position
          (when (and (eq state 'line-start)
                     (not in-block)
                     (eq char ?#))
            (push i insert-positions)
            (setq needs-transform t))
          ;; Update state
          (let ((new-state (pi-coding-agent--process-streaming-char char state in-block)))
            (setq state (car new-state))
            (setq in-block (cdr new-state)))
          (setq i (1+ i))))
      ;; Save final state
      (setq pi-coding-agent--line-parse-state state)
      (setq pi-coding-agent--in-code-block in-block)
      ;; Fast path: no transformation needed
      (if (not needs-transform)
          delta
        ;; Slow path: build result with extra # at marked positions
        ;; insert-positions is in reverse order (last position first)
        (let ((positions (nreverse insert-positions))
              (result nil)
              (prev-pos 0))
          (dolist (pos positions)
            ;; Add content before this position
            (when (< prev-pos pos)
              (push (substring delta prev-pos pos) result))
            ;; Add the extra #
            (push "#" result)
            (setq prev-pos pos))
          ;; Add remaining content
          (when (< prev-pos len)
            (push (substring delta prev-pos) result))
          (apply #'concat (nreverse result)))))))

(defun pi-coding-agent--display-message-delta (delta)
  "Display streaming message DELTA at the streaming marker.
Transforms ATX headings (outside code blocks) by adding one # level
to keep our setext H1 separators as the top-level document structure.
Modification hooks fire normally so jit-lock marks inserted text for
fontification; tree-sitter re-parses at the C level on each insert."
  (when (and delta pi-coding-agent--streaming-marker)
    (let* ((inhibit-read-only t)
           ;; Strip leading newlines from first content after header
           (delta (if (and pi-coding-agent--message-start-marker
                          (= (marker-position pi-coding-agent--message-start-marker)
                             (marker-position pi-coding-agent--streaming-marker)))
                     (string-trim-left delta "\n+")
                   delta))
           (transformed (pi-coding-agent--transform-delta delta)))
      (pi-coding-agent--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position pi-coding-agent--streaming-marker))
          (insert transformed)
          (set-marker pi-coding-agent--streaming-marker (point))))
      ;; After inserting text with completed lines, check for active table
      (when (string-match-p "\n" delta)
        (pi-coding-agent--maybe-decorate-streaming-table)))))

(defun pi-coding-agent--thinking-insert-position ()
  "Return insertion position for thinking text.
Prefers `pi-coding-agent--thinking-marker' when available so interleaved
tool headers do not move the thinking insertion point."
  (if (and pi-coding-agent--thinking-marker
           (marker-position pi-coding-agent--thinking-marker))
      (marker-position pi-coding-agent--thinking-marker)
    (marker-position pi-coding-agent--streaming-marker)))

(defun pi-coding-agent--thinking-normalize-text (text)
  "Normalize streaming thinking TEXT for stable markdown rendering.
Removes boundary blank lines and collapses internal blank-line runs to
at most one empty paragraph separator while preserving indentation."
  (let* ((source (or text ""))
         (without-leading-blank-lines
          (replace-regexp-in-string "\\`\\(?:[ \t]*\n\\)+" "" source))
         (without-boundary-blank-lines
          (replace-regexp-in-string "\\(?:\n[ \t]*\\)+\\'" ""
                                    without-leading-blank-lines)))
    (if (string-empty-p without-boundary-blank-lines)
        ""
      (replace-regexp-in-string
       "\n\\(?:[ \t]*\n\\)\\{2,\\}" "\n\n"
       without-boundary-blank-lines))))

(defun pi-coding-agent--thinking-blockquote-text (text)
  "Convert normalized thinking TEXT to markdown blockquote lines."
  (if (string-empty-p text)
      ""
    (concat "> " (replace-regexp-in-string "\n" "\n> " text))))

(defun pi-coding-agent--render-thinking-content ()
  "Render normalized accumulated thinking content in place.
Returns non-nil when meaningful content remains after normalization."
  (when (and (markerp pi-coding-agent--thinking-start-marker)
             (markerp pi-coding-agent--thinking-marker)
             (marker-position pi-coding-agent--thinking-start-marker)
             (marker-position pi-coding-agent--thinking-marker))
    (let* ((start (marker-position pi-coding-agent--thinking-start-marker))
           (end (marker-position pi-coding-agent--thinking-marker))
           (normalized (pi-coding-agent--thinking-normalize-text
                        pi-coding-agent--thinking-raw))
           (rendered (pi-coding-agent--thinking-blockquote-text normalized))
           (prev pi-coding-agent--thinking-prev-rendered))
      (when (<= start end)
        (cond
         ;; Fast path: new rendered text extends previous — just append suffix.
         ((and prev
               (not (string-empty-p prev))
               (string-prefix-p prev rendered))
          (let ((suffix (substring rendered (length prev))))
            (unless (string-empty-p suffix)
              (goto-char end)
              (insert suffix)
              (set-marker pi-coding-agent--thinking-marker (point)))))
         ;; Slow path: full rewrite; skip if buffer already matches.
         (t
          (let ((existing (buffer-substring-no-properties start end)))
            (unless (equal existing rendered)
              (goto-char start)
              (delete-region start end)
              (insert rendered)
              (set-marker pi-coding-agent--thinking-marker (point))))))
        (setq pi-coding-agent--thinking-prev-rendered rendered))
      (and (<= start end)
           (not (string-empty-p normalized))))))

(defun pi-coding-agent--ensure-blank-line-separator ()
  "Ensure exactly one blank line separator at point.
Normalizes any existing newline run to two newlines."
  (let ((start (point))
        (scan (point))
        (newline-count 0))
    (while (eq (char-after scan) ?\n)
      (setq newline-count (1+ newline-count))
      (setq scan (1+ scan)))
    (cond
     ((< newline-count 2)
      (insert (make-string (- 2 newline-count) ?\n)))
     ((> newline-count 2)
      (delete-region (+ start 2) (+ start newline-count))))))

(defun pi-coding-agent--ensure-blank-line-before-block ()
  "Ensure point is on a fresh line with a blank line above.
Used before inserting a new block (thinking, tool) so it is visually
separated from preceding content."
  (unless (bolp)
    (insert "\n"))
  (unless (save-excursion
            (forward-line -1)
            (looking-at-p "^$"))
    (insert "\n")))

(defun pi-coding-agent--reset-thinking-state ()
  "Detach and clear all thinking-stream state for the current turn."
  (when (markerp pi-coding-agent--thinking-marker)
    (set-marker pi-coding-agent--thinking-marker nil))
  (when (markerp pi-coding-agent--thinking-start-marker)
    (set-marker pi-coding-agent--thinking-start-marker nil))
  (setq pi-coding-agent--thinking-marker nil
        pi-coding-agent--thinking-start-marker nil
        pi-coding-agent--thinking-raw nil
        pi-coding-agent--thinking-prev-rendered nil))

(defun pi-coding-agent--display-thinking-start ()
  "Insert opening marker for thinking block (blockquote)."
  (when pi-coding-agent--streaming-marker
    (setq pi-coding-agent--in-thinking-block t)
    (let ((inhibit-read-only t))
      (pi-coding-agent--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position pi-coding-agent--streaming-marker))
          ;; No separator needed when this is the first content in the message.
          (when (and pi-coding-agent--message-start-marker
                     (> (point)
                        (marker-position pi-coding-agent--message-start-marker)))
            (pi-coding-agent--ensure-blank-line-before-block))
          ;; Track thinking insertion separately so it stays anchored even if
          ;; other block types (tool headers) interleave in the same message.
          ;; Keep insertion-type nil so inserts at this exact point happen
          ;; after the marker (we then advance it explicitly per delta).
          (pi-coding-agent--reset-thinking-state)
          (setq pi-coding-agent--thinking-raw "")
          (let ((start (point)))
            (insert "> ")
            (setq pi-coding-agent--thinking-start-marker
                  (copy-marker start nil))
            (setq pi-coding-agent--thinking-marker
                  (copy-marker (point) nil))))))))

(defun pi-coding-agent--display-thinking-delta (delta)
  "Display streaming thinking DELTA in the current thinking block.
Normalizes boundary and paragraph whitespace while streaming."
  (when (and delta pi-coding-agent--streaming-marker)
    (let ((inhibit-read-only t))
      (if (and pi-coding-agent--thinking-start-marker
               pi-coding-agent--thinking-marker)
          (progn
            (setq pi-coding-agent--thinking-raw
                  (concat (or pi-coding-agent--thinking-raw "") delta))
            (pi-coding-agent--with-scroll-preservation
              (save-excursion
                (pi-coding-agent--render-thinking-content))))
        ;; Fallback for malformed event streams that skip thinking_start.
        (let ((transformed (replace-regexp-in-string "\n" "\n> " delta)))
          (pi-coding-agent--with-scroll-preservation
            (save-excursion
              (goto-char (pi-coding-agent--thinking-insert-position))
              (insert transformed)
              (when pi-coding-agent--thinking-marker
                (set-marker pi-coding-agent--thinking-marker (point))))))))))

(defun pi-coding-agent--display-thinking-end (_content)
  "End thinking block (blockquote).
CONTENT is ignored - we use what was already streamed."
  (when pi-coding-agent--streaming-marker
    (setq pi-coding-agent--in-thinking-block nil)
    (let ((inhibit-read-only t))
      (pi-coding-agent--with-scroll-preservation
        (save-excursion
          (if (and pi-coding-agent--thinking-start-marker
                   pi-coding-agent--thinking-marker)
              (when (pi-coding-agent--render-thinking-content)
                (goto-char (pi-coding-agent--thinking-insert-position))
                (pi-coding-agent--ensure-blank-line-separator))
            ;; Fallback for malformed event streams that skip thinking_start.
            (goto-char (pi-coding-agent--thinking-insert-position))
            (pi-coding-agent--ensure-blank-line-separator))
          (pi-coding-agent--reset-thinking-state))))))

(defun pi-coding-agent--display-agent-end ()
  "Finalize agent turn: normalize whitespace, handle abort, process queue.
Note: status is set to `idle' by the event handler."
  ;; Reset per-turn state for clean next turn.
  (setq pi-coding-agent--local-user-message nil)
  (setq pi-coding-agent--in-thinking-block nil)
  (pi-coding-agent--reset-thinking-state)
  (let ((was-aborted pi-coding-agent--aborted))
    (let ((inhibit-read-only t))
      (pi-coding-agent--finalize-live-tool-blocks 'pi-coding-agent-tool-block-error)
      (when pi-coding-agent--tool-args-cache
        (clrhash pi-coding-agent--tool-args-cache))
      ;; Abort means "stop everything" — discard queued follow-ups too
      (when pi-coding-agent--aborted
        (pi-coding-agent--with-scroll-preservation
          (save-excursion
            (goto-char (point-max))
            ;; Remove trailing whitespace before adding indicator
            (skip-chars-backward " \t\n")
            (delete-region (point) (point-max))
            (insert "\n\n" (propertize "[Aborted]" 'face 'error) "\n")))
        (pi-coding-agent--set-aborted nil)
        (pi-coding-agent--clear-followup-queue))
      (pi-coding-agent--with-scroll-preservation
        (save-excursion
          (goto-char (point-max))
          (skip-chars-backward "\n")
          (delete-region (point) (point-max))
          (insert "\n"))))
    (pi-coding-agent--set-activity-phase "idle")
    (pi-coding-agent--refresh-header)
    ;; Check follow-up queue and send next message if any (unless aborted)
    (unless was-aborted
      (pi-coding-agent--process-followup-queue))))

(defun pi-coding-agent--dispatch-builtin-command (text)
  "Try to dispatch TEXT as a built-in slash command.
Returns non-nil if TEXT matched a built-in command and was handled."
  (when (string-prefix-p "/" text)
    (let* ((without-slash (substring text 1))
           (words (split-string without-slash))
           (cmd-name (car words))
           (entry (assoc cmd-name pi-coding-agent--builtin-commands)))
      (when entry
        (let ((handler (plist-get (cdr entry) :handler))
              (args-spec (plist-get (cdr entry) :args))
              (arg-str (let ((rest (string-trim
                                    (substring without-slash (length cmd-name)))))
                         (and (not (string-empty-p rest)) rest))))
          (pcase args-spec
            ('optional (funcall handler arg-str))
            ('required (if arg-str
                          (funcall handler arg-str)
                        (call-interactively handler)))
            (_ (funcall handler)))
          t)))))

(defun pi-coding-agent--prepare-and-send (text)
  "Prepare chat buffer state and send TEXT to pi.
Built-in slash commands are dispatched locally via the dispatch table.
Other slash commands (extensions, skills, prompts) are sent to pi.
Regular text is displayed locally for responsiveness, then sent.
Must be called with chat buffer current.
Status transitions are handled by pi events (agent_start, agent_end)."
  (cond
   ;; Built-in slash commands: dispatch locally
   ((pi-coding-agent--dispatch-builtin-command text))
   ;; Other slash commands: don't display locally, send to pi
   ((string-prefix-p "/" text)
    (pi-coding-agent--send-prompt text))
   ;; Regular text: display locally for responsiveness, then send
   (t
    (pi-coding-agent--display-user-message text (current-time))
    (setq pi-coding-agent--local-user-message text)
    (setq pi-coding-agent--assistant-header-shown nil)
    (pi-coding-agent--send-prompt text))))

(defun pi-coding-agent--process-followup-queue ()
  "Dequeue and send the oldest follow-up message.
Does nothing if queue is empty.  Messages are processed in FIFO order."
  (when-let* ((text (pi-coding-agent--dequeue-followup)))
    (pi-coding-agent--prepare-and-send text)))

(defun pi-coding-agent--display-retry-start (event)
  "Display retry notice from auto_retry_start EVENT.
Shows attempt number, delay, and raw error message."
  (let* ((attempt (plist-get event :attempt))
         (max-attempts (plist-get event :maxAttempts))
         (delay-ms (plist-get event :delayMs))
         (error-msg (or (plist-get event :errorMessage) "transient error"))
         (delay-sec (/ (or delay-ms 0) 1000.0))
         (notice (format "⟳ Retry %d/%d in %.0fs — %s"
                         (or attempt 1)
                         (or max-attempts 3)
                         delay-sec
                         error-msg)))
    (pi-coding-agent--append-to-chat
     (concat (propertize notice 'face 'pi-coding-agent-retry-notice) "\n"))))

(defun pi-coding-agent--display-retry-end (event)
  "Display retry result from auto_retry_end EVENT.
Shows success or final failure with raw error."
  (let* ((success (plist-get event :success))
         (attempt (plist-get event :attempt))
         (final-error (or (plist-get event :finalError) "unknown error")))
    (if (eq success t)
        (pi-coding-agent--append-to-chat
         (concat (propertize (format "✓ Retry succeeded on attempt %d"
                                     (or attempt 1))
                             'face 'pi-coding-agent-retry-notice)
                 "\n\n"))
      ;; Final failure
      (pi-coding-agent--append-to-chat
       (concat (propertize (format "✗ Retry failed after %d attempts — %s"
                                   (or attempt 1)
                                   final-error)
                           'face 'pi-coding-agent-error-notice)
               "\n\n")))))

(defun pi-coding-agent--display-error (error-msg)
  "Display ERROR-MSG from the server."
  (pi-coding-agent--append-to-chat
   (concat "\n" (propertize (format "[Error: %s]" (or error-msg "unknown"))
                            'face 'pi-coding-agent-error-notice)
           "\n")))

(defun pi-coding-agent--display-extension-error (event)
  "Display extension error from extension_error EVENT."
  (let* ((extension-path (plist-get event :extensionPath))
         (extension-event (plist-get event :event))
         (error-msg (plist-get event :error))
         (extension-name (if extension-path (file-name-nondirectory extension-path) "unknown")))
    (pi-coding-agent--append-to-chat
     (concat "\n"
             (propertize (format "[Extension error in %s (%s): %s]"
                                 extension-name
                                 (or extension-event "unknown")
                                 (or error-msg "unknown error"))
                         'face 'pi-coding-agent-error-notice)
             "\n"))))

(defun pi-coding-agent--extension-ui-notify (event)
  "Handle notify method from EVENT."
  (let ((msg (plist-get event :message))
        (notify-type (plist-get event :notifyType)))
    (message "Pi: %s%s"
             (pcase notify-type
               ("warning" "⚠ ")
               ("error" "✗ ")
               (_ ""))
             msg)))

(defun pi-coding-agent--extension-ui-confirm (event proc)
  "Handle confirm method from EVENT, responding via PROC."
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (msg (plist-get event :message))
         ;; Don't add colon if title already ends with one
         (separator (if (string-suffix-p ":" title) " " ": "))
         (prompt (format "%s%s%s " title separator msg))
         (confirmed (yes-or-no-p prompt)))
    (when proc
      (pi-coding-agent--send-extension-ui-response proc
                     (list :type "extension_ui_response"
                           :id id
                           :confirmed (if confirmed t :json-false))))))

(defun pi-coding-agent--extension-ui-select (event proc)
  "Handle select method from EVENT, responding via PROC."
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (options (append (plist-get event :options) nil))
         (selected (completing-read (concat title " ") options nil t)))
    (when proc
      (pi-coding-agent--send-extension-ui-response proc
                     (list :type "extension_ui_response"
                           :id id
                           :value selected)))))

(defun pi-coding-agent--extension-ui-input (event proc)
  "Handle input method from EVENT, responding via PROC."
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (placeholder (plist-get event :placeholder))
         (value (read-string (concat title " ") placeholder)))
    (when proc
      (pi-coding-agent--send-extension-ui-response proc
                     (list :type "extension_ui_response"
                           :id id
                           :value value)))))

(defun pi-coding-agent--extension-ui-set-editor-text (event)
  "Handle set_editor_text method from EVENT."
  (let ((text (plist-get event :text)))
    (when-let* ((input-buf pi-coding-agent--input-buffer))
      (when (buffer-live-p input-buf)
        (with-current-buffer input-buf
          (erase-buffer)
          (insert text))))))

(defun pi-coding-agent--extension-ui-set-status (event)
  "Handle setStatus method from EVENT."
  (let ((key (plist-get event :statusKey))
        (text (plist-get event :statusText)))
    (when text
      (setq text (ansi-color-filter-apply text)))
    (if text
        (setq pi-coding-agent--extension-status
              (cons (cons key text)
                    (assoc-delete-all key pi-coding-agent--extension-status)))
      (setq pi-coding-agent--extension-status
            (assoc-delete-all key pi-coding-agent--extension-status)))
    (force-mode-line-update t)))

(defun pi-coding-agent--extension-ui-set-working-message (event)
  "Handle setWorkingMessage method from EVENT."
  (let ((msg (plist-get event :message)))
    (when msg
      (setq msg (ansi-color-filter-apply msg)))
    (setq pi-coding-agent--working-message msg)
    (force-mode-line-update t)))

(defun pi-coding-agent--extension-ui-unsupported (event proc)
  "Handle unsupported method from EVENT by sending cancelled via PROC."
  (when proc
    (pi-coding-agent--rpc-async proc
                   (list :type "extension_ui_response"
                         :id (plist-get event :id)
                         :cancelled t)
                   #'ignore)))

(defun pi-coding-agent--handle-extension-ui-request (event)
  "Handle extension_ui_request EVENT from pi.
Dispatches to appropriate handler based on method."
  (let ((method (plist-get event :method))
        (proc pi-coding-agent--process))
    (pcase method
      ("notify"         (pi-coding-agent--extension-ui-notify event))
      ("confirm"        (pi-coding-agent--extension-ui-confirm event proc))
      ("select"         (pi-coding-agent--extension-ui-select event proc))
      ("input"          (pi-coding-agent--extension-ui-input event proc))
      ("set_editor_text" (pi-coding-agent--extension-ui-set-editor-text event))
      ("setStatus"      (pi-coding-agent--extension-ui-set-status event))
      ("setWorkingMessage" (pi-coding-agent--extension-ui-set-working-message event))
      (_                (pi-coding-agent--extension-ui-unsupported event proc)))))

(defun pi-coding-agent--display-no-model-warning ()
  "Display warning when no model is available.
Shown when the session starts without a configured model/API key."
  (pi-coding-agent--append-to-chat
   (concat "\n"
           (propertize "⚠ No models available"
                       'face 'pi-coding-agent-error-notice)
           "\n\n"
           (propertize "To get started, either:\n"
                       'face 'pi-coding-agent-retry-notice)
           (propertize "  • Set an API key: "
                       'face 'pi-coding-agent-retry-notice)
           "ANTHROPIC_API_KEY, OPENAI_API_KEY, GEMINI_API_KEY, etc.\n"
           (propertize "  • Or run "
                       'face 'pi-coding-agent-retry-notice)
           (propertize "pi --login"
                       'face 'pi-coding-agent-tool-command)
           (propertize " in a terminal to authenticate via OAuth\n"
                       'face 'pi-coding-agent-retry-notice)
           "\n")))

(defun pi-coding-agent--cleanup-on-kill ()
  "Clean up resources when chat buffer is killed.
Also kills the linked input buffer and fontification cache buffers.

Note: This runs from `kill-buffer-hook', which executes AFTER the kill
decision is made.  For proper cancellation support, use `pi-coding-agent-quit'
which asks upfront before any buffers are touched."
  (when (derived-mode-p 'pi-coding-agent-chat-mode)
    (when pi-coding-agent--process
      (pi-coding-agent--unregister-display-handler pi-coding-agent--process)
      (when (process-live-p pi-coding-agent--process)
        (delete-process pi-coding-agent--process)))
    (when (and pi-coding-agent--input-buffer (buffer-live-p pi-coding-agent--input-buffer))
      (let ((input-buf pi-coding-agent--input-buffer))
        (pi-coding-agent--set-input-buffer nil) ; break cycle before kill
        (kill-buffer input-buf)))
    (pi-coding-agent--cleanup-visible-string-buffer)))

(defun pi-coding-agent--cleanup-input-on-kill ()
  "Clean up when input buffer is killed.
Also kills the linked chat buffer (which handles process cleanup).

Note: This runs from `kill-buffer-hook', which executes AFTER the kill
decision is made.  For proper cancellation support, use `pi-coding-agent-quit'
which asks upfront before any buffers are touched."
  (when (derived-mode-p 'pi-coding-agent-input-mode)
    (when (and pi-coding-agent--chat-buffer (buffer-live-p pi-coding-agent--chat-buffer))
      (let* ((chat-buf pi-coding-agent--chat-buffer)
             (proc (buffer-local-value 'pi-coding-agent--process chat-buf)))
        (pi-coding-agent--set-chat-buffer nil) ; break cycle before kill
        (when (and proc (process-live-p proc))
          (set-process-query-on-exit-flag proc nil))
        (kill-buffer chat-buf)))))

(defun pi-coding-agent--register-display-handler (process)
  "Register display event handler for PROCESS."
  (let ((handler (pi-coding-agent--make-display-handler process)))
    (process-put process 'pi-coding-agent-display-handler handler)))

(defun pi-coding-agent--unregister-display-handler (process)
  "Unregister display event handler for PROCESS."
  (process-put process 'pi-coding-agent-display-handler nil))

(defun pi-coding-agent--make-display-handler (process)
  "Create a display event handler for PROCESS."
  (lambda (event)
    (when-let* ((chat-buf (process-get process 'pi-coding-agent-chat-buffer)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (pi-coding-agent--handle-display-event event))))))

(defun pi-coding-agent--handle-display-event (event)
  "Handle EVENT for display purposes.
Updates buffer-local state and renders display updates."
  ;; Update state first (now buffer-local)
  (pi-coding-agent--update-state-from-event event)
  ;; Then handle display
  (pcase (plist-get event :type)
    ("agent_start"
     (pi-coding-agent--display-agent-start))
    ("message_start"
     (let* ((message (plist-get event :message))
            (role (plist-get message :role)))
       ;; A new message starts a fresh rendering context.
       (setq pi-coding-agent--in-thinking-block nil)
       (pi-coding-agent--reset-thinking-state)
       (pcase role
         ("user"
          ;; User message from pi - check if we displayed it locally
          (let* ((content (plist-get message :content))
                 (timestamp (plist-get message :timestamp))
                 (text (when content
                         (pi-coding-agent--extract-user-message-text content)))
                 (local-msg pi-coding-agent--local-user-message))
            ;; Clear local tracking
            (setq pi-coding-agent--local-user-message nil)
            ;; Display if: no local message, OR pi's message differs (expanded template)
            (when (and text
                       (or (null local-msg)
                           (not (string= text local-msg))))
              (pi-coding-agent--display-user-message
               text
               (pi-coding-agent--ms-to-time timestamp))
              ;; Reset so next assistant message shows its header
              (setq pi-coding-agent--assistant-header-shown nil))))
         ("custom"
          ;; Custom message from extension (e.g., /pisay)
          ;; Display content directly if display flag is set
          (when (plist-get message :display)
            (let ((content (plist-get message :content)))
              (when (and content (stringp content) (> (length content) 0))
                (let ((start (point-max)))
                  (pi-coding-agent--append-to-chat (concat "\n" content "\n"))
                  (pi-coding-agent--decorate-tables-in-region start (point-max)))
                ;; Reset so next assistant message shows its header
                (setq pi-coding-agent--assistant-header-shown nil)))))
         (_
          ;; Assistant message - show header if needed, reset markers
          (unless pi-coding-agent--assistant-header-shown
            (pi-coding-agent--append-to-chat
             (concat "\n" (pi-coding-agent--make-separator "Assistant") "\n"))
            (setq pi-coding-agent--assistant-header-shown t))
          (pi-coding-agent--set-message-start-marker (copy-marker (point-max) nil))
          (pi-coding-agent--set-streaming-marker (copy-marker (point-max) t))))))
    ("message_update"
     (when-let* ((msg-event (plist-get event :assistantMessageEvent))
                 (event-type (plist-get msg-event :type)))
       (pcase event-type
         ("text_start") ; No-op: text block started, nothing to render
         ("text_delta"
          (pi-coding-agent--set-activity-phase "replying")
          (pi-coding-agent--display-message-delta (plist-get msg-event :delta)))
         ("text_end"
          ;; Text block ended — finalize any active table that may have
          ;; a trailing row without newline (backstop for streaming).
          (pi-coding-agent--maybe-decorate-streaming-table))
         ("thinking_start"
          (pi-coding-agent--display-thinking-start))
         ("thinking_delta"
          (pi-coding-agent--display-thinking-delta (plist-get msg-event :delta)))
         ("thinking_end"
          (pi-coding-agent--display-thinking-end (plist-get msg-event :content)))
         ((or "toolcall_start" "toolcall_delta" "toolcall_end")
          ;; Preview reconciliation follows the authoritative assistant
          ;; message content, not a singleton streaming-tool state machine.
          (pi-coding-agent--set-activity-phase "running")
          (pi-coding-agent--reconcile-toolcall-previews
           (plist-get event :message)))
         ("error"
          ;; Error during streaming (e.g., API error)
          (pi-coding-agent--display-error (plist-get msg-event :reason))))))
    ("message_end"
     (let* ((message (plist-get event :message))
            (assistant-p (equal (plist-get message :role) "assistant")))
       ;; Display error if message ended with error (e.g., API error)
       (when (equal (plist-get message :stopReason) "error")
         (pi-coding-agent--display-error (plist-get message :errorMessage)))
       ;; Capture usage from assistant messages for context % calculation.
       ;; Skip aborted messages - they may have incomplete usage data and
       ;; would reset context percentage.  Matches TUI footer.ts behavior.
       ;; Note: error messages DO have valid usage data (tokens were consumed).
       (when (and assistant-p
                  (not (equal (plist-get message :stopReason) "aborted"))
                  (plist-get message :usage))
         (pi-coding-agent--set-last-usage (plist-get message :usage)))
       ;; Refresh cumulative stats after each assistant message_end so
       ;; cost and totals update without waiting for agent_end.
       (when assistant-p
         (pi-coding-agent--refresh-header)))
     (pi-coding-agent--render-complete-message))
    ("tool_execution_start"
     (pi-coding-agent--set-activity-phase "running")
     (let* ((tool-call-id (plist-get event :toolCallId))
            (args (plist-get event :args))
            (block (pi-coding-agent--tool-block-get tool-call-id)))
       ;; Cache args for tool_execution_end (which doesn't include args)
       (when (and tool-call-id pi-coding-agent--tool-args-cache)
         (puthash tool-call-id args pi-coding-agent--tool-args-cache))
       ;; Reuse the keyed preview block when it already exists.
       (unless block
         (setq block (pi-coding-agent--display-tool-start
                      (plist-get event :toolName) args tool-call-id)))
       ;; Update header and path from authoritative args.
       ;; During streaming, the header may show placeholders since delta
       ;; args can be partial.  Execution start carries the real args.
       (pi-coding-agent--display-tool-update-header
        (plist-get event :toolName) args block)
       (when-let* ((path (pi-coding-agent--tool-path args)))
         (pi-coding-agent--tool-block-set-path block path))))
    ("tool_execution_end"
     (pi-coding-agent--set-activity-phase "thinking")
     (let* ((tool-call-id (plist-get event :toolCallId))
            (result (plist-get event :result))
            (block (pi-coding-agent--tool-block-get tool-call-id))
            ;; Retrieve cached args since tool_execution_end doesn't include args
            (args (when (and tool-call-id pi-coding-agent--tool-args-cache)
                    (prog1 (gethash tool-call-id pi-coding-agent--tool-args-cache)
                      (remhash tool-call-id pi-coding-agent--tool-args-cache)))))
       (pi-coding-agent--display-tool-end (plist-get event :toolName)
                                          args
                                          (plist-get result :content)
                                          (plist-get result :details)
                                          (plist-get event :isError)
                                          block)))
    ("tool_execution_update"
     (pi-coding-agent--display-tool-update
      (plist-get event :partialResult)
      (pi-coding-agent--tool-block-get (plist-get event :toolCallId))))
    ("auto_compaction_start"
     (setq pi-coding-agent--status 'compacting)
     (pi-coding-agent--set-activity-phase "compact")
     (let ((reason (plist-get event :reason)))
       (message "Pi: %sAuto-compacting... (C-c C-k to cancel)"
                (if (equal reason "overflow") "Context overflow, " ""))))
    ("auto_compaction_end"
     (setq pi-coding-agent--status 'idle)
     (pi-coding-agent--set-activity-phase "idle")
     (if (pi-coding-agent--normalize-boolean (plist-get event :aborted))
         (progn
           (message "Pi: Auto-compaction cancelled")
           ;; Clear queue on abort (user wanted to stop)
           (pi-coding-agent--clear-followup-queue))
       (when-let* ((result (plist-get event :result)))
         (pi-coding-agent--handle-compaction-success
          (plist-get result :tokensBefore)
          (plist-get result :summary)
          (pi-coding-agent--ms-to-time (plist-get result :timestamp))))
       ;; Process followup queue after successful compaction
       (pi-coding-agent--process-followup-queue)))
    ("agent_end"
     (pi-coding-agent--display-agent-end)
     (pi-coding-agent--update-hot-tail-boundary)
     (pi-coding-agent--cool-completed-tool-blocks-outside-hot-tail))
    ("auto_retry_start"
     (pi-coding-agent--display-retry-start event))
    ("auto_retry_end"
     (pi-coding-agent--display-retry-end event))
    ("extension_error"
     (pi-coding-agent--display-extension-error event))
    ("extension_ui_request"
     (pi-coding-agent--handle-extension-ui-request event))))


;;;; Tool Output

(defun pi-coding-agent--truncate-to-visual-lines (content max-lines width)
  "Truncate CONTENT to fit within MAX-LINES visual lines at WIDTH.
Also respects `pi-coding-agent-preview-max-bytes'.
Strips blank lines for compact display but tracks original line numbers.

Returns a plist with:
  :content      - the truncated content (or original if no truncation)
  :visual-lines - number of visual lines in result
  :hidden-lines - raw lines hidden (including stripped blanks)
  :line-map     - vector mapping displayed line to original line number"
  (let* ((safe-max-lines (max 0 (or max-lines 0)))
         (safe-width (max 1 (or width 1)))
         (trimmed (string-trim-right content "\n+"))
         (all-lines (if (string-empty-p trimmed)
                        nil
                      (split-string trimmed "\n")))
         (total-raw-lines (length all-lines))
         (visual-count 0)
         (byte-count 0)
         (max-bytes pi-coding-agent-preview-max-bytes)
         (result-lines nil)
         (line-map nil)  ; list of original line numbers for kept lines
         (truncated-first-line nil)
         (original-line-num 0))
    (if (= safe-max-lines 0)
        (list :content ""
              :visual-lines 0
              :hidden-lines total-raw-lines
              :line-map [])
      ;; Accumulate non-blank lines until we'd exceed limits
      (catch 'done
        (dolist (line all-lines)
          (setq original-line-num (1+ original-line-num))
          ;; Skip blank lines (they don't count toward visual limit)
          (unless (string-empty-p line)
            (let* ((line-len (length line))
                   ;; Visual lines: ceiling(length / width), minimum 1
                   (line-visual-lines (max 1 (ceiling (float line-len) safe-width)))
                   (new-visual-count (+ visual-count line-visual-lines))
                   ;; +1 for newline between lines
                   (new-byte-count (+ byte-count line-len (if result-lines 1 0))))
              ;; Check if adding this line would exceed limits
              (cond
               ;; Not first line and exceeds limits: stop
               ((and result-lines
                     (or (> new-visual-count safe-max-lines)
                         (> new-byte-count max-bytes)))
                (throw 'done nil))
               ;; First line exceeds limits: truncate it to fit
               ((and (null result-lines)
                     (or (> new-visual-count safe-max-lines)
                         (> new-byte-count max-bytes)))
                (let* ((max-chars-by-visual (* safe-max-lines safe-width))
                       (max-chars (min max-chars-by-visual max-bytes)))
                  (setq line (substring line 0 (min line-len max-chars)))
                  (setq line-len (length line))
                  (setq line-visual-lines (max 1 (ceiling (float line-len) safe-width)))
                  (setq new-visual-count line-visual-lines)
                  (setq new-byte-count line-len)
                  (setq truncated-first-line t))))
              (setq visual-count new-visual-count)
              (setq byte-count new-byte-count)
              (push line result-lines)
              (push original-line-num line-map)))))
      (let* ((kept-lines (nreverse result-lines))
             (line-map-vec (vconcat (nreverse line-map)))
             (last-displayed (if (> (length line-map-vec) 0)
                                 (aref line-map-vec (1- (length line-map-vec)))
                               0))
             (hidden (- total-raw-lines last-displayed)))
        (list :content (string-join kept-lines "\n")
              :visual-lines visual-count
              ;; Report hidden lines; truncated first line means there's hidden content even with 1 line
              :hidden-lines (if (and truncated-first-line (= hidden 0)) 1 hidden)
              :line-map line-map-vec)))))

(defun pi-coding-agent--clear-render-artifacts ()
  "Delete pi-owned render overlays in the current chat buffer.
This removes completed/pending tool overlays and diff overlays before
buffer reset or history rebuild, then clears keyed live-tool state,
cached execution args, and the compatibility pending overlay slot so
buffer state and overlay state stay consistent.  Tree-sitter overlays
are left alone."
  (remove-overlays (point-min) (point-max) 'pi-coding-agent-tool-block t)
  (remove-overlays (point-min) (point-max) 'pi-coding-agent-diff-overlay t)
  (setq pi-coding-agent--pending-tool-overlay nil
        pi-coding-agent--tool-block-order-counter 0)
  (when pi-coding-agent--tool-args-cache
    (clrhash pi-coding-agent--tool-args-cache))
  (when pi-coding-agent--live-tool-blocks
    (clrhash pi-coding-agent--live-tool-blocks)))

(cl-defstruct (pi-coding-agent--tool-block
               (:constructor pi-coding-agent--make-tool-block))
  tool-call-id
  overlay
  header-end
  end-marker
  order
  path
  offset
  line-map
  last-tail)

(defun pi-coding-agent--ensure-live-tool-blocks ()
  "Return the live tool block registry for the current buffer."
  (or pi-coding-agent--live-tool-blocks
      (setq pi-coding-agent--live-tool-blocks
            (make-hash-table :test 'equal))))

(defun pi-coding-agent--next-tool-block-order ()
  "Return the next monotonically increasing live tool block order."
  (let ((order (or pi-coding-agent--tool-block-order-counter 0)))
    (setq pi-coding-agent--tool-block-order-counter (1+ order))
    order))

(defun pi-coding-agent--reserve-tool-block-order (&optional order)
  "Return ORDER, or allocate the next implicit live tool block order.
When ORDER is non-nil, advance the monotonic counter past it so later
implicit insertions still sort after explicitly ordered preview blocks."
  (if order
      (progn
        (setq pi-coding-agent--tool-block-order-counter
              (max (or pi-coding-agent--tool-block-order-counter 0)
                   (1+ order)))
        order)
    (pi-coding-agent--next-tool-block-order)))

(defun pi-coding-agent--tool-block-get (tool-call-id)
  "Return the live tool block for TOOL-CALL-ID, or nil."
  (when (and tool-call-id pi-coding-agent--live-tool-blocks)
    (gethash tool-call-id pi-coding-agent--live-tool-blocks)))

(defun pi-coding-agent--live-tool-blocks-in-order ()
  "Return all live tool blocks sorted by their recorded order."
  (let (blocks)
    (when pi-coding-agent--live-tool-blocks
      (maphash (lambda (_tool-call-id block)
                 (push block blocks))
               pi-coding-agent--live-tool-blocks))
    (sort blocks
          (lambda (left right)
            (< (pi-coding-agent--tool-block-order left)
               (pi-coding-agent--tool-block-order right))))))

(defun pi-coding-agent--tool-block-next-after-order (order)
  "Return the first live tool block whose order is greater than ORDER."
  (seq-find (lambda (block)
              (> (pi-coding-agent--tool-block-order block) order))
            (pi-coding-agent--live-tool-blocks-in-order)))

(defun pi-coding-agent--tool-block-register (block)
  "Register BLOCK in the keyed live registry when it has a tool call ID."
  (when-let* ((tool-call-id (pi-coding-agent--tool-block-tool-call-id block)))
    (puthash tool-call-id block (pi-coding-agent--ensure-live-tool-blocks)))
  block)

(defun pi-coding-agent--tool-block-unregister (block)
  "Remove BLOCK from the keyed live registry."
  (when-let* ((tool-call-id (pi-coding-agent--tool-block-tool-call-id block))
              (live-blocks pi-coding-agent--live-tool-blocks))
    (remhash tool-call-id live-blocks))
  block)

(defun pi-coding-agent--tool-block-from-overlay (overlay)
  "Return the tool block record attached to OVERLAY, or nil."
  (and overlay (overlay-get overlay 'pi-coding-agent-tool-block-record)))

(defun pi-coding-agent--current-tool-block ()
  "Return the compatibility current tool block, or nil.
This is only used by legacy single-tool rendering paths that still rely
on `pi-coding-agent--pending-tool-overlay'."
  (pi-coding-agent--tool-block-from-overlay pi-coding-agent--pending-tool-overlay))

(defun pi-coding-agent--all-live-tool-blocks ()
  "Return all distinct live tool blocks in the current buffer.
Includes keyed blocks from `pi-coding-agent--live-tool-blocks' and, when
needed for compatibility, the current non-keyed pending block."
  (let ((blocks (pi-coding-agent--live-tool-blocks-in-order))
        (current (pi-coding-agent--current-tool-block)))
    (if (and current (not (memq current blocks)))
        (append blocks (list current))
      blocks)))

(defun pi-coding-agent--finalize-live-tool-blocks (face)
  "Finalize every currently live tool block with FACE."
  (dolist (block (pi-coding-agent--all-live-tool-blocks))
    (pi-coding-agent--tool-block-finalize block face)))

(defun pi-coding-agent--tool-block-overlays-in-region (start end)
  "Return tool block overlays overlapping START..END in buffer order."
  (sort (seq-filter (lambda (ov)
                      (overlay-get ov 'pi-coding-agent-tool-block))
                    (overlays-in start end))
        (lambda (left right)
          (< (overlay-start left) (overlay-start right)))))

(defun pi-coding-agent--tool-block-refresh-overlay (block)
  "Sync BLOCK metadata and bounds onto its overlay."
  (when-let* ((ov (pi-coding-agent--tool-block-overlay block))
              (end-marker (pi-coding-agent--tool-block-end-marker block)))
    (move-overlay ov (overlay-start ov) (marker-position end-marker))
    (overlay-put ov 'pi-coding-agent-tool-block-record block)
    (overlay-put ov 'pi-coding-agent-header-end
                 (pi-coding-agent--tool-block-header-end block))
    (overlay-put ov 'pi-coding-agent-tool-path
                 (pi-coding-agent--tool-block-path block))
    (overlay-put ov 'pi-coding-agent-tool-offset
                 (pi-coding-agent--tool-block-offset block))
    (overlay-put ov 'pi-coding-agent-line-map
                 (pi-coding-agent--tool-block-line-map block))
    (overlay-put ov 'pi-coding-agent-last-tail
                 (pi-coding-agent--tool-block-last-tail block)))
  block)

(defun pi-coding-agent--tool-block-set-path (block path)
  "Store PATH metadata on BLOCK and its overlay."
  (when block
    (setf (pi-coding-agent--tool-block-path block) path)
    (pi-coding-agent--tool-block-refresh-overlay block))
  block)

(defun pi-coding-agent--tool-block-set-offset (block offset)
  "Store OFFSET metadata on BLOCK and its overlay."
  (when block
    (setf (pi-coding-agent--tool-block-offset block) offset)
    (pi-coding-agent--tool-block-refresh-overlay block))
  block)

(defun pi-coding-agent--tool-block-set-line-map (block line-map)
  "Store LINE-MAP metadata on BLOCK and its overlay."
  (when block
    (setf (pi-coding-agent--tool-block-line-map block) line-map)
    (pi-coding-agent--tool-block-refresh-overlay block))
  block)

(defun pi-coding-agent--tool-block-set-last-tail (block last-tail)
  "Store LAST-TAIL preview cache metadata on BLOCK and its overlay."
  (when block
    (setf (pi-coding-agent--tool-block-last-tail block) last-tail)
    (pi-coding-agent--tool-block-refresh-overlay block))
  block)

(defun pi-coding-agent--tool-block-create (tool-name args &optional tool-call-id order)
  "Insert a live tool block for TOOL-NAME with ARGS and return it.
When TOOL-CALL-ID is non-nil, register the block in the keyed live
registry.  ORDER records the intended block ordering metadata."
  (let* ((block-order (pi-coding-agent--reserve-tool-block-order order))
         (next-block (and order
                          (pi-coding-agent--tool-block-next-after-order
                           block-order)))
         (header-display (pi-coding-agent--tool-header tool-name args))
         (path (pi-coding-agent--tool-path args))
         (block nil)
         (inhibit-read-only t))
    (pi-coding-agent--with-scroll-preservation
      (save-excursion
        (goto-char (if next-block
                       (overlay-start (pi-coding-agent--tool-block-overlay next-block))
                     (point-max)))
        (pi-coding-agent--ensure-blank-line-before-block)
        (let ((start (point)))
          (insert header-display "\n")
          (let* ((header-end (copy-marker (point) nil))
                 ;; Keep the body-end marker fixed on unrelated inserts at the
                 ;; block boundary.  Live updates move it explicitly.
                 (end-marker (copy-marker (point) nil)))
            ;; When inserting before an already-live later block, keep one
            ;; blank separator after the new block without making it part of
            ;; the tool block overlay itself.
            (when next-block
              (insert "\n"))
            (let ((ov (make-overlay start (marker-position end-marker) nil nil nil)))
              (overlay-put ov 'pi-coding-agent-tool-block t)
              (overlay-put ov 'pi-coding-agent-tool-name tool-name)
              (overlay-put ov 'face 'pi-coding-agent-tool-block)
              (setq block (pi-coding-agent--make-tool-block
                           :tool-call-id tool-call-id
                           :overlay ov
                           :header-end header-end
                           :end-marker end-marker
                           :order block-order
                           :path path))
              (pi-coding-agent--tool-block-refresh-overlay block))))))
    (setq pi-coding-agent--pending-tool-overlay
          (pi-coding-agent--tool-block-overlay block))
    (pi-coding-agent--tool-block-register block)))

(defun pi-coding-agent--tool-block-finalize (block face)
  "Finalize BLOCK with FACE and remove it from the live keyed registry."
  (when-let* ((block block)
              (ov (pi-coding-agent--tool-block-overlay block)))
    (when-let* ((end-marker (pi-coding-agent--tool-block-end-marker block)))
      (set-marker-insertion-type end-marker nil))
    (overlay-put ov 'face face)
    (pi-coding-agent--tool-block-refresh-overlay block)
    (pi-coding-agent--tool-block-unregister block)
    (when (eq pi-coding-agent--pending-tool-overlay ov)
      (setq pi-coding-agent--pending-tool-overlay nil)))
  block)

(defun pi-coding-agent--tool-block-delete (block)
  "Delete BLOCK's text and overlay, then remove it from live state."
  (when-let* ((block block)
              (ov (pi-coding-agent--tool-block-overlay block))
              (start (overlay-start ov))
              (end (overlay-end ov)))
    (let* ((previous-tool (and (> start (point-min))
                               (seq-find (lambda (other)
                                           (overlay-get other 'pi-coding-agent-tool-block))
                                         (overlays-at (1- start)))))
           (next-tool (and (< end (point-max))
                           (seq-find (lambda (other)
                                       (overlay-get other 'pi-coding-agent-tool-block))
                                     (overlays-at (1+ end)))))
           (delete-start start)
           (delete-end end)
           (inhibit-read-only t))
      (cond
       ((and next-tool (eq (char-after end) ?\n))
        (setq delete-end (1+ end)))
       ((and previous-tool (eq (char-before start) ?\n))
        (setq delete-start (1- start))))
      (pi-coding-agent--with-scroll-preservation
        (delete-region delete-start delete-end))
      (delete-overlay ov)
      (when-let* ((header-end (pi-coding-agent--tool-block-header-end block)))
        (set-marker header-end nil))
      (when-let* ((end-marker (pi-coding-agent--tool-block-end-marker block)))
        (set-marker end-marker nil))
      (pi-coding-agent--tool-block-unregister block)
      (when (eq pi-coding-agent--pending-tool-overlay ov)
        (setq pi-coding-agent--pending-tool-overlay nil)
        (when-let* ((last-block (car (last (pi-coding-agent--live-tool-blocks-in-order)))))
          (setq pi-coding-agent--pending-tool-overlay
                (pi-coding-agent--tool-block-overlay last-block))))))
  nil)

(defun pi-coding-agent--tool-overlay-finalize (face &optional block)
  "Finalize BLOCK, or the current pending tool block, with FACE."
  (pi-coding-agent--tool-block-finalize
   (or block (pi-coding-agent--current-tool-block))
   face))

(defun pi-coding-agent--pretty-print-json (plist-data)
  "Return PLIST-DATA as a 2-space indented JSON string, or nil.
Handles the plist/vector representation from `json-parse-string'
with `:object-type \\='plist'.  Returns nil when PLIST-DATA is nil."
  (when plist-data
    (require 'json)
    ;; json-serialize is fast (C) but has no pretty-print option;
    ;; json-encode supports it, but needs alists — so we round-trip.
    (let* ((compact (json-serialize plist-data))
           (parsed (json-parse-string compact :object-type 'alist))
           (json-encoding-pretty-print t)
           (json-encoding-default-indentation "  "))
      (json-encode parsed))))

(defun pi-coding-agent--propertize-details-region (details-json)
  "Return DETAILS-JSON as a details section marked as metadata.
The details payload keeps tool-output styling while setting
`pi-coding-agent-no-fontify' so explicit markdown fontification
can safely skip this region."
  (propertize (concat "**Details**\n" details-json)
              'font-lock-face 'pi-coding-agent-tool-output
              'pi-coding-agent-no-fontify t))

(defun pi-coding-agent--tool-header (tool-name args)
  "Return propertized header for tool TOOL-NAME with ARGS.
The tool name prefix uses `pi-coding-agent-tool-name' face and
the arguments use `pi-coding-agent-tool-command' face.
Built-in tools show specialized formats (e.g., \"$ cmd\" for bash).
Generic tools show JSON args: compact when the full header fits
within `fill-column', pretty-printed otherwise.
Uses `font-lock-face' to survive tree-sitter refontification."
  (let ((path (pi-coding-agent--tool-path args)))
    (pcase tool-name
      ("bash"
       (let ((cmd (or (plist-get args :command) "...")))
         (concat (propertize "$" 'font-lock-face 'pi-coding-agent-tool-name)
                 (propertize (concat " " cmd) 'font-lock-face 'pi-coding-agent-tool-command))))
      ((or "read" "write" "edit")
       (concat (propertize tool-name 'font-lock-face 'pi-coding-agent-tool-name)
               (propertize (concat " " (or path "...")) 'font-lock-face 'pi-coding-agent-tool-command)))
      (_
       (let* ((name (propertize tool-name 'font-lock-face 'pi-coding-agent-tool-name))
              (json-pretty (pi-coding-agent--pretty-print-json args))
              (json-compact (when json-pretty
                              (mapconcat #'string-trim
                                         (split-string json-pretty "\n") " ")))
              (json (cond
                     ((null json-pretty) nil)
                     ((<= (+ (length tool-name) 1 (length json-compact))
                          fill-column)
                      json-compact)
                     (t json-pretty))))
         (if json
             (concat name (propertize (concat " " json) 'font-lock-face 'pi-coding-agent-tool-command))
           name))))))

(defun pi-coding-agent--display-tool-start (tool-name args &optional tool-call-id order)
  "Insert a tool header for TOOL-NAME with ARGS and return its live block.
When TOOL-CALL-ID is non-nil, register the block in the keyed live
registry.  ORDER records ordering metadata for future reconciliation."
  (pi-coding-agent--tool-block-create tool-name args tool-call-id order))

(defun pi-coding-agent--display-tool-update-header (tool-name args &optional block)
  "Update BLOCK's header for TOOL-NAME with ARGS.
When BLOCK is nil, fall back to the current compatibility tool block.
Replaces the header text when it has changed (e.g., when authoritative
args arrive at tool_execution_start after streaming placeholder)."
  (when-let* ((block (or block (pi-coding-agent--current-tool-block)))
              (ov (pi-coding-agent--tool-block-overlay block))
              (ov-start (overlay-start ov))
              (header-end (pi-coding-agent--tool-block-header-end block)))
    (let ((new-header (pi-coding-agent--tool-header tool-name args))
          (header-limit (1- (marker-position header-end))))
      (when (<= ov-start header-limit)
        (let ((old-header (buffer-substring-no-properties ov-start header-limit)))
          (unless (string= old-header (substring-no-properties new-header))
            (let ((inhibit-read-only t))
              (pi-coding-agent--with-scroll-preservation
                (save-excursion
                  (goto-char ov-start)
                  (delete-region ov-start header-limit)
                  (insert new-header)
                  ;; Keep HEADER-END after the preserved newline that separates
                  ;; header and body.  The marker already tracks that boundary
                  ;; across the delete/insert above; resetting it here would
                  ;; move it before the newline and glue future body content to
                  ;; the header.
                  (pi-coding-agent--tool-block-refresh-overlay block))))))))))

(defun pi-coding-agent--message-tool-calls (message)
  "Return MESSAGE toolCall content blocks in assistant source order.
Each element is a plist `(:content-index N :tool-call TOOL-CALL)'."
  (let ((content-vec (plist-get message :content))
        (tool-calls nil))
    (when (vectorp content-vec)
      (dotimes (content-index (length content-vec))
        (let ((block (aref content-vec content-index)))
          (when (equal (plist-get block :type) "toolCall")
            (push (list :content-index content-index
                        :tool-call block)
                  tool-calls)))))
    (nreverse tool-calls)))

(defun pi-coding-agent--reconcile-toolcall-preview-block (content-index tool-call)
  "Create or update the preview block for TOOL-CALL at CONTENT-INDEX."
  (let* ((tool-call-id (plist-get tool-call :id))
         (tool-name (plist-get tool-call :name))
         (args (plist-get tool-call :arguments))
         (block (or (pi-coding-agent--tool-block-get tool-call-id)
                    (pi-coding-agent--display-tool-start
                     tool-name args tool-call-id content-index))))
    (setq pi-coding-agent--pending-tool-overlay
          (pi-coding-agent--tool-block-overlay block))
    (pi-coding-agent--display-tool-update-header tool-name args block)
    (when (equal tool-name "write")
      (let ((content-entry (and args (plist-member args :content))))
        (when content-entry
          (pi-coding-agent--display-tool-streaming-text
           (or (plist-get args :content) "")
           pi-coding-agent-tool-preview-lines
           (pi-coding-agent--path-to-language
            (pi-coding-agent--tool-path args))
           block))))
    block))

(defun pi-coding-agent--prune-stale-toolcall-previews (tool-call-ids)
  "Drop keyed live preview blocks whose IDs are absent from TOOL-CALL-IDS."
  (dolist (block (pi-coding-agent--live-tool-blocks-in-order))
    (when-let* ((tool-call-id (pi-coding-agent--tool-block-tool-call-id block)))
      (unless (member tool-call-id tool-call-ids)
        (pi-coding-agent--tool-block-delete block)))))

(defun pi-coding-agent--reconcile-toolcall-previews (message)
  "Reconcile live preview blocks from assistant MESSAGE content."
  (let* ((entries (pi-coding-agent--message-tool-calls message))
         (tool-call-ids (delq nil (mapcar (lambda (entry)
                                            (plist-get (plist-get entry :tool-call) :id))
                                          entries))))
    (pi-coding-agent--prune-stale-toolcall-previews tool-call-ids)
    (dolist (entry entries)
      (pi-coding-agent--reconcile-toolcall-preview-block
       (plist-get entry :content-index)
       (plist-get entry :tool-call)))))

(defun pi-coding-agent--extract-text-from-content (content-blocks)
  "Extract text from CONTENT-BLOCKS vector efficiently.
Returns the concatenated text from all text blocks.
Optimized for the common case of a single text block."
  (if (and (vectorp content-blocks) (> (length content-blocks) 0))
      (let ((first-block (aref content-blocks 0)))
        (if (and (= (length content-blocks) 1)
                 (equal (plist-get first-block :type) "text"))
            ;; Fast path: single text block (common case)
            (or (plist-get first-block :text) "")
          ;; Slow path: multiple blocks, need to filter and concat
          (mapconcat (lambda (c)
                       (if (equal (plist-get c :type) "text")
                           (or (plist-get c :text) "")
                         ""))
                     content-blocks "")))
    ""))

(defun pi-coding-agent--extract-user-message-text (content)
  "Extract text from user message CONTENT.
CONTENT is a vector of content blocks from a user message.
Returns the concatenated text, or nil if empty."
  (let ((text (pi-coding-agent--extract-text-from-content content)))
    (unless (string-empty-p text) text)))

(defun pi-coding-agent--get-tail-lines (content n)
  "Get last N non-blank lines from CONTENT by scanning backward.
Blank lines are included in the returned content but do not count
toward N, so downstream consumers that strip blanks still get N
content lines.
Returns (TAIL-CONTENT . HAS-HIDDEN) where HAS-HIDDEN is non-nil
if there are earlier lines not included in TAIL-CONTENT.
This is O(k) where k is the size of the tail, not O(n) like `split-string'."
  (let* ((len (length content))
         (pos len)
         (newlines-found 0))
    (cond
     ((= len 0)
      (cons "" nil))
     ((<= n 0)
      (cons "" (not (string-empty-p (string-trim-right content "\n+")))))
     (t
      ;; Skip trailing newlines
      (while (and (> pos 0) (eq (aref content (1- pos)) ?\n))
        (setq pos (1- pos)))
      ;; Find N newlines from the end, skipping blank-line boundaries.
      ;; A newline at `pos' leads to a blank line when content[pos+1]
      ;; is also a newline — that boundary doesn't add a content line.
      (while (and (> pos 0) (< newlines-found n))
        (setq pos (1- pos))
        (when (and (eq (aref content pos) ?\n)
                   (not (eq (aref content (1+ pos)) ?\n)))
          (setq newlines-found (1+ newlines-found))))
      ;; Adjust pos to start after the Nth newline
      (when (and (> pos 0) (eq (aref content pos) ?\n))
        (setq pos (1+ pos)))
      ;; Return tail and whether there's hidden content
      (cons (substring content pos) (> pos 0))))))

(defun pi-coding-agent--tool-block-replace-body
    (block display-content show-hidden-indicator lang)
  "Replace BLOCK body with DISPLAY-CONTENT.
SHOW-HIDDEN-INDICATOR adds the collapsed-output hint line.
LANG is passed to `pi-coding-agent--wrap-in-src-block' for fence construction."
  (when-let* ((block block)
              (header-end (pi-coding-agent--tool-block-header-end block))
              (end-marker (pi-coding-agent--tool-block-end-marker block)))
    (let ((inhibit-read-only t))
      (pi-coding-agent--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position header-end))
          (delete-region (marker-position header-end)
                         (marker-position end-marker))
          (when show-hidden-indicator
            (insert (propertize "... (earlier output)\n"
                                'face
                                'pi-coding-agent-collapsed-indicator)))
          (unless (string-empty-p display-content)
            (insert (pi-coding-agent--wrap-in-src-block
                     display-content lang)
                    "\n"))
          (set-marker end-marker (point))
          (pi-coding-agent--tool-block-refresh-overlay block))))))

(defun pi-coding-agent--display-tool-streaming-text
    (raw-text max-lines &optional lang block)
  "Display RAW-TEXT as streaming content in BLOCK.
Shows a rolling tail truncated to MAX-LINES visual lines.
When BLOCK is nil, fall back to the current compatibility tool block.

When LANG is non-nil, wrap the tail in a markdown fenced code block so
that `md-ts-mode' language injection handles syntax highlighting.
Skips redraw when only the trailing partial line changed (the preview
shows complete lines only)."
  (when (stringp raw-text)
    (when-let* ((block (or block (pi-coding-agent--current-tool-block))))
      (let* (;; For language-aware streaming, only show complete lines
             ;; (exclude trailing partial line) to keep the preview
             ;; stable across partial-token deltas.
             (complete-text
              (if (and lang (not (string-suffix-p "\n" raw-text)))
                  (let ((last-nl (cl-position ?\n raw-text :from-end t)))
                    (if last-nl (substring raw-text 0 (1+ last-nl)) ""))
                raw-text))
             (tail-result (pi-coding-agent--get-tail-lines complete-text max-lines))
             (tail-content (or (car tail-result) ""))
             (has-hidden (cdr tail-result))
             (truncation (pi-coding-agent--truncate-to-visual-lines
                          tail-content max-lines
                          (pi-coding-agent--chat-display-width)))
             (display-content
              (string-trim-right
               (or (plist-get truncation :content) "")
               "\n+"))
             (show-hidden-indicator
              (or has-hidden
                  (> (plist-get truncation :hidden-lines) 0)))
             (cache-key (if show-hidden-indicator
                            (concat "H:" display-content)
                          display-content))
             (last-tail (pi-coding-agent--tool-block-last-tail block)))
        (unless (equal cache-key last-tail)
          (pi-coding-agent--tool-block-replace-body
           block display-content show-hidden-indicator lang)
          (pi-coding-agent--tool-block-set-last-tail block cache-key))))))

(defun pi-coding-agent--display-tool-update (partial-result &optional block)
  "Display PARTIAL-RESULT as streaming output in BLOCK.
When BLOCK is nil, fall back to the current compatibility tool block.
PARTIAL-RESULT has the same structure as a tool result plist with
`:content'.  Extracts text from content blocks and delegates to
`pi-coding-agent--display-tool-streaming-text'."
  (when partial-result
    (let* ((content-blocks (plist-get partial-result :content))
           (raw-output (pi-coding-agent--extract-text-from-content content-blocks)))
      (pi-coding-agent--display-tool-streaming-text
       raw-output pi-coding-agent-bash-preview-lines nil block))))

(defun pi-coding-agent--markdown-fence-delimiter (content)
  "Return a markdown fence delimiter safe for CONTENT.
Uses triple backticks by default.  If CONTENT contains triple-backtick
runs, uses a tilde fence longer than any tilde run in CONTENT."
  (let ((text (or content "")))
    (if (string-match-p "```+" text)
        (let ((max-tilde-run 0)
              (pos 0))
          (while (string-match "~+" text pos)
            (setq max-tilde-run
                  (max max-tilde-run
                       (- (match-end 0) (match-beginning 0))))
            (setq pos (match-end 0)))
          (make-string (max 3 (1+ max-tilde-run)) ?~))
      "```")))

(defun pi-coding-agent--wrap-in-src-block (content lang)
  "Wrap CONTENT in a markdown fenced code block with LANG.
Returns markdown string for syntax highlighting."
  (let ((fence (pi-coding-agent--markdown-fence-delimiter content)))
    (format "%s%s\n%s\n%s" fence (or lang "") content fence)))

(defun pi-coding-agent--display-tool-end
    (tool-name args content details is-error &optional block)
  "Display result for TOOL-NAME and finalize BLOCK.
ARGS contains tool arguments, CONTENT is a list of content blocks.
DETAILS contains tool-specific data (e.g., a diff for the edit tool);
for generic tools, non-nil DETAILS are rendered below the content.
IS-ERROR indicates failure.
When BLOCK is nil, fall back to the current compatibility tool block and,
if none exists, render the result at point without a live overlay."
  (let* ((block (or block (pi-coding-agent--current-tool-block)))
         (is-error (eq t is-error))
         (text-blocks (seq-filter (lambda (c) (equal (plist-get c :type) "text"))
                                  content))
         (raw-output (mapconcat (lambda (c) (or (plist-get c :text) ""))
                                text-blocks "\n"))
         ;; Determine language for syntax highlighting
         (lang (when-let* ((path (pi-coding-agent--tool-path args)))
                 (pi-coding-agent--path-to-language path)))
         ;; For edit tool with diff, we'll apply diff overlays after insertion
         (is-edit-diff (and (equal tool-name "edit")
                            (not is-error)
                            (plist-get details :diff)))
         (display-content
          (ansi-color-filter-apply
           (pcase tool-name
             ("edit" (or (plist-get details :diff) raw-output))
             ("write" (or (plist-get args :content) raw-output))
             ((or "bash" "read") raw-output)
             (_ (if-let* ((details-json
                          (pi-coding-agent--pretty-print-json details)))
                    (concat raw-output "\n\n"
                            (pi-coding-agent--propertize-details-region
                             details-json))
                  raw-output)))))
         (preview-limit (pcase tool-name
                          ("bash" pi-coding-agent-bash-preview-lines)
                          (_ pi-coding-agent-tool-preview-lines)))
         ;; Use visual line truncation with byte limit
         (width (pi-coding-agent--chat-display-width))
         (truncation (pi-coding-agent--truncate-to-visual-lines
                      display-content preview-limit width))
         (hidden-count (plist-get truncation :hidden-lines))
         (needs-collapse (> hidden-count 0))
         (inhibit-read-only t))
    (pi-coding-agent--with-scroll-preservation
      (save-excursion
        (if block
            (let* ((header-end (pi-coding-agent--tool-block-header-end block))
                   (end-marker (pi-coding-agent--tool-block-end-marker block)))
              (goto-char (marker-position header-end))
              (delete-region (marker-position header-end)
                             (marker-position end-marker))
              (if needs-collapse
                  ;; Long output: show preview with toggle button.
                  (let ((preview-content (plist-get truncation :content)))
                    (pi-coding-agent--insert-tool-content-with-toggle
                     preview-content display-content lang is-edit-diff hidden-count nil))
                ;; Short output: show all without toggle.
                (pi-coding-agent--insert-rendered-tool-content
                 (string-trim-right display-content "\n+")
                 lang
                 is-edit-diff))
              (set-marker end-marker (point))
              (pi-coding-agent--tool-block-refresh-overlay block)
              ;; Note: no [error] badge — error content in the block is sufficient,
              ;; and the overlay face already shifts to pi-coding-agent-tool-block-error.
              (when (and (equal tool-name "read")
                         (plist-get args :offset))
                (pi-coding-agent--tool-block-set-offset
                 block (plist-get args :offset)))
              (when-let* ((line-map (plist-get truncation :line-map)))
                (pi-coding-agent--tool-block-set-line-map block line-map))
              (pi-coding-agent--tool-overlay-finalize
               (if is-error 'pi-coding-agent-tool-block-error
                 'pi-coding-agent-tool-block)
               block)
              (when (eobp)
                (insert "\n")))
          (progn
            (goto-char (point-max))
            (if needs-collapse
                (let ((preview-content (plist-get truncation :content)))
                  (pi-coding-agent--insert-tool-content-with-toggle
                   preview-content display-content lang is-edit-diff hidden-count nil))
              (pi-coding-agent--insert-rendered-tool-content
               (string-trim-right display-content "\n+")
               lang
               is-edit-diff))
            (insert "\n")))))))

(defun pi-coding-agent--ranges-excluding-property (start end prop)
  "Return contiguous ranges in START..END where PROP is nil."
  (let ((pos start)
        (ranges nil))
    (while (< pos end)
      (let* ((excluded (get-text-property pos prop))
             (next (or (next-single-property-change pos prop nil end)
                       end)))
        (unless excluded
          (push (cons pos next) ranges))
        (setq pos next)))
    (nreverse ranges)))

(defun pi-coding-agent--font-lock-ensure-excluding-property (start end prop)
  "Fontify START..END while skipping regions where PROP is non-nil.
Stops after the first font-lock error to avoid repeated failures."
  (catch 'pi-coding-agent--font-lock-failed
    (dolist (range (pi-coding-agent--ranges-excluding-property start end prop))
      (condition-case err
          (font-lock-ensure (car range) (cdr range))
        (error
         (when debug-on-error
           (message "pi-coding-agent: toggle fontification failed: %S" err))
         (throw 'pi-coding-agent--font-lock-failed nil))))))

(defun pi-coding-agent--toggle-tool-output (button)
  "Toggle between preview and full content for BUTTON.
Preserves window scroll position during the toggle."
  (let* ((inhibit-read-only t)
         (expanded (button-get button 'pi-coding-agent-expanded))
         (full-content (button-get button 'pi-coding-agent-full-content))
         (preview-content (button-get button 'pi-coding-agent-preview-content))
         (lang (button-get button 'pi-coding-agent-lang))
         (is-edit-diff (button-get button 'pi-coding-agent-is-edit-diff))
         (hidden-count (button-get button 'hidden-count))
         (btn-start (button-start button))
         (btn-end (button-end button)))
    (save-excursion
      ;; Find the tool overlay
      (goto-char btn-start)
      (when-let* ((bounds (pi-coding-agent--find-tool-block-bounds))
                  (ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block))
                                (overlays-at (point))))
                  (header-end (overlay-get ov 'pi-coding-agent-header-end)))
        ;; Save window positions relative to content-start
        ;; Windows before the tool block: save absolute position
        ;; Windows inside tool block: will use header position after toggle
        (let* ((content-start header-end)
               (block-start (car bounds))
               (saved-windows
                (mapcar (lambda (w)
                          (let ((ws (window-start w)))
                            (list w ws (window-point w)
                                  ;; Flag: was window-start before content area?
                                  (< ws content-start))))
                        (get-buffer-window-list (current-buffer) nil t))))
          ;; Delete from content start to after button
          (delete-region content-start (1+ btn-end))
          (goto-char content-start)
          ;; Toggle: if currently expanded, show collapsed (and vice versa)
          (pi-coding-agent--insert-tool-content-with-toggle
           preview-content full-content lang is-edit-diff hidden-count (not expanded))
          ;; Ensure fontification of inserted content (JIT font-lock is lazy)
          ;; while excluding metadata-like details payload.
          (pi-coding-agent--font-lock-ensure-excluding-property
           content-start (point) 'pi-coding-agent-no-fontify)
          ;; Update overlay to include new content
          (move-overlay ov block-start (point))
          ;; Restore window positions
          (dolist (win-state saved-windows)
            (let ((win (nth 0 win-state))
                  (old-start (nth 1 win-state))
                  (old-point (nth 2 win-state))
                  (was-before-content (nth 3 win-state)))
              (when (window-live-p win)
                (if was-before-content
                    ;; Window was before tool content - restore exactly
                    (progn
                      (set-window-start win old-start t)
                      (set-window-point win (min old-point (point-max))))
                  ;; Window was inside tool content - show from block start
                  (set-window-start win block-start t)
                  (set-window-point win block-start))))))))))

(defun pi-coding-agent--insert-rendered-tool-content (content lang is-edit-diff)
  "Insert CONTENT rendered for LANG with a trailing newline.
When IS-EDIT-DIFF is non-nil, apply diff overlays to the inserted block."
  (let ((content-start (point)))
    (insert (pi-coding-agent--wrap-in-src-block content lang) "\n")
    (when is-edit-diff
      (pi-coding-agent--apply-diff-overlays content-start (point)))))

(defun pi-coding-agent--tool-hidden-line-label (hidden-count)
  "Return the plain display label for HIDDEN-COUNT hidden lines."
  (format "... (%d more lines)" hidden-count))

(defun pi-coding-agent--insert-tool-content-with-toggle
    (preview-content full-content lang is-edit-diff hidden-count expanded)
  "Insert tool content with a toggle button.
When EXPANDED is nil, shows PREVIEW-CONTENT with expand button.
When EXPANDED is non-nil, shows FULL-CONTENT with collapse button.
LANG is for syntax highlighting.  IS-EDIT-DIFF applies diff overlays.
HIDDEN-COUNT is stored for the button label."
  (let* ((display-content (if expanded
                              (string-trim-right full-content "\n+")
                            preview-content))
         (button-label (if expanded
                           "[-]"
                         (pi-coding-agent--tool-hidden-line-label hidden-count))))
    (pi-coding-agent--insert-rendered-tool-content
     display-content
     lang
     is-edit-diff)
    (insert-text-button
     (propertize button-label 'face 'pi-coding-agent-collapsed-indicator)
     'action #'pi-coding-agent--toggle-tool-output
     'follow-link t
     'pi-coding-agent-full-content full-content
     'pi-coding-agent-preview-content preview-content
     'pi-coding-agent-lang lang
     'pi-coding-agent-is-edit-diff is-edit-diff
     'pi-coding-agent-expanded expanded
     'hidden-count hidden-count)
    (insert "\n")))

(defun pi-coding-agent--find-tool-block-bounds ()
  "Find the bounds of the tool block at point.
Returns (START . END) if inside a tool block, nil otherwise."
  (let ((overlays (overlays-at (point))))
    (when-let* ((ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block)) overlays)))
      (cons (overlay-start ov) (overlay-end ov)))))

(defun pi-coding-agent--find-toggle-button-in-region (start end)
  "Find a toggle button between START and END."
  (save-excursion
    (goto-char start)
    (let ((found nil))
      (while (and (not found) (< (point) end))
        (let ((btn (button-at (point))))
          (if (and btn (button-get btn 'pi-coding-agent-full-content))
              (setq found btn)
            (forward-char 1))))
      found)))

(defun pi-coding-agent-toggle-tool-section ()
  "Toggle the tool output section at point.
Works anywhere inside a tool block overlay."
  (interactive)
  (let ((original-pos (point)))
    (if-let* ((bounds (pi-coding-agent--find-tool-block-bounds)))
        (if-let* ((btn (pi-coding-agent--find-toggle-button-in-region (car bounds) (cdr bounds))))
            (progn
              (pi-coding-agent--toggle-tool-output btn)
              ;; Try to restore position, clamped to new block bounds.
              ;; Use (1- end) because overlays-at uses half-open [start, end),
              ;; so clamping to exactly end would place cursor outside the
              ;; overlay, breaking the next toggle.
              (when-let* ((new-bounds (pi-coding-agent--find-tool-block-bounds)))
                (goto-char (min original-pos (1- (cdr new-bounds))))))
          ;; No button found - short output, use outline-cycle
          (outline-cycle))
      ;; Not in a tool block
      (outline-cycle))))

;;;; Tool Block Cooling
;;
;; Completed tool blocks outside the hot tail (older than the most
;; recent `pi-coding-agent-hot-tail-turn-count' headed turns) are
;; cooled into plain text.  The cold form keeps the header and visible
;; preview but drops overlays, buttons, and syntax-tagged rendering.

(defun pi-coding-agent--tool-overlay-live-p (overlay)
  "Return non-nil when OVERLAY's tool block is still in the live registry."
  (when-let* ((rec (overlay-get overlay 'pi-coding-agent-tool-block-record))
              (id (pi-coding-agent--tool-block-tool-call-id rec)))
    (pi-coding-agent--tool-block-get id)))

(defun pi-coding-agent--completed-tool-overlay-p (overlay)
  "Return non-nil when OVERLAY is a completed (finalized) tool block.
Live tool blocks that are still executing are excluded."
  (and (overlayp overlay)
       (overlay-buffer overlay)
       (overlay-get overlay 'pi-coding-agent-tool-block)
       (not (eq overlay pi-coding-agent--pending-tool-overlay))
       (not (pi-coding-agent--tool-overlay-live-p overlay))
       (overlay-get overlay 'pi-coding-agent-header-end)))

(defun pi-coding-agent--tool-overlay-visible-body (overlay)
  "Return the currently visible body text for completed tool OVERLAY.
Extracts the text between the outer fence lines, removing only the
wrapper newline inserted before the closing fence.  Collapsed blocks
therefore cool into their visible preview only."
  (when-let* ((header-end-marker (overlay-get overlay 'pi-coding-agent-header-end))
              (header-end (and (markerp header-end-marker)
                               (marker-position header-end-marker)))
              (overlay-end (overlay-end overlay)))
    (save-excursion
      (goto-char header-end)
      (when-let* ((opening-fence (pi-coding-agent--fence-line-info-at-point)))
        (forward-line 1)
        (let ((content-start (point))
              (closing-start nil))
          (while (and (not closing-start) (< (point) overlay-end))
            (let ((line-info (pi-coding-agent--fence-line-info-at-point)))
              (when (pi-coding-agent--fence-closing-line-p opening-fence line-info)
                (setq closing-start (line-beginning-position))))
            (unless closing-start
              (forward-line 1)))
          (when closing-start
            (let ((wrapped-body (buffer-substring-no-properties
                                 content-start closing-start)))
              (if (string-suffix-p "\n" wrapped-body)
                  (substring wrapped-body 0 -1)
                wrapped-body))))))))

(defun pi-coding-agent--tool-overlay-cold-metadata (overlay)
  "Return cold-history metadata for completed tool OVERLAY.
The result is a plist with `:visible-body' and, for currently collapsed
blocks only, `:hidden-count'.  Expanded blocks return no hidden-count
because cold history must stay preview-only."
  (when-let* ((visible-body (pi-coding-agent--tool-overlay-visible-body overlay))
              (header-end (marker-position
                           (overlay-get overlay 'pi-coding-agent-header-end))))
    (let* ((button (pi-coding-agent--find-toggle-button-in-region
                    header-end (overlay-end overlay)))
           (hidden-count (and button
                              (not (button-get button 'pi-coding-agent-expanded))
                              (button-get button 'hidden-count))))
      (list :visible-body visible-body
            :hidden-count (and (integerp hidden-count)
                               (> hidden-count 0)
                               hidden-count)))))

(defun pi-coding-agent--cool-tool-overlay (overlay)
  "Rewrite completed tool OVERLAY into its cold plain-history form.
Preserves the header and visible preview, drops overlays, buttons, and
diff annotations."
  (when (pi-coding-agent--completed-tool-overlay-p overlay)
    (when-let* ((metadata (pi-coding-agent--tool-overlay-cold-metadata overlay))
                (visible-body (plist-get metadata :visible-body))
                (header-end (marker-position
                             (overlay-get overlay 'pi-coding-agent-header-end))))
      (let* ((inhibit-read-only t)
             (hidden-count (plist-get metadata :hidden-count))
             (cold-body (concat
                         (pi-coding-agent--wrap-in-src-block visible-body nil)
                         "\n"
                         (when hidden-count
                           (concat (pi-coding-agent--tool-hidden-line-label hidden-count)
                                   "\n"))))
             (ov-start (overlay-start overlay))
             (ov-end (overlay-end overlay)))
        (remove-overlays ov-start ov-end 'pi-coding-agent-diff-overlay t)
        (delete-overlay overlay)
        (save-excursion
          (goto-char header-end)
          (delete-region header-end ov-end)
          (insert cold-body))
        t))))

(defun pi-coding-agent--cool-completed-tool-blocks (overlays)
  "Cool the given completed tool OVERLAYS.
Blocks are processed from the end of the buffer backward so region
rewrites do not disturb remaining candidates."
  (let* ((candidates (seq-filter #'pi-coding-agent--completed-tool-overlay-p
                                 overlays))
         (sorted (sort (copy-sequence candidates)
                       (lambda (a b)
                         (> (overlay-start a) (overlay-start b))))))
    (pi-coding-agent--with-scroll-preservation
      (save-excursion
        (dolist (overlay sorted)
          (pi-coding-agent--cool-tool-overlay overlay))))))

(defun pi-coding-agent--cool-completed-tool-blocks-outside-hot-tail ()
  "Cool completed tool blocks that fall before the hot-tail boundary.
Uses the same `pi-coding-agent--hot-tail-start' marker that tables
use for resize scope, so there is one unified hot-tail concept."
  (when (and (markerp pi-coding-agent--hot-tail-start)
             (> (marker-position pi-coding-agent--hot-tail-start) (point-min)))
    (let* ((boundary (marker-position pi-coding-agent--hot-tail-start))
           (cold-overlays
            (seq-filter (lambda (ov)
                          (and (pi-coding-agent--completed-tool-overlay-p ov)
                               (< (overlay-start ov) boundary)))
                        (overlays-in (point-min) boundary))))
      (when cold-overlays
        (pi-coding-agent--cool-completed-tool-blocks cold-overlays)))))

;;;; File Navigation

(defun pi-coding-agent--diff-line-at-point ()
  "Extract line number from diff line at point.
Returns the line number if point is on an added, removed, or context
line, nil otherwise.
Diff format: [+- ] LINENUM content.
For example: '+ 7     code', '-12     code', or '  9     context'."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[ +-] *\\([0-9]+\\)\\(?:[ \t]+\\|$\\)")
      (string-to-number (match-string 1)))))

(defun pi-coding-agent--fence-line-info-at-point ()
  "Return fence info for current line, or nil when not on a fence line.
Return value is plist `(:char CHAR :len LEN :trailing TEXT)'.
TEXT is everything after the fence run on this line.
Only recognizes fences indented by at most three spaces."
  (save-excursion
    (beginning-of-line)
    (let ((indent-start (point)))
      (skip-chars-forward " ")
      (let ((indent (- (point) indent-start))
            (char (char-after)))
        (when (and (<= indent 3)
                   (memq char '(?` ?~)))
          (let ((start (point)))
            (skip-chars-forward (char-to-string char))
            (let ((len (- (point) start)))
              (when (>= len 3)
                (list :char char
                      :len len
                      :trailing (buffer-substring-no-properties
                                 (point) (line-end-position)))))))))))

(defun pi-coding-agent--fence-closing-line-p (fence line-info)
  "Return non-nil when LINE-INFO closes FENCE.
FENCE and LINE-INFO are plists from
`pi-coding-agent--fence-line-info-at-point'."
  (and line-info
       (= (plist-get line-info :char) (plist-get fence :char))
       (>= (plist-get line-info :len) (plist-get fence :len))
       (string-match-p "^[ \t]*$" (plist-get line-info :trailing))))

(defun pi-coding-agent--code-block-line-at-point (&optional start-pos)
  "Return line number within code block content at point.
Supports both backtick and tilde fenced blocks.  Returns nil unless
point is on a content line inside an open fenced block.
When START-POS is non-nil, parse fences starting from that position."
  (save-excursion
    (let* ((target-line (line-number-at-pos))
           (scan-start (or start-pos (point-min)))
           (open-fence nil)
           (open-line nil)
           (result nil)
           line-no)
      (goto-char scan-start)
      (beginning-of-line)
      (setq line-no (line-number-at-pos))
      (while (and (<= line-no target-line) (not result))
        (let ((line-info (pi-coding-agent--fence-line-info-at-point)))
          (cond
           (open-fence
            (cond
             ((pi-coding-agent--fence-closing-line-p open-fence line-info)
              (setq open-fence nil)
              (setq open-line nil))
             ((= line-no target-line)
              (setq result (- line-no open-line)))))
           (line-info
            (setq open-fence line-info)
            (setq open-line line-no))))
        (forward-line 1)
        (setq line-no (1+ line-no)))
      result)))

(defun pi-coding-agent--tool-overlay-collapsed-p (overlay)
  "Return non-nil when OVERLAY is currently collapsed.
Collapsed tool blocks contain a toggle button whose
`pi-coding-agent-expanded' property is nil.  Expanded blocks and
non-collapsible blocks return nil."
  (when-let* ((btn (pi-coding-agent--find-toggle-button-in-region
                   (overlay-start overlay)
                   (overlay-end overlay))))
    (not (button-get btn 'pi-coding-agent-expanded))))

(defun pi-coding-agent--tool-line-at-point (overlay)
  "Calculate file line number at point for tool OVERLAY.
For edit diffs: parse line number from added, removed, or context rows.
For read/write: use line-map only in collapsed preview mode; otherwise
count directly from the rendered code block."
  (let* ((tool-name (overlay-get overlay 'pi-coding-agent-tool-name))
         (offset (if (equal tool-name "read")
                     (or (overlay-get overlay 'pi-coding-agent-tool-offset) 1)
                   1))
         (line-map (overlay-get overlay 'pi-coding-agent-line-map))
         (header-end (overlay-get overlay 'pi-coding-agent-header-end))
         (use-line-map (and line-map
                            header-end
                            (pi-coding-agent--tool-overlay-collapsed-p overlay))))
    (if (equal tool-name "edit")
        ;; Edit navigation uses the explicit line number in diff rows.
        (pi-coding-agent--diff-line-at-point)
      (or
       ;; Collapsed preview strips blank lines, so use line-map there.
       (when use-line-map
         (save-excursion
           (let* ((current-line (line-number-at-pos))
                  (header-line (line-number-at-pos header-end))
                  (lines-from-header (- current-line header-line))
                  (map-index (1- lines-from-header)))
             (when (and (>= map-index 0) (< map-index (length line-map)))
               (+ (aref line-map map-index) (1- offset))))))
       ;; Expanded/full output preserves blank lines: derive from code block.
       (when-let* ((block-line (pi-coding-agent--code-block-line-at-point header-end)))
         (+ block-line (1- offset)))))))

(defun pi-coding-agent-visit-file (&optional toggle)
  "Visit the file associated with the tool block at point.
If on a diff line, go to the corresponding line number.
For read/write, go to the line within the displayed content.
By default, uses `pi-coding-agent-visit-file-other-window' to decide
whether to open in another window.  With prefix arg TOGGLE, invert
that behavior."
  (interactive "P")
  (if-let* ((ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block))
                          (overlays-at (point))))
            (path (overlay-get ov 'pi-coding-agent-tool-path)))
      (if-let* ((line (pi-coding-agent--tool-line-at-point ov)))
          (let ((use-other-window (if toggle
                                      (not pi-coding-agent-visit-file-other-window)
                                    pi-coding-agent-visit-file-other-window)))
            (funcall (if use-other-window #'find-file-other-window #'find-file) path)
            (goto-char (point-min))
            (forward-line (1- line)))
        (user-error "No file line at point"))
    (user-error "No file at point")))

;;;; Diff Overlay Highlighting

;; Overlay priorities determine stacking order (higher = on top)
;; Tool-block overlay has no priority (defaults to 0)
(defconst pi-coding-agent--diff-line-priority 10
  "Priority for diff line background overlays.
Higher than tool-block (0) so diff colors show through.")

(defconst pi-coding-agent--diff-indicator-priority 20
  "Priority for diff indicator (+/-) overlays.
Higher than line background so indicator face isn't obscured.")

(defun pi-coding-agent--apply-diff-overlays (start end)
  "Apply diff highlighting overlays to region from START to END.
Scans for lines starting with +/- and applies diff faces via overlays.
Overlays survive font-lock refontification, unlike text properties.
The diff format from pi is: [+-]<space><padded-line-number><space><code>
For example: '+ 7     code' or '-12     code'"
  (save-excursion
    (goto-char start)
    (while (re-search-forward "^\\([+-]\\) *\\([0-9]+\\)" end t)
      (let* ((indicator (match-string 1))
             (is-added (string= indicator "+"))
             (indicator-start (match-beginning 1))
             (line-end (line-end-position))
             ;; Overlay for the indicator character
             (ind-ov (make-overlay indicator-start (match-end 1)))
             ;; Overlay for the rest of the line (background color)
             (line-ov (make-overlay (match-beginning 1) line-end)))
        ;; Indicator face (+/-) - highest priority to show on top
        (overlay-put ind-ov 'face (if is-added
                                      'diff-indicator-added
                                    'diff-indicator-removed))
        (overlay-put ind-ov 'priority pi-coding-agent--diff-indicator-priority)
        (overlay-put ind-ov 'pi-coding-agent-diff-overlay t)
        ;; Line background face - higher than tool-block but lower than indicator
        (overlay-put line-ov 'face (if is-added 'diff-added 'diff-removed))
        (overlay-put line-ov 'priority pi-coding-agent--diff-line-priority)
        (overlay-put line-ov 'pi-coding-agent-diff-overlay t)))))

;;;; Compaction Display

(defun pi-coding-agent--display-compaction-result (tokens-before summary &optional timestamp)
  "Display a compaction result block in the chat buffer.
TOKENS-BEFORE is the token count before compaction.
SUMMARY is the compaction summary text (markdown).
TIMESTAMP is optional time when compaction occurred."
  (let ((start (with-current-buffer (pi-coding-agent--get-chat-buffer) (point-max))))
    (pi-coding-agent--append-to-chat
     (concat "\n" (pi-coding-agent--make-separator "Compaction" timestamp) "\n"
             (propertize (format "Compacted from %s tokens\n\n"
                                 (pi-coding-agent--format-number (or tokens-before 0)))
                         'face 'pi-coding-agent-tool-name)
             (or summary "") "\n"))
    (with-current-buffer (pi-coding-agent--get-chat-buffer)
      (pi-coding-agent--decorate-tables-in-region start (point-max)))))

(defun pi-coding-agent--handle-compaction-success (tokens-before summary &optional timestamp)
  "Handle successful compaction: display result, reset state, notify user.
TOKENS-BEFORE is the pre-compaction token count.
SUMMARY is the compaction summary text.
TIMESTAMP is optional time when compaction occurred."
  (pi-coding-agent--display-compaction-result tokens-before summary timestamp)
  (pi-coding-agent--set-last-usage nil)
  (pi-coding-agent--refresh-header)
  (message "Pi: Compacted from %s tokens" (pi-coding-agent--format-number (or tokens-before 0))))

(defun pi-coding-agent--render-complete-message ()
  "Finalize completed message: ensure trailing newline, decorate tables.
Uses message-start-marker and streaming-marker to find content.
No explicit fontification needed — jit-lock + tree-sitter fontify
at each redisplay cycle during streaming, and any remaining gaps
are fontified at the redisplay after this function returns.
Display-only table decoration is applied after the content is stable."
  (when (and pi-coding-agent--message-start-marker pi-coding-agent--streaming-marker)
    (let ((start (marker-position pi-coding-agent--message-start-marker))
          (end (marker-position pi-coding-agent--streaming-marker)))
      (when (< start end)
        (let ((inhibit-read-only t))
          (pi-coding-agent--with-scroll-preservation
            (save-excursion
              (goto-char end)
              (unless (eq (char-before) ?\n)
                (insert "\n")
                (set-marker pi-coding-agent--streaming-marker (point))))))
        (if (pi-coding-agent--chat-buffer-hidden-p)
            (setq pi-coding-agent--table-decoration-pending t)
          (pi-coding-agent--decorate-tables-in-region
           start (marker-position pi-coding-agent--streaming-marker)))))))

;;;; Tool Property Restoration

(defun pi-coding-agent--restore-tool-properties (beg end)
  "Restore tool header faces after tree-sitter fontification in BEG..END.
Tree-sitter markdown applies `invisible' and `face' properties to markup
patterns in tool headers (for example, `$ echo **hello**').  This strips
that markdown damage and restores the intended `font-lock-face' values for
all overlapping tool headers, live or finalized."
  (let ((inhibit-read-only t))
    (dolist (ov (pi-coding-agent--tool-block-overlays-in-region beg end))
      (when-let* ((ov-start (overlay-start ov))
                  (ov-end (overlay-end ov))
                  (header-end-marker (overlay-get ov 'pi-coding-agent-header-end))
                  (header-end (marker-position header-end-marker)))
        (when (and (< beg ov-end) (> end ov-start))
          ;; Header: restore face from font-lock-face (varies per span)
          (let ((hdr-beg (max beg ov-start))
                (hdr-end (min end header-end)))
            (when (< hdr-beg hdr-end)
              (remove-text-properties
               hdr-beg hdr-end
               '(invisible nil))
              (let ((pos hdr-beg))
                (while (< pos hdr-end)
                  (let* ((fl-face (get-text-property pos 'font-lock-face))
                         (next (or (next-single-property-change
                                    pos 'font-lock-face nil hdr-end)
                                   hdr-end)))
                    (when fl-face
                      (put-text-property pos next 'face fl-face))
                    (setq pos next))))
              (put-text-property hdr-beg hdr-end 'fontified t))))))))

;;;; History Display

(defun pi-coding-agent--extract-message-text (message)
  "Extract plain text content from MESSAGE.
MESSAGE is a plist with :role and :content.
Returns concatenated text from all text blocks."
  (let* ((content (plist-get message :content))
         (texts '()))
    (when (vectorp content)
      (dotimes (i (length content))
        (let* ((block (aref content i))
               (block-type (plist-get block :type)))
          (when (equal block-type "text")
            (push (plist-get block :text) texts)))))
    (string-join (nreverse texts) "")))

(defun pi-coding-agent--build-tool-result-index (messages)
  "Build hash-table mapping toolCallId to toolResult message from MESSAGES."
  (let ((index (make-hash-table :test 'equal)))
    (when (vectorp messages)
      (dotimes (i (length messages))
        (let ((msg (aref messages i)))
          (when (equal (plist-get msg :role) "toolResult")
            (puthash (plist-get msg :toolCallId) msg index)))))
    index))

(defun pi-coding-agent--render-history-text (text)
  "Render TEXT as markdown content with proper isolation.
Ensures markdown structures don't leak to subsequent content.
Display-only table decoration is applied after fontification."
  (when (and text (not (string-empty-p text)))
    (let ((start (with-current-buffer (pi-coding-agent--get-chat-buffer) (point-max))))
      (pi-coding-agent--append-to-chat text)
      (with-current-buffer (pi-coding-agent--get-chat-buffer)
        (font-lock-ensure start (point-max))
        (pi-coding-agent--decorate-tables-in-region start (point-max)))
      ;; Two trailing newlines reset any open markdown list/paragraph context
      (pi-coding-agent--append-to-chat "\n\n"))))

(defun pi-coding-agent--render-history-tool (tool-call result)
  "Render a single tool from history: TOOL-CALL block with its RESULT.
TOOL-CALL is a content block plist with :type \"toolCall\", :id, :name,
and :arguments.  RESULT is the matching toolResult message, or nil."
  (let ((tool-name (plist-get tool-call :name))
        (args (plist-get tool-call :arguments)))
    (pi-coding-agent--display-tool-start tool-name args)
    (if result
        (pi-coding-agent--display-tool-end
         tool-name args
         (plist-get result :content)
         (plist-get result :details)
         (plist-get result :isError))
      (pi-coding-agent--tool-overlay-finalize 'pi-coding-agent-tool-block)
      (let ((inhibit-read-only t))
        (save-excursion (goto-char (point-max)) (insert "\n"))))))

(defun pi-coding-agent--display-history-messages (messages)
  "Display MESSAGES from session history with full tool rendering.
Consecutive assistant messages are grouped under one header.
Tool calls are rendered with headers, output, overlays, and toggles."
  (let ((prev-role nil)
        (results (pi-coding-agent--build-tool-result-index messages)))
    (dotimes (i (length messages))
      (let* ((message (aref messages i))
             (role (plist-get message :role)))
        (pcase role
          ("user"
           (let* ((text (pi-coding-agent--extract-message-text message))
                  (timestamp (pi-coding-agent--ms-to-time (plist-get message :timestamp))))
             (when (and text (not (string-empty-p text)))
               (pi-coding-agent--display-user-message text timestamp)))
           (setq prev-role "user"))
          ("assistant"
           (when (not (equal prev-role "assistant"))
             (pi-coding-agent--append-to-chat
              (concat "\n" (pi-coding-agent--make-separator "Assistant") "\n")))
           (let* ((content (plist-get message :content))
                  (text (pi-coding-agent--extract-message-text message)))
             (when (and text (not (string-empty-p text)))
               (pi-coding-agent--render-history-text text))
             ;; Render each tool call with its result
             (when (vectorp content)
               (dotimes (j (length content))
                 (let ((block (aref content j)))
                   (when (equal (plist-get block :type) "toolCall")
                     (pi-coding-agent--render-history-tool
                      block (gethash (plist-get block :id) results)))))))
           (setq prev-role "assistant"))
          ("compactionSummary"
           (let* ((summary (plist-get message :summary))
                  (tokens-before (plist-get message :tokensBefore))
                  (timestamp (pi-coding-agent--ms-to-time (plist-get message :timestamp))))
             (pi-coding-agent--display-compaction-result tokens-before summary timestamp))
           (setq prev-role "compactionSummary"))
          ("toolResult"
           nil))))))

(defun pi-coding-agent--display-session-history (messages &optional chat-buf)
  "Display session history MESSAGES in the chat buffer.
MESSAGES is a vector of message plists from get_messages RPC.
CHAT-BUF is the target buffer; if nil, uses `pi-coding-agent--get-chat-buffer'.
Note: When called from async callbacks, pass CHAT-BUF explicitly."
  (setq chat-buf (or chat-buf (pi-coding-agent--get-chat-buffer)))
  (when (and chat-buf (buffer-live-p chat-buf))
    (with-current-buffer chat-buf
      (let ((inhibit-read-only t))
        (pi-coding-agent--clear-render-artifacts)
        (erase-buffer)
        (insert (pi-coding-agent--format-startup-header) "\n")
        (when (vectorp messages)
          (pi-coding-agent--display-history-messages messages))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (pi-coding-agent--set-message-start-marker nil)
        (pi-coding-agent--set-streaming-marker nil)
        (pi-coding-agent--update-hot-tail-boundary)
        (pi-coding-agent--cool-completed-tool-blocks-outside-hot-tail)
        (goto-char (point-max))))))

(provide 'pi-coding-agent-render)

;;; pi-coding-agent-render.el ends here
