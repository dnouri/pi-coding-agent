;;; piem-render.el --- Chat rendering and tool display -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/piem

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

;; Rendering module for piem: streaming chat display, tool output,
;; and fontification.
;;
;; This module handles everything that appears in the chat buffer:
;; - Streaming message display (text deltas, thinking blocks)
;; - Tool call output (overlay creation, streaming preview, toggle)
;; - Event dispatching (handle-display-event)
;; - Streaming fontification (incremental syntax highlighting)
;; - Diff overlay highlighting
;; - Compaction display
;; - File navigation from strict tool rows, plain paths, and local
;;   Markdown labels
;; - Session history display and rendering

;;; Code:

(require 'piem-ui)
(require 'piem-table)
(require 'cl-lib)
(require 'ansi-color)
(require 'image)

;; Forward references for functions in other modules
(declare-function piem-compact "piem-menu" (&optional custom-instructions))
;; Declare the Emacs 29 minimum API.  Adding Emacs 30's optional TAG here makes
;; its byte compiler pad three-argument calls, producing incompatible bytecode.
(declare-function treesit-parser-create "treesit.c"
                  (language &optional buffer no-reuse))
(declare-function treesit-parser-delete "treesit.c" (parser))
(declare-function treesit-parser-list "treesit.c"
                  (&optional buffer language tag))
(declare-function treesit-parser-language "treesit.c" (parser))
(declare-function treesit-parser-included-ranges "treesit.c" (parser))
(declare-function treesit-parser-root-node "treesit.c" (parser))
(declare-function treesit-parser-set-included-ranges "treesit.c"
                  (parser ranges))
(declare-function treesit-node-descendant-for-range "treesit.c"
                  (node beg end &optional named))
(declare-function treesit-query-capture "treesit.c"
                  (node query &optional beg end node-only))
(declare-function comint-output-filter "comint" (process string))
(declare-function comint-term-environment "comint" ())
(declare-function shell-mode "shell" ())

(defconst piem--history-replay-gc-threshold (* 64 1024 1024)
  "Minimum `gc-cons-threshold' used while replaying full session history.")

;;;; Response Display

(defvar-local piem--defer-history-postprocessing nil
  "Non-nil while replaying history with batched display post-processing.
When set, per-message explicit fontification and table decoration are skipped.
Jit-lock fontifies visible markdown at redisplay; history post-processing only
adds display-only table overlays for the recent hot tail after insertion.")

(defvar-local piem--streaming-table-candidate nil
  "Non-nil when recent streaming text may contain a markdown pipe table.
Streaming table decoration is comparatively expensive because it queries the
current message with tree-sitter.  Most assistant text is not table content, so
we track whether a pipe has appeared since the last decoration attempt and skip
the query when no table can be present.")

(defvar-local piem--toolcall-streams nil
  "Tool-call generation state keyed by assistant content index.
Pi's RPC protocol streams raw argument JSON deltas without a cumulative
assistant message.  This table exists only while the current assistant message
is being generated; completed tool execution remains keyed separately by tool
call ID in `piem--live-tool-blocks'.")

(defun piem--history-postprocessing-deferred-p ()
  "Return non-nil when history display post-processing is currently deferred."
  piem--defer-history-postprocessing)

(defun piem--decorate-tables-unless-deferred (start end)
  "Decorate markdown tables between START and END unless deferred."
  (unless (piem--history-postprocessing-deferred-p)
    (piem--decorate-tables-in-region start end)))

(defun piem--display-user-message
    (text &optional timestamp content track-region)
  "Display user message TEXT and optional image CONTENT in the chat buffer.
If TIMESTAMP (Emacs time value) is provided, display it in the header.  When
TRACK-REGION is non-nil, return a marker pair bounding the inserted turn."
  (let* ((chat-buffer (piem--get-chat-buffer))
         (start (with-current-buffer chat-buffer (point-max)))
         (previews (piem--content-image-previews content)))
    (piem--append-to-chat
     (concat "\n" (piem--make-separator "You" timestamp) "\n"
             (or text "") "\n"
             (or (piem--image-previews-text previews) "")))
    (with-current-buffer chat-buffer
      (piem--decorate-tables-unless-deferred start (point-max))
      (when track-region
        (cons (copy-marker start nil) (copy-marker (point-max) nil))))))

(defun piem--discard-local-user-message ()
  "Retract the speculative local user turn when pi handled it invisibly."
  (unwind-protect
      (when-let* ((region piem--local-user-message-region)
                  (start (marker-position (car region)))
                  (end (marker-position (cdr region)))
                  ((<= start end)))
        (let ((inhibit-read-only t))
          (save-restriction
            (widen)
            (delete-region start end))))
    (setq piem--local-user-message nil)
    (piem--clear-local-user-message-region)))

(defun piem--handle-no-turn-local-prompt ()
  "Release speculative local echo state, then process queued follow-ups."
  (unwind-protect
      (piem--discard-local-user-message)
    (piem--schedule-followup-queue-processing)))

(defun piem--content-has-image-p (content)
  "Return non-nil if CONTENT has an image block."
  (seq-some (lambda (block)
              (equal (plist-get block :type) "image"))
            (piem--content-block-list content)))

(defun piem--local-user-message-matches-p
    (local-message text content)
  "Return non-nil when LOCAL-MESSAGE exactly represents TEXT and CONTENT.
Strings retain the existing text-only echo contract.  Image turns use their
full content vector, so an authoritative image transformation cannot be lost."
  (cond
   ((vectorp local-message)
    (and (vectorp content) (equal local-message content)))
   ((stringp local-message)
    (and text
         (not (piem--content-has-image-p content))
         (string= text local-message)))))

(defun piem--display-agent-start ()
  "Display separator for new agent turn.
Only shows the Assistant header once per prompt, even during retries.
Note: status is set to `streaming' by the event handler."
  (piem--set-aborted nil)  ; Reset abort flag for new turn
  ;; Only show header if not already shown for this prompt.
  (unless piem--assistant-header-shown
    (piem--append-to-chat
     (concat "\n" (piem--make-separator "Assistant") "\n"))
    (setq piem--assistant-header-shown t))
  ;; Create markers at current end position
  ;; message-start-marker: where content begins (for later replacement)
  ;; streaming-marker: where new deltas are inserted
  (piem--set-message-start-marker (copy-marker (point-max) nil))
  (piem--set-streaming-marker (copy-marker (point-max) t))
  ;; Reset streaming parse state - content starts at line beginning, outside code/thinking block
  (setq piem--line-parse-state 'line-start)
  (setq piem--in-code-block nil)
  (setq piem--in-thinking-block nil)
  (setq piem--streaming-table-candidate nil)
  (piem--reset-toolcall-streams)
  (piem--set-activity-phase "thinking"))

(defun piem--process-streaming-char (char state in-block)
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

(defun piem--transform-delta (delta)
  "Transform DELTA for display, handling code blocks and heading levels.
Uses and updates buffer-local state variables for parse state.
Returns the transformed string.

Performance: Uses a two-pass approach.  First checks if transformation
is needed (rare), then only does the work when necessary.  The common
case of no headings is O(n) with no allocations."
  (let ((state piem--line-parse-state)
        (in-block piem--in-code-block)
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
          (let ((new-state (piem--process-streaming-char char state in-block)))
            (setq state (car new-state))
            (setq in-block (cdr new-state)))
          (setq i (1+ i))))
      ;; Save final state
      (setq piem--line-parse-state state)
      (setq piem--in-code-block in-block)
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

(defun piem--display-message-delta (delta)
  "Display streaming message DELTA at the streaming marker.
Transforms ATX headings (outside code blocks) by adding one # level
to keep our setext H1 separators as the top-level document structure.
Modification hooks fire normally so jit-lock marks inserted text for
fontification; tree-sitter re-parses at the C level on each insert."
  (when (and delta piem--streaming-marker)
    (let* ((inhibit-read-only t)
           (delta (piem--render-safe-string delta))
           ;; Strip leading newlines from first content after header
           (delta (if (and piem--message-start-marker
                          (= (marker-position piem--message-start-marker)
                             (marker-position piem--streaming-marker)))
                     (string-trim-left delta "\n+")
                   delta))
           (transformed (piem--transform-delta delta)))
      (piem--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position piem--streaming-marker))
          (insert transformed)
          (set-marker piem--streaming-marker (point))))
      ;; After inserting text with completed lines, check for active tables only
      ;; if recent streaming text contained a pipe.  This avoids a tree-sitter
      ;; table query on every non-table newline in long assistant messages.
      (when (string-match-p "|" delta)
        (setq piem--streaming-table-candidate t))
      (when (and piem--streaming-table-candidate
                 (string-match-p "\n" delta))
        (setq piem--streaming-table-candidate nil)
        (piem--maybe-decorate-streaming-table)))))

(defun piem--thinking-insert-position ()
  "Return insertion position for thinking text.
Prefers `piem--thinking-marker' when available so interleaved
tool headers do not move the thinking insertion point."
  (if (and piem--thinking-marker
           (marker-position piem--thinking-marker))
      (marker-position piem--thinking-marker)
    (marker-position piem--streaming-marker)))

(defun piem--thinking-normalize-text (text)
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

(defun piem--thinking-blockquote-text (text)
  "Convert normalized thinking TEXT to markdown blockquote lines."
  (if (string-empty-p text)
      ""
    (concat "> " (replace-regexp-in-string "\n" "\n> " text))))

(defun piem--thinking-line-count-label (count)
  "Return COUNT formatted as a singular or plural line label."
  (format "%d line%s" count (if (= count 1) "" "s")))

(defun piem--thinking-more-lines-label (count)
  "Return COUNT formatted as a singular or plural hidden-line label."
  (format "%d more line%s" count (if (= count 1) "" "s")))

(defun piem--thinking-first-content-line (normalized)
  "Return the first non-empty trimmed line from NORMALIZED, or nil."
  (catch 'first
    (dolist (line (split-string normalized "\n" nil))
      (let ((trimmed (string-trim line)))
        (unless (string-empty-p trimmed)
          (throw 'first trimmed))))))

(defun piem--thinking-hidden-stub (normalized)
  "Return the collapsed completed-thinking stub for NORMALIZED."
  (let* ((line-count (length (split-string normalized "\n" nil)))
         (first-line (piem--thinking-first-content-line normalized))
         (previewable (and piem-thinking-hidden-preview
                           (> line-count 1)
                           first-line
                           (>= (length first-line) 3)
                           (< (length first-line) 72))))
    (if previewable
        (format "> Thinking: %s… (%s)"
                first-line
                (piem--thinking-more-lines-label (1- line-count)))
      (format "> Thinking hidden… (%s)"
              (piem--thinking-line-count-label line-count)))))

(defun piem--next-thinking-block-order ()
  "Return the next monotonically increasing completed-thinking block order."
  (let ((order (or piem--thinking-block-order-counter 0)))
    (setq piem--thinking-block-order-counter (1+ order))
    order))

(defun piem--propertize-completed-thinking
    (rendered order normalized display)
  "Return RENDERED tagged as completed thinking block metadata.
ORDER identifies the logical block across rerenders.  NORMALIZED stores the
canonical completed thinking text, and DISPLAY records whether this block is
currently shown as `visible' or `hidden'."
  (propertize rendered
              'piem-thinking-block order
              'piem-thinking-normalized normalized
              'piem-thinking-block-display display
              'help-echo "TAB: toggle completed thinking"))

(defun piem--apply-completed-thinking-properties
    (start end order normalized display)
  "Tag START..END as completed thinking metadata.
ORDER identifies the block, NORMALIZED stores its canonical text, and DISPLAY
records whether it is currently shown as `visible' or `hidden'."
  (when (< start end)
    (add-text-properties
     start end
     `(piem-thinking-block ,order
       piem-thinking-normalized ,normalized
       piem-thinking-block-display ,display
       help-echo "TAB: toggle completed thinking"))))

(defun piem--thinking-block-probe-pos (pos)
  "Return a position inside the completed-thinking block at POS, or nil.
Checks POS and the preceding character so point on a block boundary can still
resolve to the completed thinking block the user was inspecting."
  (when (> (point-max) (point-min))
    (let ((probe (cond ((<= pos (point-min)) (point-min))
                       ((>= pos (point-max)) (max (point-min)
                                                  (1- (point-max))))
                       (t pos))))
      (cond
       ((get-text-property probe 'piem-thinking-block) probe)
       ((and (> probe (point-min))
             (get-text-property (1- probe)
                                'piem-thinking-block))
        (1- probe))))))

(defun piem--thinking-block-at-pos (pos)
  "Return completed-thinking block order at POS, or nil."
  (when-let* ((probe (piem--thinking-block-probe-pos pos)))
    (get-text-property probe 'piem-thinking-block)))

(defun piem--thinking-block-start (block-order)
  "Return the start position of completed thinking BLOCK-ORDER, or nil."
  (when block-order
    (text-property-any (point-min) (point-max)
                       'piem-thinking-block block-order)))

(defun piem--thinking-block-bounds-from-probe (probe)
  "Return completed-thinking bounds around PROBE, or nil.
PROBE must already be inside a completed-thinking block."
  (when (get-text-property probe 'piem-thinking-block)
    (cons (or (previous-single-property-change
               (1+ probe)
               'piem-thinking-block
               nil
               (point-min))
              (point-min))
          (or (next-single-property-change
               probe
               'piem-thinking-block
               nil
               (point-max))
              (point-max)))))

(defun piem--thinking-block-bounds-at-pos (pos)
  "Return bounds of the completed-thinking block at POS, or nil."
  (when-let* ((probe (piem--thinking-block-probe-pos pos)))
    (piem--thinking-block-bounds-from-probe probe)))

(defun piem--thinking-block-metadata-at-pos (pos)
  "Return completed-thinking block metadata at POS, or nil."
  (when-let* ((probe (piem--thinking-block-probe-pos pos))
              (bounds (piem--thinking-block-bounds-from-probe probe))
              (normalized (get-text-property probe
                                             'piem-thinking-normalized)))
    (list :order (get-text-property probe 'piem-thinking-block)
          :display (or (get-text-property probe
                                          'piem-thinking-block-display)
                       'visible)
          :normalized normalized
          :start (car bounds)
          :end (cdr bounds))))

(defun piem--replace-thinking-region (rendered)
  "Replace the active thinking region with RENDERED text.
RENDERED should already be the markdown to insert, or an empty string to remove
an empty placeholder block.  Returns non-nil when the resulting region is
non-empty."
  (when (and (markerp piem--thinking-start-marker)
             (markerp piem--thinking-marker)
             (marker-position piem--thinking-start-marker)
             (marker-position piem--thinking-marker))
    (let* ((start (marker-position piem--thinking-start-marker))
           (end (marker-position piem--thinking-marker))
           (text (or rendered ""))
           (plain-text (substring-no-properties text))
           (order (and (> (length text) 0)
                       (get-text-property 0 'piem-thinking-block text)))
           (normalized (and order
                            (get-text-property 0
                                               'piem-thinking-normalized
                                               text)))
           (display (and order
                         (get-text-property 0
                                            'piem-thinking-block-display
                                            text))))
      (when (<= start end)
        (let ((existing (buffer-substring-no-properties start end)))
          (if (equal existing plain-text)
              (when order
                (piem--apply-completed-thinking-properties
                 start end order normalized display))
            (goto-char start)
            (delete-region start end)
            (insert text)
            (set-marker piem--thinking-marker (point))))
        (not (string-empty-p text))))))

(defun piem--render-thinking-content ()
  "Render normalized accumulated thinking content in place.
Returns non-nil when meaningful content remains after normalization."
  (when (and (markerp piem--thinking-start-marker)
             (markerp piem--thinking-marker)
             (marker-position piem--thinking-start-marker)
             (marker-position piem--thinking-marker))
    (let* ((start (marker-position piem--thinking-start-marker))
           (end (marker-position piem--thinking-marker))
           (normalized (piem--thinking-normalize-text
                        piem--thinking-raw))
           (rendered (piem--thinking-blockquote-text normalized))
           (prev piem--thinking-prev-rendered))
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
              (set-marker piem--thinking-marker (point)))))
         ;; Slow path: full rewrite; skip if buffer already matches.
         (t
          (let ((existing (buffer-substring-no-properties start end)))
            (unless (equal existing rendered)
              (goto-char start)
              (delete-region start end)
              (insert rendered)
              (set-marker piem--thinking-marker (point))))))
        (setq piem--thinking-prev-rendered rendered))
      (and (<= start end)
           (not (string-empty-p normalized))))))

(defun piem--ensure-blank-line-separator ()
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

(defun piem--ensure-blank-line-before-block ()
  "Ensure point is on a fresh line with a blank line above.
Used before inserting a new block (thinking, tool) so it is visually
separated from preceding content."
  (unless (bolp)
    (insert "\n"))
  (unless (save-excursion
            (forward-line -1)
            (looking-at-p "^$"))
    (insert "\n")))

(defun piem--reset-thinking-state ()
  "Detach and clear all thinking-stream state for the current turn."
  (when (markerp piem--thinking-marker)
    (set-marker piem--thinking-marker nil))
  (when (markerp piem--thinking-start-marker)
    (set-marker piem--thinking-start-marker nil))
  (setq piem--thinking-marker nil
        piem--thinking-start-marker nil
        piem--thinking-raw nil
        piem--thinking-prev-rendered nil))

(defmacro piem--with-window-rewrite-preservation (&rest body)
  "Execute BODY and keep chat windows useful after a large rewrite.
This is for rewrites that can delete the text under `window-start', such as
collapsing a long thinking block or rebuilding canonical history.  Tail views
stay at the new tail; non-tail views keep their point and approximate row,
clamped so the window remains filled when possible."
  (declare (indent 0) (debug t))
  `(let ((buffer (current-buffer))
         (saved-windows (piem--capture-window-rewrite-states))
         result)
     (unwind-protect
         (setq result (progn ,@body))
       (piem--restore-window-rewrite-states buffer saved-windows))
     result))

(defun piem--display-thinking-start ()
  "Insert opening marker for thinking block (blockquote)."
  (when piem--streaming-marker
    (setq piem--in-thinking-block t)
    (let ((inhibit-read-only t))
      (piem--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position piem--streaming-marker))
          ;; No separator needed when this is the first content in the message.
          (when (and piem--message-start-marker
                     (> (point)
                        (marker-position piem--message-start-marker)))
            (piem--ensure-blank-line-before-block))
          ;; Track thinking insertion separately so it stays anchored even if
          ;; other block types (tool headers) interleave in the same message.
          ;; Keep insertion-type nil so inserts at this exact point happen
          ;; after the marker (we then advance it explicitly per delta).
          (piem--reset-thinking-state)
          (setq piem--thinking-raw "")
          (let ((start (point)))
            (insert "> ")
            (setq piem--thinking-start-marker
                  (copy-marker start nil))
            (setq piem--thinking-marker
                  (copy-marker (point) nil))))))))

(defun piem--display-thinking-delta (delta)
  "Display streaming thinking DELTA in the current thinking block.
Normalizes boundary and paragraph whitespace while streaming."
  (when (and delta piem--streaming-marker)
    (let ((delta (piem--render-safe-string delta))
          (inhibit-read-only t))
      (if (and piem--thinking-start-marker
               piem--thinking-marker)
          (progn
            (setq piem--thinking-raw
                  (concat (or piem--thinking-raw "") delta))
            (piem--with-scroll-preservation
              (save-excursion
                (piem--render-thinking-content))))
        ;; Fallback for malformed event streams that skip thinking_start.
        (let ((transformed (replace-regexp-in-string "\n" "\n> " delta)))
          (piem--with-scroll-preservation
            (save-excursion
              (goto-char (piem--thinking-insert-position))
              (insert transformed)
              (when piem--thinking-marker
                (set-marker piem--thinking-marker (point))))))))))

(defun piem--display-thinking-end (_content)
  "End thinking block (blockquote).
CONTENT is ignored - we use what was already streamed."
  (when piem--streaming-marker
    (let* ((buffer (current-buffer))
           (saved-windows (piem--capture-window-rewrite-states))
           (old-point-max (point-max))
           (rewrite-start (and (markerp piem--thinking-start-marker)
                               (marker-position piem--thinking-start-marker)))
           (rewrite-end (and (markerp piem--thinking-marker)
                             (marker-position piem--thinking-marker))))
      (unwind-protect
          (progn
            (setq piem--in-thinking-block nil)
            (let ((inhibit-read-only t))
              (piem--with-scroll-preservation
                (save-excursion
                  (if (and piem--thinking-start-marker
                           piem--thinking-marker)
                      (when (piem--replace-thinking-region
                             (piem--completed-thinking-rendered-text
                              piem--thinking-raw))
                        (goto-char (piem--thinking-insert-position))
                        (piem--ensure-blank-line-separator))
                    ;; Fallback for malformed event streams that skip thinking_start.
                    (goto-char (piem--thinking-insert-position))
                    (piem--ensure-blank-line-separator))
                  (piem--reset-thinking-state)))))
        (piem--restore-window-rewrite-states
         buffer
         saved-windows
         (when (and rewrite-start rewrite-end)
           (let ((replacements
                  (list (list rewrite-start
                              rewrite-end
                              (with-current-buffer buffer
                                (- (point-max) old-point-max))))))
             (lambda (pos)
               (piem--adjust-pos-after-region-replacements
                pos replacements)))))))))

(defconst piem--followup-drain-delay 0.05
  "Seconds to wait after agent_end before draining local follow-ups.
Pi may emit post-run compaction or retry events immediately after agent_end;
this short delay lets those events claim ordering before Emacs sends a local
follow-up as a fresh prompt.")

(defun piem--cancel-followup-drain-timer ()
  "Cancel any pending local follow-up queue drain timer."
  (when (timerp piem--followup-drain-timer)
    (cancel-timer piem--followup-drain-timer))
  (setq piem--followup-drain-timer nil))

(defun piem--ready-to-drain-followups-p ()
  "Return non-nil when a queued follow-up may become the next prompt."
  (and piem--followup-queue
       (eq piem--status 'idle)
       (not (piem--model-change-pending-p))
       (not (piem--session-transition-active-p))
       (not (piem--prompt-start-wait-active-p))
       (null piem--local-user-message)))

(defun piem--drain-followup-queue-if-idle (buffer)
  "Drain BUFFER's follow-up queue only if the session is still idle."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq piem--followup-drain-timer nil)
      (when (piem--ready-to-drain-followups-p)
        (piem--process-followup-queue)))))

(defun piem--schedule-followup-queue-processing ()
  "Schedule local follow-up queue processing after post-run events settle."
  (when (piem--ready-to-drain-followups-p)
    (piem--cancel-followup-drain-timer)
    (setq piem--followup-drain-timer
          (run-at-time piem--followup-drain-delay nil
                       #'piem--drain-followup-queue-if-idle
                       (current-buffer)))))

(defun piem--display-agent-end ()
  "Finalize agent turn: normalize whitespace, handle abort, schedule queue."
  ;; Reset per-turn state for clean next turn.
  (setq piem--local-user-message nil)
  (piem--clear-local-user-message-region)
  (setq piem--in-thinking-block nil)
  (piem--reset-thinking-state)
  (let ((was-aborted piem--aborted))
    (let ((inhibit-read-only t))
      (piem--finalize-live-tool-blocks 'piem-tool-block-error)
      (piem--reset-toolcall-streams)
      (when piem--tool-args-cache
        (clrhash piem--tool-args-cache))
      ;; Abort means "stop everything" — discard queued follow-ups too
      (when piem--aborted
        (piem--with-scroll-preservation
          (save-excursion
            (goto-char (point-max))
            ;; Remove trailing whitespace before adding indicator
            (skip-chars-backward " \t\n")
            (delete-region (point) (point-max))
            (insert "\n\n" (propertize "[Aborted]" 'face 'error) "\n")))
        (piem--set-aborted nil)
        (piem--clear-followup-queue))
      (piem--with-scroll-preservation
        (save-excursion
          (goto-char (point-max))
          (skip-chars-backward "\n")
          (delete-region (point) (point-max))
          (insert "\n"))))
    (piem--set-activity-phase
     (if (eq piem--status 'sending) "thinking" "idle"))
    (piem--refresh-header)
    ;; Give immediate post-run compaction/retry events a chance to arrive before
    ;; turning a local follow-up into a new independent prompt.
    (unless was-aborted
      (piem--schedule-followup-queue-processing))))

(defun piem--dispatch-builtin-command (text)
  "Try to dispatch TEXT as a built-in slash command.
Returns non-nil if TEXT matched a built-in command and was handled."
  (when (string-prefix-p "/" text)
    (let* ((without-slash (substring text 1))
           (words (split-string without-slash))
           (cmd-name (car words))
           (entry (assoc cmd-name piem--builtin-commands)))
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

(defun piem--prepare-and-send (text &optional queued prompt-image)
  "Prepare chat buffer state and send TEXT with optional PROMPT-IMAGE to pi.
Built-in slash commands are dispatched locally via the dispatch table.
Other slash commands (extensions, skills, prompts) are sent to pi without
local transcript display.  Regular text is displayed after prompt preflight
accepts it.
When QUEUED is non-nil, TEXT is the oldest local follow-up and is removed
from the queue only after prompt preflight succeeds.  PROMPT-IMAGE is only
valid for a direct regular prompt.
Must be called with chat buffer current.  Pi events own streaming/idle turn
transitions; prompt submission marks the local pre-event window as busy."
  (piem--invalidate-history-loads)
  (cond
   ;; Built-in slash commands are interactive client actions, not durable queued
   ;; prompts.  Busy input refuses new ones; this guard keeps stale queued items
   ;; from running later out of context.
   ((and queued (piem--builtin-command-text-p text))
    (piem--restore-followup-queue-to-input)
    (message "Pi: Cannot run queued /%s command automatically"
             (piem--builtin-command-name text)))
   ;; Built-in slash commands: dispatch locally.
   ((piem--dispatch-builtin-command text))
   ;; Other slash commands: don't display locally, send to pi.
   ((string-prefix-p "/" text)
    (piem--send-prompt
     text
     (when queued
       (lambda () (piem--drop-followup text)))
     (if queued
         #'piem--restore-followup-queue-to-input
       (lambda () (piem--restore-input-text text)))
     #'piem--schedule-followup-queue-processing))
   ;; Regular text is displayed only after prompt preflight accepts it.  That
   ;; keeps rejected prompts out of the transcript and lets us restore them to
   ;; the input buffer for user recovery.
   (prompt-image
    (let* ((image-block
            (piem--prompt-image-content-block prompt-image))
           (user-content
            (vector (list :type "text" :text text) image-block)))
      (piem--send-prompt
       text
       (lambda ()
         (setq piem--local-user-message-region
               (piem--display-user-message
                text (current-time) user-content t))
         (setq piem--local-user-message user-content)
         (setq piem--assistant-header-shown nil))
       (lambda () (piem--restore-input-text text prompt-image))
       #'piem--handle-no-turn-local-prompt
       prompt-image)))
   (queued
    (piem--send-prompt
     text
     (lambda ()
       (when (piem--drop-followup text)
         (setq piem--local-user-message-region
               (piem--display-user-message
                text (current-time) nil t))
         (setq piem--local-user-message text)
         (setq piem--assistant-header-shown nil)))
     #'piem--restore-followup-queue-to-input
     #'piem--handle-no-turn-local-prompt))
   (t
    (piem--send-prompt
     text
     (lambda ()
       (setq piem--local-user-message-region
             (piem--display-user-message
              text (current-time) nil t))
       (setq piem--local-user-message text)
       (setq piem--assistant-header-shown nil))
     (lambda () (piem--restore-input-text text))
     #'piem--handle-no-turn-local-prompt))))

(defun piem--process-followup-queue ()
  "Send the oldest follow-up only when it is safe to become the next prompt.
Messages are processed in FIFO order and dropped only after preflight accepts
them."
  (when (piem--ready-to-drain-followups-p)
    (when-let* ((text (piem--peek-followup)))
      (piem--prepare-and-send text 'queued))))

(defun piem--display-compaction-failure (error-message)
  "Display failed compaction ERROR-MESSAGE without changing the queue."
  (let ((error-text (or (piem--normalize-string-or-null error-message)
                        "unknown error")))
    (piem--display-error (format "Compaction failed: %s" error-text))
    (message "Pi: Compaction failed: %s" error-text)))

(defun piem--post-compaction-activity-phase ()
  "Return activity phase after a compaction_end event."
  (if (or (eq piem--status 'sending)
          (piem--prompt-start-wait-active-p))
      "thinking"
    "idle"))

(defun piem--handle-compaction-end-event (event)
  "Display canonical compaction_end EVENT and manage follow-up queues.
Status transitions are handled by `piem--update-state-from-event'."
  (let ((result (piem--compaction-result-from-event event)))
    (cond
     ((piem--normalize-boolean (plist-get event :aborted))
      (piem--set-activity-phase
       (piem--post-compaction-activity-phase))
      (message "Pi: Compaction cancelled")
      ;; Clear queue on abort (user wanted to stop).
      (piem--clear-followup-queue))
     (result
      (piem--handle-compaction-success
       (plist-get result :tokensBefore)
       (plist-get result :summary)
       (piem--ms-to-time (plist-get result :timestamp)))
      (if (eq piem--status 'sending)
          (progn
            ;; Pi is either retrying automatically or resuming a prompt whose
            ;; preflight compacted first.  Keep local follow-ups behind that
            ;; Pi-owned work.
            (piem--set-activity-phase "thinking"))
        (piem--set-activity-phase "idle")
        (piem--schedule-followup-queue-processing)))
     (t
      (piem--set-activity-phase
       (piem--post-compaction-activity-phase))
      (piem--display-compaction-failure
       (plist-get event :errorMessage))
      ;; During prompt preflight, Pi reports compaction failure before the
      ;; prompt RPC failure that owns the original prompt text.  Restore local
      ;; follow-ups now; the prompt callback clears the wait and restores the
      ;; direct prompt if Pi rejects it.
      (piem--restore-followup-queue-to-input)))))

(defun piem--display-retry-start (event)
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
    (piem--append-to-chat
     (concat (propertize notice 'face 'piem-retry-notice) "\n"))))

(defun piem--display-retry-end (event)
  "Display retry result from auto_retry_end EVENT.
Shows success or final failure with raw error."
  (let* ((success (plist-get event :success))
         (attempt (plist-get event :attempt))
         (final-error (or (plist-get event :finalError) "unknown error")))
    (if (eq success t)
        (piem--append-to-chat
         (concat (propertize (format "✓ Retry succeeded on attempt %d"
                                     (or attempt 1))
                             'face 'piem-retry-notice)
                 "\n\n"))
      ;; Final failure
      (piem--append-to-chat
       (concat (propertize (format "✗ Retry failed after %d attempts — %s"
                                   (or attempt 1)
                                   final-error)
                           'face 'piem-error-notice)
               "\n\n")))))

(defun piem--display-error (error-msg)
  "Display ERROR-MSG from the server."
  (piem--append-to-chat
   (concat "\n" (propertize (format "[Error: %s]" (or error-msg "unknown"))
                            'face 'piem-error-notice)
           "\n")))

(defconst piem--startup-env-node-hint
  (concat "Probable cause: Pi's Node launcher cannot see `node`.\n\n"
          "Emacs found the configured Pi launcher, but it uses `/usr/bin/env node`, "
          "which searches the subprocess PATH, not only Emacs `exec-path`. "
          "Put Node's bin directory on the PATH seen by Emacs-created "
          "processes, or set `piem-executable` to a wrapper "
          "that does.")
  "Hint shown when Pi's Node launcher cannot find node at startup.")

(defun piem--startup-env-node-error-p (exit-code stderr)
  "Return non-nil when EXIT-CODE and STDERR look like env failing to find node."
  (and (equal exit-code 127)
       (stringp stderr)
       (let ((case-fold-search nil))
         (catch 'found
           (dolist (line (split-string stderr "[\r\n]+" t))
             (when (and (string-match-p "\\(?:\\`\\|/\\)env:" line)
                        (string-match-p
                         "\\(?:\\`\\|[^[:alnum:]_]\\)node\\(?:[^[:alnum:]_]\\|\\'\\)"
                         line))
               (throw 'found t)))))))

(defun piem--format-process-error
    (heading error-msg &optional stderr detail)
  "Format process error HEADING and ERROR-MSG.
Include optional STDERR in a text fence and optional DETAIL before it."
  (let ((stderr (and (stringp stderr)
                     (not (string-empty-p stderr))
                     stderr)))
    (concat "\n"
            (propertize heading 'face 'piem-error-notice)
            "\n\n"
            (or error-msg "unknown error")
            (when detail (concat "\n\n" detail))
            (when stderr
              (concat "\n\n"
                      (propertize "stderr:"
                                  'face 'piem-retry-notice)
                      "\n```text\n"
                      stderr
                      (unless (string-suffix-p "\n" stderr) "\n")
                      "```\n")))))

(defun piem--display-startup-error (error-msg &optional stderr exit-code)
  "Display a pi startup ERROR-MSG, optional STDERR, and EXIT-CODE."
  (piem--append-to-chat
   (concat (piem--format-process-error
            "✗ pi failed to start" error-msg stderr)
           (when (piem--startup-env-node-error-p exit-code stderr)
             (concat "\n" piem--startup-env-node-hint "\n")))))

(defun piem--display-process-exit-error
    (error-msg &optional stderr exit-code)
  "Display a pi process exit ERROR-MSG, optional STDERR, and EXIT-CODE."
  (piem--append-to-chat
   (piem--format-process-error
    "✗ pi process exited" error-msg stderr
    (when exit-code
      (format "Exit code: %s" exit-code)))))

(defun piem--display-extension-error (event)
  "Display extension error from extension_error EVENT."
  (let* ((extension-path (plist-get event :extensionPath))
         (extension-event (plist-get event :event))
         (error-msg (plist-get event :error))
         (extension-name (if extension-path (file-name-nondirectory extension-path) "unknown")))
    (piem--append-to-chat
     (concat "\n"
             (propertize (format "[Extension error in %s (%s): %s]"
                                 extension-name
                                 (or extension-event "unknown")
                                 (or error-msg "unknown error"))
                         'face 'piem-error-notice)
             "\n"))))

(defun piem--extension-ui-notify (event)
  "Handle notify method from EVENT."
  (let ((msg (plist-get event :message))
        (notify-type (plist-get event :notifyType)))
    (message "Pi: %s%s"
             (pcase notify-type
               ("warning" "⚠ ")
               ("error" "✗ ")
               (_ ""))
             msg)))

(defun piem--extension-ui-confirm (event proc)
  "Handle confirm method from EVENT, responding via PROC."
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (msg (plist-get event :message))
         ;; Don't add colon if title already ends with one
         (separator (if (string-suffix-p ":" title) " " ": "))
         (prompt (format "%s%s%s " title separator msg))
         (confirmed (yes-or-no-p prompt)))
    (when proc
      (piem--send-extension-ui-response proc
                     (list :type "extension_ui_response"
                           :id id
                           :confirmed (if confirmed t :json-false))))))

(defun piem--extension-ui-select (event proc)
  "Handle select method from EVENT, responding via PROC."
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (options (append (plist-get event :options) nil))
         (selected (completing-read (concat title " ") options nil t)))
    (when proc
      (piem--send-extension-ui-response proc
                     (list :type "extension_ui_response"
                           :id id
                           :value selected)))))

(defun piem--extension-ui-input (event proc)
  "Handle input method from EVENT, responding via PROC."
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (placeholder (plist-get event :placeholder))
         (value (read-string (concat title " ") placeholder)))
    (when proc
      (piem--send-extension-ui-response proc
                     (list :type "extension_ui_response"
                           :id id
                           :value value)))))

(defun piem--extension-ui-set-editor-text (event)
  "Handle set_editor_text method from EVENT."
  (let ((text (plist-get event :text)))
    (when-let* ((input-buf piem--input-buffer))
      (piem--replace-input-draft input-buf text))))

(defun piem--extension-ui-set-status (event)
  "Handle setStatus method from EVENT."
  (let ((key (plist-get event :statusKey))
        (text (plist-get event :statusText)))
    (when text
      (setq text (ansi-color-filter-apply
                  (piem--render-safe-string text))))
    (if text
        (setq piem--extension-status
              (cons (cons key text)
                    (assoc-delete-all key piem--extension-status)))
      (setq piem--extension-status
            (assoc-delete-all key piem--extension-status)))
    (force-mode-line-update t)))

;; Pi RPC currently suppresses this method; retain client protocol support.
(defun piem--extension-ui-set-working-message (event)
  "Handle setWorkingMessage method from EVENT."
  (let ((msg (plist-get event :message)))
    (when msg
      (setq msg (ansi-color-filter-apply
                 (piem--render-safe-string msg))))
    (setq piem--working-message msg)
    (force-mode-line-update t)))

(defconst piem--extension-ui-fire-and-forget-methods
  '("notify" "setStatus" "setWidget" "setTitle" "set_editor_text"
    "setWorkingMessage")
  "Extension UI methods that do not expect RPC responses.")

(defun piem--extension-ui-response-required-p (method)
  "Return non-nil when unsupported extension UI METHOD may expect a response."
  (not (member method piem--extension-ui-fire-and-forget-methods)))

(defun piem--extension-ui-warn-unsupported-once (method)
  "Warn at most once per pi session for unsupported extension UI METHOD."
  (when (piem--record-unsupported-extension-ui-warning method)
    (message "Pi: extension UI method `%s' not supported in Emacs" method)))

(defun piem--extension-ui-unsupported (event proc)
  "Handle unsupported method from EVENT, using PROC to cancel when needed.
Warn at most once per method in a pi session.
Dialog-like methods receive a cancelled response so extensions do not hang;
fire-and-forget methods are only warned because they do not expect responses.
See URL `https://github.com/dnouri/piem/issues/176'."
  (let ((method (plist-get event :method))
        (id (plist-get event :id)))
    (piem--extension-ui-warn-unsupported-once method)
    (when (and proc
               id
               (piem--extension-ui-response-required-p method))
      (piem--send-extension-ui-response
       proc (list :type "extension_ui_response"
                  :id id
                  :cancelled t)))))

(defun piem--handle-extension-ui-request (event)
  "Handle extension_ui_request EVENT from pi.
Dispatches to appropriate handler based on method."
  (let ((method (plist-get event :method))
        (proc piem--process))
    (pcase method
      ("notify"         (piem--extension-ui-notify event))
      ("confirm"        (piem--extension-ui-confirm event proc))
      ("select"         (piem--extension-ui-select event proc))
      ("input"          (piem--extension-ui-input event proc))
      ("set_editor_text" (piem--extension-ui-set-editor-text event))
      ("setStatus"      (piem--extension-ui-set-status event))
      ("setWorkingMessage" (piem--extension-ui-set-working-message event))
      (_                (piem--extension-ui-unsupported event proc)))))

(defun piem--display-no-model-warning ()
  "Display warning when no model is available.
Shown when the session starts without a configured model/API key."
  (piem--append-to-chat
   (concat "\n"
           (propertize "⚠ No models available"
                       'face 'piem-error-notice)
           "\n\n"
           (propertize "To get started, either:\n"
                       'face 'piem-retry-notice)
           (propertize "  • Set an API key: "
                       'face 'piem-retry-notice)
           "ANTHROPIC_API_KEY, OPENAI_API_KEY, GEMINI_API_KEY, etc.\n"
           (propertize "  • Or run "
                       'face 'piem-retry-notice)
           (propertize "pi --login"
                       'face 'piem-tool-command)
           (propertize " in a terminal to authenticate via OAuth\n"
                       'face 'piem-retry-notice)
           "\n")))

(defun piem--cleanup-on-kill ()
  "Clean up resources when chat buffer is killed.
Also kills the linked input buffer and fontification cache buffers.

Note: This runs from `kill-buffer-hook', which executes AFTER the kill
decision is made.  For proper cancellation support, use `piem-quit'
which asks upfront before any buffers are touched."
  (when (derived-mode-p 'piem-chat-mode)
    (piem--cancel-followup-drain-timer)
    (piem--cancel-tool-update-flush)
    (piem--cancel-tool-cooling)
    (piem--invalidate-prompt-start-wait)
    (piem--set-activity-phase "idle" 'teardown t)
    (dolist (proc (delete-dups (delq nil (list piem--process
                                               piem--session-transition-process))))
      (when (processp proc)
        (piem--unregister-display-handler proc)
        (when (process-live-p proc)
          (delete-process proc))))
    (when (and piem--input-buffer (buffer-live-p piem--input-buffer))
      (let ((input-buf piem--input-buffer))
        (piem--set-input-buffer nil) ; break cycle before kill
        (kill-buffer input-buf)))
    (piem--cleanup-visible-string-buffer)))

(defun piem--cleanup-input-on-kill ()
  "Clean up when input buffer is killed.
Also kills the linked chat buffer (which handles process cleanup).

Note: This runs from `kill-buffer-hook', which executes AFTER the kill
decision is made.  For proper cancellation support, use `piem-quit'
which asks upfront before any buffers are touched."
  (when (derived-mode-p 'piem-input-mode)
    (when (and piem--chat-buffer (buffer-live-p piem--chat-buffer))
      (let* ((chat-buf piem--chat-buffer)
             (proc (buffer-local-value 'piem--process chat-buf)))
        (piem--set-chat-buffer nil) ; break cycle before kill
        (when (and proc (process-live-p proc))
          (piem--skip-process-kill-confirmation proc)
          (set-process-query-on-exit-flag proc nil))
        (kill-buffer chat-buf)))))

(defun piem--register-display-handler (process)
  "Register display and process-exit handlers for PROCESS."
  (process-put process 'piem-display-handler
               (piem--make-display-handler process))
  (process-put process 'piem-exit-handler
               (piem--make-process-exit-handler process)))

(defun piem--unregister-display-handler (process)
  "Unregister display and process-exit handlers for PROCESS."
  (process-put process 'piem-display-handler nil)
  (process-put process 'piem-exit-handler nil))

(defun piem--make-display-handler (process)
  "Create a display event handler for PROCESS."
  (lambda (event)
    (when-let* ((chat-buf (process-get process 'piem-chat-buffer)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (piem--handle-display-event event))))))

(defun piem--make-process-exit-handler (process)
  "Create a frontend cleanup handler for PROCESS exit."
  (lambda (response)
    (when-let* ((chat-buf (process-get process 'piem-chat-buffer))
                ((buffer-live-p chat-buf)))
      (with-current-buffer chat-buf
        (piem--mark-process-exited process response)))))

(defun piem--mark-process-exited (process response)
  "Mark current chat buffer idle after PROCESS exits with RESPONSE."
  (when (eq piem--process process)
    (let ((error-msg (or (plist-get response :error) "Process exited")))
      (setq piem--status 'idle)
      (setq piem--state
            (plist-put piem--state :last-error error-msg))
      (unwind-protect
          (unless (process-get process 'piem-exit-error-rendered)
            (piem--display-process-exit-error
             error-msg
             (plist-get response :stderr)
             (plist-get response :exitCode))
            (process-put process 'piem-exit-error-rendered t))
        (piem--cancel-tool-update-flush)
        (piem--finalize-live-tool-blocks
         'piem-tool-block-error)
        (piem--reset-toolcall-streams)
        (when piem--tool-args-cache
          (clrhash piem--tool-args-cache))
        (piem--set-process nil)
        (piem--invalidate-model-change)
        (piem--set-activity-phase "idle")
        (setq piem--local-user-message nil)
        (piem--clear-local-user-message-region)
        (setq piem--pre-compaction-status nil)
        (piem--cancel-followup-drain-timer)
        (piem--invalidate-prompt-start-wait)
        (piem--restore-followup-queue-to-input)
        (force-mode-line-update t)))))

(defun piem--display-custom-message (content)
  "Display visible custom CONTENT in the current chat buffer."
  (when (and (stringp content)
             (not (string-empty-p content)))
    (let ((start (point-max)))
      (piem--append-to-chat (concat "\n" content "\n"))
      (piem--decorate-tables-unless-deferred start (point-max)))
    ;; Reset so next assistant message shows its header
    (setq piem--assistant-header-shown nil)))

(defun piem--handle-display-event (event)
  "Handle EVENT for display purposes.
Updates buffer-local state and renders display updates."
  ;; Update state first (now buffer-local)
  (piem--update-state-from-event event)
  ;; Then handle display
  (pcase (plist-get event :type)
    ("agent_start"
     (piem--invalidate-prompt-start-wait)
     (piem--cancel-followup-drain-timer)
     (piem--display-agent-start))
    ("message_start"
     (let* ((message (plist-get event :message))
            (role (plist-get message :role)))
       ;; A new message starts a fresh rendering context.
       (setq piem--in-thinking-block nil)
       (piem--reset-thinking-state)
       (when (equal role "assistant")
         (piem--reset-toolcall-streams))
       (pcase role
         ("user"
          ;; User message from pi - check if we displayed it locally
          (let* ((content (plist-get message :content))
                 (timestamp (plist-get message :timestamp))
                 (text (when content
                         (piem--extract-user-message-text content)))
                 (has-images (piem--content-has-image-p content))
                 (local-msg piem--local-user-message))
            ;; Clear local tracking before rendering the authoritative turn.
            (setq piem--local-user-message nil)
            (piem--clear-local-user-message-region)
            (when (and (or text has-images)
                       (not (piem--local-user-message-matches-p
                             local-msg text content)))
              (piem--display-user-message
               text
               (piem--ms-to-time timestamp)
               content)
              ;; Reset so next assistant message shows its header
              (setq piem--assistant-header-shown nil))))
         ("custom"
          (when (plist-get message :display)
            (piem--display-custom-message
             (plist-get message :content))))
         (_
          ;; Assistant message - show header if needed, reset markers
          (unless piem--assistant-header-shown
            (piem--append-to-chat
             (concat "\n" (piem--make-separator "Assistant") "\n"))
            (setq piem--assistant-header-shown t))
          (piem--set-message-start-marker (copy-marker (point-max) nil))
          (piem--set-streaming-marker (copy-marker (point-max) t))))))
    ("message_update"
     (when-let* ((msg-event (plist-get event :assistantMessageEvent))
                 (event-type (plist-get msg-event :type)))
       (pcase event-type
         ("text_start") ; No-op: text block started, nothing to render
         ("text_delta"
          (piem--set-activity-phase "replying")
          (piem--display-message-delta (plist-get msg-event :delta)))
         ("text_end"
          ;; Text block ended — finalize any active table that may have
          ;; a trailing row without newline (backstop for streaming).
          (piem--maybe-decorate-streaming-table)
          (setq piem--streaming-table-candidate nil))
         ("thinking_start"
          (piem--display-thinking-start))
         ("thinking_delta"
          (piem--display-thinking-delta (plist-get msg-event :delta)))
         ("thinking_end"
          (piem--display-thinking-end (plist-get msg-event :content)))
         ((or "toolcall_start" "toolcall_delta" "toolcall_end")
          (piem--set-activity-phase "running")
          (piem--handle-toolcall-message-event msg-event))
         ("error"
          ;; Error during streaming (e.g., API error)
          (piem--display-error (plist-get msg-event :reason))))))
    ("message_end"
     (let* ((message (plist-get event :message))
            (assistant-p (equal (plist-get message :role) "assistant")))
       ;; Display error if message ended with error (e.g., API error)
       (when (equal (plist-get message :stopReason) "error")
         (piem--display-error (plist-get message :errorMessage)))
       ;; The completed assistant message is authoritative over streamed
       ;; preview identities, arguments, and membership.
       (when assistant-p
         (when (plist-member message :content)
           (piem--reconcile-toolcall-previews message))
         (piem--reset-toolcall-streams)
         (piem--refresh-header)))
     (piem--render-complete-message))
    ("tool_execution_start"
     (piem--set-activity-phase "running")
     (let* ((tool-call-id (plist-get event :toolCallId))
            (args (plist-get event :args))
            (block (piem--tool-block-get tool-call-id)))
       ;; Cache args for tool_execution_end (which doesn't include args)
       (when (and tool-call-id piem--tool-args-cache)
         (puthash tool-call-id args piem--tool-args-cache))
       ;; Reuse the keyed preview block when it already exists.
       (unless block
         (setq block (piem--display-tool-start
                      (plist-get event :toolName) args tool-call-id)))
       ;; Update header and path from authoritative args.
       ;; During streaming, the header may show placeholders since delta
       ;; args can be partial.  Execution start carries the real args.
       (piem--display-tool-update-header
        (plist-get event :toolName) args block)
       (piem--tool-block-sync-path-metadata
        block (piem--tool-arg-path args))))
    ("tool_execution_end"
     (piem--set-activity-phase "thinking")
     (let* ((tool-call-id (plist-get event :toolCallId))
            (result (plist-get event :result))
            ;; A keyed miss must not finalize an unrelated legacy block.
            (block (or (piem--tool-block-get tool-call-id)
                       (and (piem--tool-call-id-p tool-call-id)
                            (piem--display-tool-start
                             (plist-get event :toolName) nil tool-call-id))))
            ;; Retrieve cached args since tool_execution_end doesn't include args
            (args (when (and tool-call-id piem--tool-args-cache)
                    (prog1 (gethash tool-call-id piem--tool-args-cache)
                      (remhash tool-call-id piem--tool-args-cache)))))
       ;; The authoritative result supersedes any pending preview; discard
       ;; it first so completion renders exactly once.
       (piem--discard-pending-tool-update tool-call-id)
       (piem--display-tool-end (plist-get event :toolName)
                                          args
                                          (plist-get result :content)
                                          (plist-get result :details)
                                          (plist-get event :isError)
                                          block)))
    ("tool_execution_update"
     (let ((tool-call-id (plist-get event :toolCallId))
           (partial-result (plist-get event :partialResult)))
       (if tool-call-id
           ;; Coalesce: keep only the latest preview per tool call and
           ;; render at the flush cadence, not once per event.
           (piem--queue-tool-update tool-call-id partial-result)
         ;; No toolCallId: render through the legacy compatibility block.
         (piem--display-tool-update partial-result nil))))
    ("compaction_start"
     (piem--cancel-followup-drain-timer)
     (piem--set-activity-phase "compact")
     (message (if (equal (plist-get event :reason) "overflow")
                  "Pi: Context overflow, compacting..."
                "Pi: Compacting...")))
    ("compaction_end"
     (piem--cancel-followup-drain-timer)
     (piem--handle-compaction-end-event event))
    ("agent_end"
     ;; Defensively drop pending previews and cancel the flush timer; any
     ;; tool still running here is aborted and its block is finalized below.
     (piem--cancel-tool-update-flush)
     (piem--set-canonical-messages
      (plist-get piem--state :messages))
     (piem--display-agent-end)
     (piem--update-hot-tail-boundary)
     (piem--queue-tool-cooling-outside-hot-tail))
    ("auto_retry_start"
     (piem--cancel-followup-drain-timer)
     (piem--display-retry-start event))
    ("auto_retry_end"
     (piem--display-retry-end event)
     (unless (eq (plist-get event :success) t)
       (piem--set-activity-phase "idle")
       (piem--restore-followup-queue-to-input)))
    ("extension_error"
     (piem--display-extension-error event))
    ("extension_ui_request"
     (piem--handle-extension-ui-request event))))


;;;; Tool Output

(defun piem--truncate-to-visual-lines (content max-lines width)
  "Truncate CONTENT to fit within MAX-LINES visual lines at WIDTH.
Also respects `piem-preview-max-bytes'.
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
         (max-bytes piem-preview-max-bytes)
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

(defun piem--clear-render-artifacts ()
  "Delete pi-owned render artifacts in the current chat buffer.
This removes completed/pending tool overlays, diff overlays, and lightweight
cold-tool metadata before buffer reset or history rebuild, then clears keyed
live-tool state, cached execution args, pending coalesced tool updates, and
the compatibility pending overlay slot so buffer and render state stay
consistent.  Pending deferred cooling is invalidated first.  Tree-sitter
overlays are left alone."
  (piem--cancel-tool-update-flush)
  (piem--cancel-tool-cooling)
  (remove-overlays (point-min) (point-max) 'piem-tool-block t)
  (remove-overlays (point-min) (point-max) 'piem-diff-overlay t)
  (let ((inhibit-read-only t))
    (remove-text-properties
     (point-min) (point-max) '(piem-cold-tool-block nil)))
  (setq piem--pending-tool-overlay nil
        piem--tool-block-order-counter 0
        piem--thinking-block-order-counter 0)
  (when piem--tool-args-cache
    (clrhash piem--tool-args-cache))
  (when piem--live-tool-blocks
    (clrhash piem--live-tool-blocks))
  (piem--reset-toolcall-streams))

(cl-defstruct (piem--tool-block
               (:constructor piem--make-tool-block))
  tool-call-id
  overlay
  header-end
  end-marker
  order
  path
  raw-path
  path-error
  offset
  line-map
  last-tail
  image-previews)

(cl-defstruct (piem--toolcall-stream
               (:conc-name piem--tool-stream-)
               (:constructor piem--make-toolcall-stream))
  "Incremental preview state for one streamed tool call."
  content-index
  tool-call-id
  tool-name
  block
  arguments
  rendered-header-key
  content-render-p
  content-truncated
  (depth 0)
  (root-state 'root)
  string-role
  (string-value "")
  current-key
  escape-state
  (unicode-value 0)
  (unicode-digits 0)
  high-surrogate)

(defun piem--ensure-live-tool-blocks ()
  "Return the live tool block registry for the current buffer."
  (or piem--live-tool-blocks
      (setq piem--live-tool-blocks
            (make-hash-table :test 'equal))))

(defun piem--next-tool-block-order ()
  "Return the next monotonically increasing live tool block order."
  (let ((order (or piem--tool-block-order-counter 0)))
    (setq piem--tool-block-order-counter (1+ order))
    order))

(defun piem--reserve-tool-block-order (&optional order)
  "Return ORDER, or allocate the next implicit live tool block order.
When ORDER is non-nil, advance the monotonic counter past it so later
implicit insertions still sort after explicitly ordered preview blocks."
  (if order
      (progn
        (setq piem--tool-block-order-counter
              (max (or piem--tool-block-order-counter 0)
                   (1+ order)))
        order)
    (piem--next-tool-block-order)))

(defun piem--tool-call-id-p (tool-call-id)
  "Return non-nil when TOOL-CALL-ID is usable for event correlation."
  (and (stringp tool-call-id) (not (string-empty-p tool-call-id))))

(defun piem--tool-block-get (tool-call-id)
  "Return the live tool block for TOOL-CALL-ID, or nil."
  (when (and (piem--tool-call-id-p tool-call-id)
             piem--live-tool-blocks)
    (gethash tool-call-id piem--live-tool-blocks)))

(defun piem--live-tool-blocks-in-order ()
  "Return all keyed and generation-owned tool blocks in display order."
  (let (blocks)
    (when piem--live-tool-blocks
      (maphash
       (lambda (_tool-call-id block)
         (when (overlay-buffer (piem--tool-block-overlay block))
           (push block blocks)))
       piem--live-tool-blocks))
    (when piem--toolcall-streams
      (maphash
       (lambda (_content-index stream)
         (when-let* ((block (piem--tool-stream-block stream))
                     (overlay (piem--tool-block-overlay block))
                     ((overlay-buffer overlay)))
           (unless (memq block blocks)
             (push block blocks))))
       piem--toolcall-streams))
    (sort blocks
          (lambda (left right)
            (< (piem--tool-block-order left)
               (piem--tool-block-order right))))))

(defun piem--tool-block-next-after-order (order)
  "Return the first live tool block whose order is greater than ORDER."
  (seq-find (lambda (block)
              (> (piem--tool-block-order block) order))
            (piem--live-tool-blocks-in-order)))

(defun piem--tool-block-register (block)
  "Register BLOCK when it has a nonempty authoritative tool call ID."
  (when-let* ((tool-call-id (piem--tool-block-tool-call-id block))
              ((piem--tool-call-id-p tool-call-id)))
    (puthash tool-call-id block (piem--ensure-live-tool-blocks)))
  block)

(defun piem--tool-block-unregister (block)
  "Remove BLOCK from the keyed live registry."
  (when-let* ((tool-call-id (piem--tool-block-tool-call-id block))
              (live-blocks piem--live-tool-blocks))
    (when (eq (gethash tool-call-id live-blocks) block)
      (remhash tool-call-id live-blocks)))
  block)

(defun piem--tool-block-from-overlay (overlay)
  "Return the tool block record attached to OVERLAY, or nil."
  (and overlay (overlay-get overlay 'piem-tool-block-record)))

(defun piem--current-tool-block ()
  "Return the compatibility current tool block, or nil.
This is only used by legacy single-tool rendering paths that still rely
on `piem--pending-tool-overlay'."
  (piem--tool-block-from-overlay piem--pending-tool-overlay))

(defun piem--all-live-tool-blocks ()
  "Return all distinct live tool blocks in the current buffer.
Includes keyed blocks from `piem--live-tool-blocks' and, when
needed for compatibility, the current non-keyed pending block."
  (let ((blocks (piem--live-tool-blocks-in-order))
        (current (piem--current-tool-block)))
    (if (and current (not (memq current blocks)))
        (append blocks (list current))
      blocks)))

(defun piem--finalize-live-tool-blocks (face)
  "Finalize every currently live tool block with FACE."
  (dolist (block (piem--all-live-tool-blocks))
    (piem--tool-block-finalize block face)))

(defun piem--tool-block-overlays-in-region (start end)
  "Return tool block overlays overlapping START..END in buffer order."
  (sort (seq-filter (lambda (ov)
                      (overlay-get ov 'piem-tool-block))
                    (overlays-in start end))
        (lambda (left right)
          (< (overlay-start left) (overlay-start right)))))

(defun piem--tool-block-refresh-overlay (block)
  "Sync BLOCK metadata and bounds onto its overlay."
  (when-let* ((ov (piem--tool-block-overlay block))
              (end-marker (piem--tool-block-end-marker block)))
    (move-overlay ov (overlay-start ov) (marker-position end-marker))
    (overlay-put ov 'piem-tool-block-record block)
    (overlay-put ov 'piem-header-end
                 (piem--tool-block-header-end block))
    (overlay-put ov 'piem-tool-path
                 (piem--tool-block-path block))
    (overlay-put ov 'piem-tool-raw-path
                 (piem--tool-block-raw-path block))
    (overlay-put ov 'piem-tool-path-error
                 (piem--tool-block-path-error block))
    (overlay-put ov 'piem-tool-offset
                 (piem--tool-block-offset block))
    (overlay-put ov 'piem-line-map
                 (piem--tool-block-line-map block))
    (overlay-put ov 'piem-last-tail
                 (piem--tool-block-last-tail block)))
  block)

(defun piem--tool-emacs-path (path)
  "Return Pi tool PATH normalized for Emacs in the current chat session."
  (piem--emacs-path
   path
   (piem--chat-session-directory)))

(defun piem--tool-arg-get (args prop)
  "Return PROP from ARGS without signaling during passive rendering."
  (when (listp args)
    (condition-case nil
        (plist-get args prop)
      (error nil))))

(defun piem--tool-arg-member (args prop)
  "Return non-nil when PROP is present in ARGS, without signaling."
  (when (listp args)
    (condition-case nil
        (plist-member args prop)
      (error nil))))

(defun piem--tool-arg-path (args)
  "Extract path metadata from ARGS without signaling."
  (or (piem--tool-arg-get args :path)
      (piem--tool-arg-get args :file_path)))

(defun piem--tool-path-string (path)
  "Return PATH when it is a nonempty NUL-free string, otherwise nil."
  (and (stringp path)
       (not (string-empty-p path))
       (not (piem--path-string-contains-nul-p path))
       path))

(defun piem--render-safe-string (value &optional nil-value)
  "Return VALUE as a string safe for render string APIs.
Nil becomes NIL-VALUE, or the empty string when NIL-VALUE is nil.
This helper does not escape display controls; callers that render metadata in
headers should also use `piem--escape-control-chars-for-display'."
  (cond
   ((stringp value) value)
   ((null value) (or nil-value ""))
   (t
    (condition-case nil
        (format "%s" value)
      (error "#<unprintable>")))))

(defun piem--content-block-list (content)
  "Return CONTENT as a list of plist blocks, or nil when malformed."
  (cond
   ((vectorp content)
    (cl-remove-if-not #'consp (append content nil)))
   ((and (listp content)
         (or (null content) (consp (car content))))
    (cl-remove-if-not #'consp content))
   (t nil)))

(defun piem--unicode-escape-char (char)
  "Return a display escape for Unicode CHAR."
  (if (<= char #xffff)
      (format "\\u%04X" char)
    (format "\\U%08X" char)))

(defun piem--escape-control-chars-for-display (text &optional preserve-newlines)
  "Return TEXT with control and format chars escaped for safe display.
When PRESERVE-NEWLINES is non-nil, leave newline separators untouched.
This helper is display-only; callers must keep raw metadata separately."
  (when (stringp text)
    (mapconcat
     (lambda (char)
       (let ((category (get-char-code-property char 'general-category)))
         (cond
          ((eq char ?\n) (if preserve-newlines "\n" "\\n"))
          ((eq char ?\r) "\\r")
          ((eq char ?\t) "\\t")
          ((or (< char 32) (= char #x7f))
           (format "\\x%02X" char))
          ((or (eq category 'Cc)
               (eq category 'Cf))
           (piem--unicode-escape-char char))
          (t (char-to-string char)))))
     text
     "")))

(defun piem--tool-display-value-string (value &optional placeholder)
  "Return VALUE escaped for one-line tool header display.
Nil becomes PLACEHOLDER.  Non-string values are stringified first so malformed
backend metadata cannot make passive rendering signal."
  (cond
   ((stringp value)
    (piem--escape-control-chars-for-display value))
   ((null value) placeholder)
   (t
    (piem--escape-control-chars-for-display
     (piem--render-safe-string value)))))

(defun piem--tool-display-path-string (path)
  "Return PATH escaped for one-line tool header display, or nil.
NUL-containing and non-string paths remain absent, so malformed path metadata
renders with the normal absent-path placeholder."
  (when-let* ((path (piem--tool-path-string path)))
    (piem--escape-control-chars-for-display path)))

(defun piem--tool-render-path-metadata (path)
  "Return passive render metadata for backend tool PATH.
The returned plist may contain:

`:path'       normalized Emacs path safe for navigation, or nil;
`:raw-path'   original backend path string, or nil;
`:path-error' user-facing error string when PATH was present but unsafe.

Never signal for invalid backend metadata; keep strict validation inside
`piem--tool-emacs-path' and record any failure here."
  (cond
   ((null path) nil)
   ((not (stringp path))
    (list :path-error "Tool path metadata is not a string"))
   ((string-empty-p path) nil)
   (t
    (condition-case err
        (list :path (piem--tool-emacs-path path)
              :raw-path path)
      (error
       (list :raw-path path
             :path-error (piem--escape-control-chars-for-display
                          (error-message-string err))))))))

(defun piem--tool-render-path (path)
  "Return PATH normalized for passive tool rendering, or nil.
Never signal for invalid backend metadata."
  (plist-get (piem--tool-render-path-metadata path) :path))

(defun piem--tool-block-sync-path-metadata (block path)
  "Sync PATH-derived navigation metadata on BLOCK and its overlay.
A nil, missing, or empty PATH clears stale safe-path, raw-path, and error
metadata.  Invalid PATH values clear the safe path and store controlled raw
path/error metadata for `piem-visit-file'."
  (when block
    (let ((metadata (piem--tool-render-path-metadata path)))
      (setf (piem--tool-block-path block)
            (plist-get metadata :path)
            (piem--tool-block-raw-path block)
            (plist-get metadata :raw-path)
            (piem--tool-block-path-error block)
            (plist-get metadata :path-error)))
    (piem--tool-block-refresh-overlay block))
  block)

(defun piem--tool-block-set-offset (block offset)
  "Store OFFSET metadata on BLOCK and its overlay."
  (when block
    (setf (piem--tool-block-offset block) offset)
    (piem--tool-block-refresh-overlay block))
  block)

(defun piem--tool-block-set-line-map (block line-map)
  "Store LINE-MAP metadata on BLOCK and its overlay."
  (when block
    (setf (piem--tool-block-line-map block) line-map)
    (piem--tool-block-refresh-overlay block))
  block)

(defun piem--tool-block-set-last-tail (block last-tail)
  "Store LAST-TAIL preview cache metadata on BLOCK and its overlay."
  (when block
    (setf (piem--tool-block-last-tail block) last-tail)
    (piem--tool-block-refresh-overlay block))
  block)

(defun piem--tool-block-set-image-previews (block previews)
  "Store rendered image PREVIEWS on BLOCK for toggling and cooling."
  (when block
    (setf (piem--tool-block-image-previews block) previews))
  block)

(defun piem--image-type-for-mime (mime-type)
  "Return the Emacs image type for MIME-TYPE, or nil when unsupported."
  (pcase mime-type
    ((or "image/jpeg" "image/jpg") 'jpeg)
    ("image/png" 'png)
    ("image/gif" 'gif)
    ("image/webp" 'webp)
    ("image/svg+xml" 'svg)))

(defun piem--image-preview-window ()
  "Return the window whose frame and dimensions govern image previews."
  (or (piem--chat-display-window)
      (selected-window)))

(defun piem--image-display-capable-p ()
  "Return non-nil when the image preview window is graphical."
  (display-images-p
   (window-frame (piem--image-preview-window))))

(defun piem--image-preview-pixel-limits ()
  "Return window-relative image preview size properties."
  (let* ((window (piem--image-preview-window))
         (window-width
          (max 1 (truncate (* 0.9 (window-pixel-width window)))))
         (configured-width
          (if (natnump piem-image-preview-max-width)
              (max 1 piem-image-preview-max-width)
            window-width)))
    (list :max-width (min window-width configured-width)
          :max-height
          (max 1 (truncate (* 0.5 (window-pixel-height window)))))))

(defconst piem--image-previews-per-content-limit 8
  "Maximum image blocks rendered from one message or tool result.")

(defun piem--image-preview-byte-limit ()
  "Return the nonnegative source-byte limit for one image preview."
  (if (natnump piem-image-preview-max-bytes)
      piem-image-preview-max-bytes
    (* 10 1024 1024)))

(defun piem--image-preview-safe-field (value &optional fallback)
  "Return VALUE as control-safe placeholder text, or FALLBACK when nil."
  (piem--escape-control-chars-for-display
   (piem--render-safe-string value fallback)))

(defun piem--image-preview-string (fallback &optional image)
  "Return FALLBACK marked as an image preview, displaying IMAGE when non-nil."
  (apply #'propertize fallback
         (append
          (list 'face 'piem-tool-header
                'fontified t
                'piem-no-fontify t
                'piem-image-preview t
                'rear-nonsticky t
                'help-echo fallback)
          (when image (list 'display image)))))

(defun piem--image-preview-label (mime-type description)
  "Return an image placeholder for MIME-TYPE and DESCRIPTION."
  (format "Image: %s, %s"
          (piem--image-preview-safe-field mime-type "unknown")
          description))

(defun piem--image-too-large-description (limit)
  "Return a placeholder description for an image exceeding LIMIT bytes."
  (format "too large (limit %s)"
          (file-size-human-readable limit 'iec " " "B")))

(defun piem--image-data-matches-type-p (data type)
  "Return non-nil when Emacs recognizes DATA as the expected image TYPE."
  (eq type (condition-case nil
               (image-type-from-data data)
             (error nil))))

(defun piem--render-content-image-preview (block)
  "Return a rendered preview string for image content BLOCK."
  (let* ((mime-type (or (plist-get block :mimeType)
                        (plist-get block :mime-type)))
         (data (plist-get block :data))
         (type (piem--image-type-for-mime mime-type))
         (limit (piem--image-preview-byte-limit)))
    (cond
     ((or (null data) (and (stringp data) (string-empty-p data)))
      (piem--image-preview-string
       (piem--image-preview-label mime-type "empty data")))
     ((not (stringp data))
      (piem--image-preview-string
       (piem--image-preview-label mime-type "decode error")))
     ((> (string-bytes data) (* 2 limit))
      (piem--image-preview-string
       (piem--image-preview-label
        mime-type (piem--image-too-large-description limit))))
     (t
      (condition-case nil
          (let* ((raw (base64-decode-string data))
                 (raw-bytes (length raw))
                 (size (file-size-human-readable raw-bytes 'iec " " "B")))
            (cond
             ((string-empty-p raw)
              (piem--image-preview-string
               (piem--image-preview-label mime-type "empty data")))
             ((> raw-bytes limit)
              (piem--image-preview-string
               (piem--image-preview-label
                mime-type (piem--image-too-large-description limit))))
             ((not type)
              (piem--image-preview-string
               (piem--image-preview-label
                mime-type (format "%s, unsupported type" size))))
             ((not (piem--image-data-matches-type-p raw type))
              (piem--image-preview-string
               (piem--image-preview-label
                mime-type (format "%s, invalid data" size))))
             ((not (piem--image-display-capable-p))
              (piem--image-preview-string
               (piem--image-preview-label mime-type size)))
             ((not (image-type-available-p type))
              (piem--image-preview-string
               (piem--image-preview-label
                mime-type (format "%s, unavailable" size))))
             (t
              (condition-case nil
                  (let ((image (apply #'create-image raw type t
                                      (piem--image-preview-pixel-limits))))
                    (if image
                        (piem--image-preview-string
                         (piem--image-preview-label mime-type size)
                         image)
                      (piem--image-preview-string
                       (piem--image-preview-label
                        mime-type "display error"))))
                (error
                 (piem--image-preview-string
                  (piem--image-preview-label
                   mime-type "display error")))))))
        (error
         (piem--image-preview-string
          (piem--image-preview-label mime-type "decode error"))))))))

(defun piem--content-image-previews (content)
  "Render a bounded number of image blocks from vector or list CONTENT."
  (let* ((blocks
          (seq-filter
           (lambda (block) (equal (plist-get block :type) "image"))
           (piem--content-block-list content)))
         (limit (max 0 piem--image-previews-per-content-limit))
         (shown (seq-take blocks limit))
         (omitted (- (length blocks) (length shown))))
    (append
     (mapcar #'piem--render-content-image-preview shown)
     (when (> omitted 0)
       (list
        (piem--image-preview-string
         (format "Image: %d additional preview%s omitted"
                 omitted (if (= omitted 1) "" "s"))))))))

(defun piem--svg-fragment-links-only-p (source)
  "Return non-nil when every href in SVG SOURCE is a local fragment."
  (let ((case-fold-search t)
        (without-local-links source)
        (local-link-re
         "\\_<\\(?:[[:alnum:]_.-]+:\\)?href[[:space:]]*=[[:space:]]*\\([\"']\\)#[^\"']*\\1"))
    (setq without-local-links
          (replace-regexp-in-string local-link-re "" without-local-links))
    (not (string-match-p
          "\\_<\\(?:[[:alnum:]_.-]+:\\)?href[[:space:]]*="
          without-local-links))))

(defun piem--standalone-svg-p (source)
  "Return non-nil for a complete SVG SOURCE without obvious resources."
  (let ((case-fold-search t))
    (and (eq 'svg (condition-case nil
                      (image-type-from-data source)
                    (error nil)))
         (or (string-match-p "</svg[[:space:]]*>[[:space:]]*\\'" source)
             (string-match-p "<svg\\_>[^>]*?/>[[:space:]]*\\'" source))
         (not (string-match-p
               "\\(?:<[[:space:]]*\\(?:script\\|foreignobject\\|image\\|feimage\\)\\_>\\|url[[:space:]]*(\\|@import\\|<!doctype\\|<!entity\\)"
               source))
         (piem--svg-fragment-links-only-p source))))

(defun piem--read-svg-preview
    (tool-name args raw-output details is-error)
  "Render a complete standalone SVG returned as READ's RAW-OUTPUT.
TOOL-NAME, ARGS, DETAILS, and IS-ERROR describe the completed result."
  (when (and (equal tool-name "read")
             (not is-error)
             (not (piem--tool-arg-get args :offset))
             (not (piem--tool-arg-get args :limit))
             (let ((truncation
                    (piem--tool-arg-get details :truncation)))
               (or (null truncation)
                   (piem--json-null-p truncation)))
             (stringp raw-output)
             (piem--standalone-svg-p raw-output))
    (let* ((bytes (string-bytes raw-output))
           (limit (piem--image-preview-byte-limit))
           (size (file-size-human-readable bytes 'iec " " "B"))
           (fallback (piem--image-preview-label
                      "image/svg+xml" size)))
      (cond
       ((> bytes limit)
        (piem--image-preview-string
         (piem--image-preview-label
          "image/svg+xml" (piem--image-too-large-description limit))))
       ((not (piem--image-display-capable-p))
        (piem--image-preview-string fallback))
       ((not (image-type-available-p 'svg))
        (piem--image-preview-string
         (piem--image-preview-label
          "image/svg+xml" (format "%s, unavailable" size))))
       (t
        (condition-case nil
            (let ((image
                   (apply #'create-image raw-output 'svg t
                          (append (piem--image-preview-pixel-limits)
                                  '(:base-uri "data:" :scale 1)))))
              (piem--image-preview-string fallback image))
          (error
           (piem--image-preview-string
            (piem--image-preview-label
             "image/svg+xml" (format "%s, display error" size))))))))))

(defun piem--insert-image-previews (previews)
  "Insert rendered image PREVIEWS at point, one per line."
  (dolist (preview previews)
    (insert preview "\n")))

(defun piem--image-previews-text (previews)
  "Return rendered PREVIEWS as newline-terminated text, or nil."
  (when previews
    (concat (mapconcat #'identity previews "\n") "\n")))

(defun piem--tool-block-create
    (tool-name args &optional tool-call-id order preview-state path-metadata-policy)
  "Insert a live tool block for TOOL-NAME with ARGS and return it.
When TOOL-CALL-ID is non-nil, register the block in the keyed live
registry.  ORDER records the intended block ordering metadata.
When PREVIEW-STATE is `streaming', generic tool headers omit ARGS.
When PATH-METADATA-POLICY is `defer', leave navigation metadata absent
until an authoritative tool execution/history event supplies it."
  (let* ((block-order (piem--reserve-tool-block-order order))
         (next-block (and order
                          (piem--tool-block-next-after-order
                           block-order)))
         (next-overlay (and next-block
                            (piem--tool-block-overlay next-block)))
         (header-display (piem--tool-header tool-name args preview-state))
         (block nil)
         (inhibit-read-only t))
    (piem--with-scroll-preservation
      (save-excursion
        (goto-char (if next-overlay
                       (overlay-start next-overlay)
                     (point-max)))
        (piem--ensure-blank-line-before-block)
        (let ((start (point)))
          (insert header-display "\n")
          (let* ((header-end (copy-marker (point) nil))
                 ;; Keep the body-end marker fixed on unrelated inserts at the
                 ;; block boundary.  Live updates move it explicitly.
                 (end-marker (copy-marker (point) nil)))
            ;; When inserting before an already-live later block, keep one
            ;; blank separator after the new block without making it part of
            ;; the tool block overlay itself.
            (when next-overlay
              (insert "\n")
              ;; Insertion at an overlay's front boundary normally makes the
              ;; new text part of that overlay.  Restore the later block's
              ;; start to its shifted header so the two blocks stay disjoint.
              (move-overlay next-overlay (point) (overlay-end next-overlay)))
            (let ((ov (make-overlay start (marker-position end-marker) nil nil nil)))
              (overlay-put ov 'piem-tool-block t)
              (overlay-put ov 'piem-tool-name tool-name)
              (overlay-put ov 'face 'piem-tool-block)
              (setq block (piem--make-tool-block
                           :tool-call-id tool-call-id
                           :overlay ov
                           :header-end header-end
                           :end-marker end-marker
                           :order block-order))
              (if (eq path-metadata-policy 'defer)
                  (piem--tool-block-refresh-overlay block)
                (piem--tool-block-sync-path-metadata
                 block (piem--tool-arg-path args))))))))
    (setq piem--pending-tool-overlay
          (piem--tool-block-overlay block))
    (piem--tool-block-register block)))

(defun piem--tool-block-finalize (block face)
  "Finalize BLOCK with FACE and remove it from the live keyed registry."
  (when-let* ((block block)
              (ov (piem--tool-block-overlay block)))
    (when-let* ((end-marker (piem--tool-block-end-marker block)))
      (set-marker-insertion-type end-marker nil))
    (overlay-put ov 'face face)
    (piem--tool-block-refresh-overlay block)
    (piem--tool-block-unregister block)
    (when (eq piem--pending-tool-overlay ov)
      (setq piem--pending-tool-overlay nil)))
  block)

(defun piem--tool-block-delete (block)
  "Delete BLOCK's text and overlay, then remove it from live state."
  (when-let* ((block block)
              (ov (piem--tool-block-overlay block))
              (start (overlay-start ov))
              (end (overlay-end ov)))
    (let* ((previous-tool (and (> start (point-min))
                               (seq-find (lambda (other)
                                           (overlay-get other 'piem-tool-block))
                                         (overlays-at (1- start)))))
           (next-tool (and (< end (point-max))
                           (seq-find (lambda (other)
                                       (overlay-get other 'piem-tool-block))
                                     (overlays-at (1+ end)))))
           (delete-start start)
           (delete-end end)
           (inhibit-read-only t))
      (cond
       ((and next-tool (eq (char-after end) ?\n))
        (setq delete-end (1+ end)))
       ((and previous-tool (eq (char-before start) ?\n))
        (setq delete-start (1- start))))
      (piem--with-scroll-preservation
        (delete-region delete-start delete-end))
      (delete-overlay ov)
      (when-let* ((header-end (piem--tool-block-header-end block)))
        (set-marker header-end nil))
      (when-let* ((end-marker (piem--tool-block-end-marker block)))
        (set-marker end-marker nil))
      (piem--tool-block-unregister block)
      (when (eq piem--pending-tool-overlay ov)
        (setq piem--pending-tool-overlay nil)
        (when-let* ((last-block (car (last (piem--live-tool-blocks-in-order)))))
          (setq piem--pending-tool-overlay
                (piem--tool-block-overlay last-block))))))
  nil)

(defun piem--tool-overlay-finalize (face &optional block)
  "Finalize BLOCK, or the current pending tool block, with FACE."
  (piem--tool-block-finalize
   (or block (piem--current-tool-block))
   face))

(defun piem--pretty-print-json (plist-data)
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

(defun piem--propertize-details-region (details-json)
  "Return DETAILS-JSON as a details section marked as metadata.
The details payload keeps tool-output styling while setting
`piem-no-fontify' so explicit markdown fontification
can safely skip this region."
  (propertize (concat "**Details**\n" details-json)
              'font-lock-face 'piem-tool-output
              'piem-no-fontify t))

(defun piem--tool-header (tool-name args &optional preview-state)
  "Return propertized header for tool TOOL-NAME with ARGS.
The tool name prefix uses `piem-tool-name' face and
the arguments use `piem-tool-command' face.
Built-in tools show specialized formats (e.g., \"$ cmd\" for bash).
Generic tools show JSON args: compact when the full header fits
within `fill-column', pretty-printed otherwise.
When PREVIEW-STATE is `streaming', generic tools show only their
name and do not parse or pretty-print ARGS.  Built-in tools ignore
PREVIEW-STATE and keep their compact streaming headers.
Uses `font-lock-face' to survive tree-sitter refontification."
  (let* ((raw-tool-name tool-name)
         (tool-name (or (piem--tool-display-value-string
                         tool-name "tool")
                        "tool"))
         (path (piem--tool-display-path-string
                (piem--tool-arg-path args))))
    (pcase raw-tool-name
      ("bash"
       (let ((cmd (piem--tool-display-value-string
                   (piem--tool-arg-get args :command)
                   "...")))
         (concat (propertize "$" 'font-lock-face 'piem-tool-name)
                 (propertize (concat " " cmd) 'font-lock-face 'piem-tool-command))))
      ((or "read" "write" "edit")
       (concat (propertize tool-name 'font-lock-face 'piem-tool-name)
               (propertize (concat " " (or path "...")) 'font-lock-face 'piem-tool-command)))
      (_
       (let ((name (propertize tool-name 'font-lock-face 'piem-tool-name)))
         (if (eq preview-state 'streaming)
             name
           (let* ((json-pretty (condition-case nil
                                   (when-let* ((json (piem--pretty-print-json args)))
                                     (piem--escape-control-chars-for-display
                                      json t))
                                 (error nil)))
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
                 (concat name (propertize (concat " " json) 'font-lock-face 'piem-tool-command))
               name))))))))

(defun piem--display-tool-start
    (tool-name args &optional tool-call-id order preview-state path-metadata-policy)
  "Insert a tool header for TOOL-NAME with ARGS and return its live block.
When TOOL-CALL-ID is non-nil, register the block in the keyed live
registry.  ORDER records ordering metadata for future reconciliation.
When PREVIEW-STATE is `streaming', generic tool headers omit ARGS.
PATH-METADATA-POLICY is forwarded to `piem--tool-block-create'."
  (piem--tool-block-create
   tool-name args tool-call-id order preview-state path-metadata-policy))

(defun piem--display-tool-update-header
    (tool-name args &optional block preview-state)
  "Update BLOCK's header for TOOL-NAME with ARGS.
When BLOCK is nil, fall back to the current compatibility tool block.
Replaces the header text when it has changed (e.g., when authoritative
args arrive at tool_execution_start after streaming placeholder).
When PREVIEW-STATE is `streaming', generic tool headers omit ARGS."
  (when-let* ((block (or block (piem--current-tool-block)))
              (ov (piem--tool-block-overlay block))
              (ov-start (overlay-start ov))
              (header-end (piem--tool-block-header-end block)))
    (let ((new-header (piem--tool-header tool-name args preview-state))
          (header-limit (1- (marker-position header-end))))
      (when (<= ov-start header-limit)
        (let ((old-header (buffer-substring-no-properties ov-start header-limit)))
          (unless (string= old-header (substring-no-properties new-header))
            (let ((inhibit-read-only t))
              (piem--with-scroll-preservation
                (save-excursion
                  (goto-char ov-start)
                  (delete-region ov-start header-limit)
                  (insert new-header)
                  ;; Keep HEADER-END after the preserved newline that separates
                  ;; header and body.  The marker already tracks that boundary
                  ;; across the delete/insert above; resetting it here would
                  ;; move it before the newline and glue future body content to
                  ;; the header.
                  (piem--tool-block-refresh-overlay block))))))))))

(defun piem--ensure-toolcall-streams ()
  "Return the current message's tool-call stream registry."
  (or piem--toolcall-streams
      (setq piem--toolcall-streams
            (make-hash-table :test 'eql))))

(defun piem--reset-toolcall-streams ()
  "Discard raw tool-call generation state for the current message."
  (when piem--toolcall-streams
    (clrhash piem--toolcall-streams)))

(defconst piem--toolcall-preview-metadata-limit 4096
  "Maximum characters retained for a streamed command or path preview.")

(defun piem--toolcall-preview-property (stream)
  "Return STREAM's preview property for its current top-level JSON key."
  (let ((key (piem--tool-stream-current-key stream)))
    (pcase (piem--tool-stream-tool-name stream)
      ("bash" (and (equal key "command") :command))
      ((or "read" "edit")
       (pcase key
         ("path" :path)
         ("file_path" :file_path)))
      ("write"
       (pcase key
         ("path" :path)
         ("file_path" :file_path)
         ("content" :content))))))

(defun piem--toolcall-stream-append-string (stream text)
  "Append decoded TEXT to STREAM's bounded interesting JSON string."
  (let ((role (piem--tool-stream-string-role stream)))
    (when (memq role '(key value))
      (unless (string-empty-p text)
        (setf (piem--tool-stream-high-surrogate stream) nil))
      (let* ((property (and (eq role 'value)
                            (piem--toolcall-preview-property
                             stream)))
             (limit (cond
                     ((eq role 'key) 64)
                     ((eq property :content)
                      (max 1 piem-preview-max-bytes))
                     (t piem--toolcall-preview-metadata-limit)))
             (combined
              (concat (piem--tool-stream-string-value stream)
                      text)))
        (when (and (eq property :content) (string-match-p "\n" text))
          (setf (piem--tool-stream-content-render-p stream) t))
        (setf (piem--tool-stream-string-value stream)
              (cond
               ((<= (length combined) limit) combined)
               ((eq property :content)
                (unless (piem--tool-stream-content-truncated stream)
                  (setf (piem--tool-stream-content-render-p stream)
                        t))
                (setf (piem--tool-stream-content-truncated stream)
                      t)
                (substring combined (- (length combined) limit)))
               (t (substring combined 0 limit))))))))

(defun piem--toolcall-stream-clear-value (stream)
  "Clear STREAM's preview value before consuming a later JSON value."
  (when-let* ((property (piem--toolcall-preview-property stream)))
    (setf (piem--tool-stream-arguments stream)
          (plist-put (piem--tool-stream-arguments stream)
                     property nil))
    (when (eq property :content)
      (setf (piem--tool-stream-content-render-p stream) t
            (piem--tool-stream-content-truncated stream)
            nil))))

(defun piem--toolcall-stream-publish-value (stream)
  "Publish STREAM's current top-level preview string into its arguments."
  (when-let* ((property (piem--toolcall-preview-property stream)))
    (setf (piem--tool-stream-arguments stream)
          (plist-put (piem--tool-stream-arguments stream)
                     property
                     (piem--tool-stream-string-value stream)))))

(defun piem--toolcall-stream-start-string (stream role)
  "Start a JSON string with ROLE in STREAM."
  (setf (piem--tool-stream-string-role stream) role
        (piem--tool-stream-string-value stream) ""
        (piem--tool-stream-escape-state stream) nil
        (piem--tool-stream-unicode-value stream) 0
        (piem--tool-stream-unicode-digits stream) 0
        (piem--tool-stream-high-surrogate stream) nil)
  (when (eq role 'value)
    (piem--toolcall-stream-publish-value stream)))

(defun piem--toolcall-stream-finish-string (stream)
  "Finish STREAM's current JSON string and advance its root parser."
  (let ((role (piem--tool-stream-string-role stream))
        (value (piem--tool-stream-string-value stream)))
    (when (eq role 'value)
      (piem--toolcall-stream-publish-value stream))
    (setf (piem--tool-stream-string-role stream) nil
          (piem--tool-stream-string-value stream) ""
          (piem--tool-stream-escape-state stream) nil
          (piem--tool-stream-high-surrogate stream) nil)
    (pcase role
      ('key
       (setf (piem--tool-stream-current-key stream) value
             (piem--tool-stream-root-state stream) 'colon))
      ((or 'value 'root-value)
       (setf (piem--tool-stream-root-state stream)
             'after-value)))))

(defun piem--json-hex-digit-value (char)
  "Return CHAR's hexadecimal value, or nil when CHAR is not hexadecimal."
  (cond
   ((and (>= char ?0) (<= char ?9)) (- char ?0))
   ((and (>= char ?a) (<= char ?f)) (+ 10 (- char ?a)))
   ((and (>= char ?A) (<= char ?F)) (+ 10 (- char ?A)))))

(defun piem--toolcall-stream-append-codepoint (stream codepoint)
  "Append decoded Unicode CODEPOINT to STREAM's current JSON string."
  (let ((high (piem--tool-stream-high-surrogate stream)))
    (cond
     ((and high (>= codepoint #xdc00) (<= codepoint #xdfff))
      (setf (piem--tool-stream-high-surrogate stream) nil)
      (piem--toolcall-stream-append-string
       stream
       (char-to-string
        (+ #x10000
           (ash (- high #xd800) 10)
           (- codepoint #xdc00)))))
     (high
      (setf (piem--tool-stream-high-surrogate stream) nil)
      (piem--toolcall-stream-append-codepoint stream codepoint))
     ((and (>= codepoint #xd800) (<= codepoint #xdbff))
      (setf (piem--tool-stream-high-surrogate stream)
            codepoint))
     ((and (>= codepoint #xdc00) (<= codepoint #xdfff)))
     ((<= codepoint #x10ffff)
      (piem--toolcall-stream-append-string
       stream (char-to-string codepoint))))))

(defun piem--toolcall-stream-feed-string-char (stream char)
  "Consume one JSON string CHAR for STREAM."
  (pcase (piem--tool-stream-escape-state stream)
    ('unicode
     (if-let* ((digit (piem--json-hex-digit-value char)))
         (let ((count (1+ (piem--tool-stream-unicode-digits
                           stream)))
               (value (+ (ash (piem--tool-stream-unicode-value
                               stream)
                              4)
                         digit)))
           (setf (piem--tool-stream-unicode-digits stream)
                 count
                 (piem--tool-stream-unicode-value stream)
                 value)
           (when (= count 4)
             (setf (piem--tool-stream-escape-state stream) nil
                   (piem--tool-stream-unicode-digits stream) 0
                   (piem--tool-stream-unicode-value stream) 0)
             (piem--toolcall-stream-append-codepoint stream value)))
       ;; Pi repairs malformed provider escapes by preserving the backslash.
       ;; Publish the literal prefix and reprocess CHAR so a quote still closes
       ;; the string rather than corrupting all later top-level fields.
       (let* ((count (piem--tool-stream-unicode-digits stream))
              (value (piem--tool-stream-unicode-value stream))
              (digits (if (= count 0)
                          ""
                        (format (format "%%0%dX" count) value))))
         (setf (piem--tool-stream-escape-state stream) nil
               (piem--tool-stream-unicode-digits stream) 0
               (piem--tool-stream-unicode-value stream) 0
               (piem--tool-stream-high-surrogate stream) nil)
         (piem--toolcall-stream-append-string
          stream (concat "\\u" digits))
         (piem--toolcall-stream-feed-string-char stream char))))
    ('escape
     (if (eq char ?u)
         (setf (piem--tool-stream-escape-state stream) 'unicode
               (piem--tool-stream-unicode-digits stream) 0
               (piem--tool-stream-unicode-value stream) 0)
       (setf (piem--tool-stream-escape-state stream) nil
             (piem--tool-stream-high-surrogate stream) nil)
       (piem--toolcall-stream-append-string
        stream
        (pcase char
          (?\" "\"")
          (?\\ "\\")
          (?/ "/")
          (?b "\b")
          (?f "\f")
          (?n "\n")
          (?r "\r")
          (?t "\t")
          (_ (concat "\\" (char-to-string char)))))))
    (_
     (cond
      ((eq char ?\\)
       (setf (piem--tool-stream-escape-state stream) 'escape))
      ((eq char ?\")
       (piem--toolcall-stream-finish-string stream))
      (t
       (setf (piem--tool-stream-high-surrogate stream) nil)
       (piem--toolcall-stream-append-string
        stream (char-to-string char)))))))

(defun piem--json-whitespace-char-p (char)
  "Return non-nil when CHAR is JSON whitespace."
  (memq char '(32 9 10 13)))

(defun piem--toolcall-stream-feed-structure-char (stream char)
  "Consume one non-string JSON CHAR for STREAM's top-level object parser."
  (let ((depth (piem--tool-stream-depth stream)))
    (cond
     ((= depth 0)
      (when (eq char ?{)
        (setf (piem--tool-stream-depth stream) 1
              (piem--tool-stream-root-state stream) 'key)))
     ((> depth 1)
      (cond
       ((eq char ?\")
        (piem--toolcall-stream-start-string stream 'nested))
       ((or (eq char ?{) (eq char ?\[))
        (setf (piem--tool-stream-depth stream) (1+ depth)))
       ((or (eq char ?}) (eq char ?\]))
        (let ((new-depth (1- depth)))
          (setf (piem--tool-stream-depth stream) new-depth)
          (when (= new-depth 1)
            (setf (piem--tool-stream-root-state stream)
                  'after-value))))))
     (t
      (pcase (piem--tool-stream-root-state stream)
        ('key
         (cond
          ((eq char ?\")
           (piem--toolcall-stream-start-string stream 'key))
          ((eq char ?})
           (setf (piem--tool-stream-depth stream) 0
                 (piem--tool-stream-root-state stream) 'done))))
        ('colon
         (when (eq char ?:)
           (setf (piem--tool-stream-root-state stream) 'value)))
        ('value
         (unless (piem--json-whitespace-char-p char)
           ;; JSON's last occurrence wins.  Clear a prior preview before
           ;; learning whether this later value is a string we can display.
           (piem--toolcall-stream-clear-value stream)
           (cond
            ((eq char ?\")
             (piem--toolcall-stream-start-string
              stream
              (if (piem--toolcall-preview-property stream)
                  'value
                'root-value)))
            ((or (eq char ?{) (eq char ?\[))
             (setf (piem--tool-stream-depth stream) 2
                   (piem--tool-stream-root-state stream) 'nested))
            ((eq char ?,)
             (setf (piem--tool-stream-root-state stream) 'key
                   (piem--tool-stream-current-key stream) nil))
            ((eq char ?})
             (setf (piem--tool-stream-depth stream) 0
                   (piem--tool-stream-root-state stream) 'done))
            (t
             (setf (piem--tool-stream-root-state stream)
                   'scalar)))))
        ('scalar
         (cond
          ((eq char ?,)
           (setf (piem--tool-stream-root-state stream) 'key
                 (piem--tool-stream-current-key stream) nil))
          ((eq char ?})
           (setf (piem--tool-stream-depth stream) 0
                 (piem--tool-stream-root-state stream) 'done))))
        ('after-value
         (cond
          ((eq char ?,)
           (setf (piem--tool-stream-root-state stream) 'key
                 (piem--tool-stream-current-key stream) nil))
          ((eq char ?})
           (setf (piem--tool-stream-depth stream) 0
                 (piem--tool-stream-root-state stream) 'done)))))))))

(defun piem--toolcall-stream-feed (stream delta)
  "Append raw argument JSON DELTA to STREAM's display-only preview state."
  (when (stringp delta)
    (let ((index 0)
          (length (length delta)))
      (while (< index length)
        (if (and (piem--tool-stream-string-role stream)
                 (null (piem--tool-stream-escape-state stream)))
            ;; Plain string runs dominate write payloads.  Append each run once
            ;; rather than repeatedly copying the accumulated value per char.
            (let ((special (string-match "[\"\\\\]" delta index)))
              (if special
                  (progn
                    (when (> special index)
                      (piem--toolcall-stream-append-string
                       stream (substring delta index special)))
                    (piem--toolcall-stream-feed-string-char
                     stream (aref delta special))
                    (setq index (1+ special)))
                (piem--toolcall-stream-append-string
                 stream (substring delta index))
                (setq index length)))
          (let ((char (aref delta index)))
            (if (piem--tool-stream-string-role stream)
                (piem--toolcall-stream-feed-string-char stream char)
              (piem--toolcall-stream-feed-structure-char stream char)))
          (setq index (1+ index)))))
    (when (eq (piem--tool-stream-string-role stream) 'value)
      (piem--toolcall-stream-publish-value stream)))
  stream)

(defun piem--tool-name-p (tool-name)
  "Return non-nil when TOOL-NAME can identify a streamed preview."
  (and (stringp tool-name) (not (string-empty-p tool-name))))

(defun piem--toolcall-complete-content (content)
  "Return the complete-line prefix of streamed write CONTENT."
  (when (stringp content)
    (if (string-suffix-p "\n" content)
        content
      (when-let* ((last-newline (cl-position ?\n content :from-end t)))
        (substring content 0 (1+ last-newline))))))

(defun piem--toolcall-stream-header-key (stream)
  "Return STREAM metadata whose change requires a header repaint."
  (let ((args (piem--tool-stream-arguments stream)))
    (pcase (piem--tool-stream-tool-name stream)
      ("bash" (piem--tool-arg-get args :command))
      ((or "read" "write" "edit")
       (piem--tool-arg-path args))
      (_ nil))))

(defun piem--render-toolcall-stream (stream _event-type)
  "Render changed, visible parts of generation STREAM."
  (when (piem--tool-name-p
         (piem--tool-stream-tool-name stream))
    (let* ((tool-name (piem--tool-stream-tool-name stream))
           (args (piem--tool-stream-arguments stream))
           (header-key (piem--toolcall-stream-header-key stream))
           (block (piem--tool-stream-block stream)))
      (unless block
        ;; Generation identity is contentIndex, not a provider ID that may be
        ;; empty, duplicated, or corrected at either authoritative end event.
        (setq block
              (piem--display-tool-start
               tool-name args nil
               (piem--tool-stream-content-index stream)
               'streaming 'defer))
        (setf (piem--tool-stream-block stream) block
              (piem--tool-stream-rendered-header-key stream)
              header-key))
      (setq piem--pending-tool-overlay
            (piem--tool-block-overlay block))
      (unless (equal header-key
                     (piem--tool-stream-rendered-header-key
                      stream))
        (piem--display-tool-update-header
         tool-name args block 'streaming)
        (setf (piem--tool-stream-rendered-header-key stream)
              header-key)
        (when (and (equal tool-name "write")
                   (piem--tool-arg-member args :content))
          (setf (piem--tool-stream-content-render-p stream)
                t)))
      ;; The write preview displays complete lines only.  Avoid rewriting its
      ;; fenced body for every token that merely extends a partial line.
      (when (and (equal tool-name "write")
                 (piem--tool-arg-member args :content)
                 (piem--tool-stream-content-render-p stream))
        (let* ((content (piem--tool-arg-get args :content))
               (complete-content
                (piem--toolcall-complete-content content)))
          (if (stringp content)
              (piem--display-tool-streaming-text
               (or complete-content "")
               piem-tool-preview-lines
               (piem--path-to-language
                (piem--tool-path-string
                 (piem--tool-arg-path args)))
               block
               (piem--tool-stream-content-truncated stream))
            (piem--clear-toolcall-preview-body block))
          (setf (piem--tool-stream-content-render-p stream)
                nil))))))

(defun piem--tool-block-rekey (block tool-call-id)
  "Change live BLOCK's registry key to authoritative TOOL-CALL-ID."
  (when (and block (piem--tool-call-id-p tool-call-id))
    (let ((changed
           (not (equal (piem--tool-block-tool-call-id block)
                       tool-call-id))))
      (when changed
        (piem--tool-block-unregister block)
        (setf (piem--tool-block-tool-call-id block) tool-call-id))
      ;; Registration may have been displaced by a colliding provisional ID.
      (piem--tool-block-register block)
      (when changed
        (piem--tool-block-refresh-overlay block))))
  block)

(defun piem--reconcile-final-toolcall
    (content-index tool-call &optional stream)
  "Reconcile authoritative TOOL-CALL at CONTENT-INDEX with STREAM preview."
  (let* ((tool-call-id (plist-get tool-call :id))
         (tool-name (plist-get tool-call :name))
         (args (plist-get tool-call :arguments))
         ;; Content index owns a generation stream.  If that stream has not
         ;; rendered yet (as in tagged Pi 0.84.2), create its block rather than
         ;; adopting another stream's colliding authoritative ID.
         (block (if stream
                    (or (piem--tool-stream-block stream)
                        (piem--display-tool-start
                         tool-name args nil content-index nil 'defer))
                  (piem--tool-block-get tool-call-id))))
    (when block
      (piem--tool-block-rekey block tool-call-id))
    (setq block
          (piem--reconcile-toolcall-preview-block
           content-index tool-call "toolcall_end" block))
    (when stream
      (setf (piem--tool-stream-tool-call-id stream) tool-call-id
            (piem--tool-stream-tool-name stream)
            (plist-get tool-call :name)
            (piem--tool-stream-arguments stream)
            (plist-get tool-call :arguments)
            (piem--tool-stream-block stream) block))
    block))

(defun piem--handle-toolcall-message-event (event)
  "Assemble and render one delta-only toolcall message EVENT."
  (let* ((event-type (plist-get event :type))
         (content-index (plist-get event :contentIndex))
         (streams (piem--ensure-toolcall-streams)))
    (pcase event-type
      ("toolcall_start"
       (when-let* ((old-stream (gethash content-index streams))
                   (old-block (piem--tool-stream-block
                               old-stream)))
         (piem--tool-block-delete old-block))
       (let ((stream (piem--make-toolcall-stream
                      :content-index content-index
                      :tool-call-id (plist-get event :id)
                      :tool-name (plist-get event :toolName))))
         (puthash content-index stream streams)
         (piem--render-toolcall-stream stream event-type)))
      ("toolcall_delta"
       (let ((stream (or (gethash content-index streams)
                         (let ((new-stream
                                (piem--make-toolcall-stream
                                 :content-index content-index)))
                           (puthash content-index new-stream streams)
                           new-stream))))
         ;; Generic tool arguments are hidden while streaming.  Scan only the
         ;; built-ins whose command/path/content drives a live preview.
         (when (member (piem--tool-stream-tool-name stream)
                       '("bash" "read" "edit" "write"))
           (piem--toolcall-stream-feed
            stream (plist-get event :delta)))
         (piem--render-toolcall-stream stream event-type)))
      ("toolcall_end"
       (when-let* ((tool-call (plist-get event :toolCall)))
         (let ((stream (or (gethash content-index streams)
                           (let ((new-stream
                                  (piem--make-toolcall-stream
                                   :content-index content-index)))
                             (puthash content-index new-stream streams)
                             new-stream))))
           ;; Keep compact content-index/block identity until message_end,
           ;; which may apply an extension-replaced final call.
           (piem--reconcile-final-toolcall
            content-index tool-call stream)))))))

(defun piem--message-tool-calls (message)
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

(defun piem--clear-toolcall-preview-body (block)
  "Clear stale streamed body state from tool preview BLOCK."
  (piem--tool-block-set-last-tail block nil)
  (piem--tool-block-replace-body block "" nil nil))

(defun piem--reconcile-toolcall-preview-block
    (content-index tool-call &optional event-type block)
  "Create or update BLOCK for TOOL-CALL at CONTENT-INDEX.
EVENT-TYPE selects streaming or authoritative presentation.  When BLOCK is
nil, reuse a keyed block or create one."
  (let* ((tool-call-id (plist-get tool-call :id))
         (tool-name (plist-get tool-call :name))
         (args (plist-get tool-call :arguments))
         (streaming-p (member event-type '("toolcall_start" "toolcall_delta")))
         (preview-state (and streaming-p 'streaming))
         (existing-block (or block
                             (piem--tool-block-get tool-call-id)))
         (block (or existing-block
                    (piem--display-tool-start
                     tool-name args tool-call-id content-index preview-state
                     'defer))))
    (setq piem--pending-tool-overlay
          (piem--tool-block-overlay block))
    (overlay-put (piem--tool-block-overlay block)
                 'piem-tool-name tool-name)
    (when existing-block
      (piem--display-tool-update-header
       tool-name args block preview-state))
    (let ((content (piem--tool-arg-get args :content)))
      (cond
       ((equal event-type "toolcall_end")
        (if (and (equal tool-name "write") (stringp content))
            (piem--display-tool-streaming-text
             content
             piem-tool-preview-lines
             (piem--path-to-language
              (piem--tool-path-string
               (piem--tool-arg-path args)))
             block)
          (piem--clear-toolcall-preview-body block)))
       ((and (equal tool-name "write")
             (piem--tool-arg-member args :content))
        (if (stringp content)
            (piem--display-tool-streaming-text
             content
             piem-tool-preview-lines
             (piem--path-to-language
              (piem--tool-path-string
               (piem--tool-arg-path args)))
             block)
          (piem--clear-toolcall-preview-body block)))))
    block))

(defun piem--prune-stale-toolcall-previews (tool-call-ids)
  "Drop keyed live preview blocks whose IDs are absent from TOOL-CALL-IDS."
  (dolist (block (piem--live-tool-blocks-in-order))
    (when-let* ((tool-call-id (piem--tool-block-tool-call-id block))
                ((piem--tool-call-id-p tool-call-id)))
      (unless (member tool-call-id tool-call-ids)
        (piem--tool-block-delete block)))))

(defun piem--toolcall-stream-by-id (tool-call-id excluded)
  "Return a stream for TOOL-CALL-ID that is not in EXCLUDED."
  (when (and (piem--tool-call-id-p tool-call-id)
             piem--toolcall-streams)
    (catch 'found
      (maphash
       (lambda (_content-index stream)
         (when (and (not (memq stream excluded))
                    (equal tool-call-id
                           (piem--tool-stream-tool-call-id
                            stream)))
           (throw 'found stream)))
       piem--toolcall-streams)
      nil)))

(defun piem--dedupe-final-toolcall-items (items)
  "In ITEMS, keep the first authoritative ID and delete ambiguous duplicates."
  (let ((seen (make-hash-table :test 'equal))
        unique)
    (dolist (item items)
      (let* ((tool-call (plist-get item :tool-call))
             (tool-call-id (plist-get tool-call :id))
             (first (gethash tool-call-id seen))
             (block (plist-get item :block)))
        (if first
            (unless (eq block (plist-get first :block))
              (when-let* ((stream (plist-get item :stream)))
                (setf (piem--tool-stream-block stream) nil))
              (piem--tool-block-delete block)
              (puthash tool-call-id
                       (plist-get first :block)
                       (piem--ensure-live-tool-blocks)))
          (puthash tool-call-id item seen)
          (push item unique))))
    (nreverse unique)))

(defun piem--reconcile-toolcall-previews (message)
  "Reconcile live preview blocks from authoritative assistant MESSAGE."
  (let ((entries (piem--message-tool-calls message))
        (streams piem--toolcall-streams)
        matched-streams
        matched-blocks
        matched-items
        tool-call-ids)
    ;; Content index owns generation; a stable ID is only the fallback for a
    ;; final message that no longer has a matching stream index.
    (dolist (entry entries)
      (let* ((content-index (plist-get entry :content-index))
             (tool-call (plist-get entry :tool-call))
             (tool-call-id (plist-get tool-call :id))
             (indexed-stream (and streams (gethash content-index streams)))
             (stream (or (and indexed-stream
                              (not (memq indexed-stream matched-streams))
                              indexed-stream)
                         (piem--toolcall-stream-by-id
                          tool-call-id matched-streams)))
             (block (piem--reconcile-final-toolcall
                     content-index tool-call stream)))
        (when stream
          (push stream matched-streams))
        (if (piem--tool-call-id-p tool-call-id)
            (progn
              (when block
                (push block matched-blocks)
                (push (list :content-index content-index
                            :tool-call tool-call
                            :stream stream
                            :block block)
                      matched-items))
              (push tool-call-id tool-call-ids))
          ;; Invalid final IDs cannot correlate execution.  Remove their
          ;; generation-only block now rather than leaving an orphan behind.
          (when block
            (when stream
              (setf (piem--tool-stream-block stream) nil))
            (piem--tool-block-delete block)))))
    (setq matched-items
          (piem--dedupe-final-toolcall-items
           (nreverse matched-items))
          matched-blocks
          (mapcar (lambda (item) (plist-get item :block)) matched-items)
          tool-call-ids (delete-dups tool-call-ids))
    ;; Empty/provisional IDs are owned only by their stream records and cannot
    ;; be pruned through the execution-ID registry.
    (when streams
      (maphash
       (lambda (_content-index stream)
         (when-let* ((block (piem--tool-stream-block stream)))
           (unless (memq block matched-blocks)
             (setf (piem--tool-stream-block stream) nil)
             (piem--tool-block-delete block))))
       streams))
    (piem--prune-stale-toolcall-previews tool-call-ids)))

(defun piem--extract-text-from-content (content-blocks)
  "Extract text from CONTENT-BLOCKS vector efficiently.
Returns the concatenated text from all text blocks.
Optimized for the common case of a single text block."
  (if (and (vectorp content-blocks) (> (length content-blocks) 0))
      (let ((first-block (aref content-blocks 0)))
        (if (and (= (length content-blocks) 1)
                 (equal (plist-get first-block :type) "text"))
            ;; Fast path: single text block (common case)
            (piem--render-safe-string
             (plist-get first-block :text))
          ;; Slow path: multiple blocks, need to filter and concat
          (mapconcat (lambda (c)
                       (if (equal (plist-get c :type) "text")
                           (piem--render-safe-string
                            (plist-get c :text))
                         ""))
                     content-blocks "")))
    ""))

(defun piem--extract-user-message-text (content)
  "Extract text from user message CONTENT.
CONTENT is a vector of content blocks from a user message.
Returns the concatenated text, or nil if empty."
  (let ((text (piem--extract-text-from-content content)))
    (unless (string-empty-p text) text)))

(defun piem--get-tail-lines (content n)
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

(defun piem--tool-block-replace-body
    (block display-content show-hidden-indicator lang)
  "Replace BLOCK body with DISPLAY-CONTENT.
SHOW-HIDDEN-INDICATOR adds the collapsed-output hint line.
LANG is passed to `piem--wrap-in-src-block' for fence construction."
  (when-let* ((block block)
              (header-end (piem--tool-block-header-end block))
              (end-marker (piem--tool-block-end-marker block)))
    (let ((inhibit-read-only t))
      (piem--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position header-end))
          (delete-region (marker-position header-end)
                         (marker-position end-marker))
          (when show-hidden-indicator
            (insert (propertize "... (earlier output)\n"
                                'face
                                'piem-collapsed-indicator)))
          (unless (string-empty-p display-content)
            (insert (piem--wrap-in-src-block
                     display-content lang)
                    "\n"))
          (set-marker end-marker (point))
          (piem--tool-block-refresh-overlay block))))))

(defun piem--display-tool-streaming-text
    (raw-text max-lines &optional lang block source-truncated)
  "Display RAW-TEXT as streaming content in BLOCK.
Shows a rolling tail truncated to MAX-LINES visual lines.
When BLOCK is nil, fall back to the current compatibility tool block.
SOURCE-TRUNCATED means the argument assembler already dropped an older prefix.

When LANG is non-nil, wrap the tail in a markdown fenced code block so
that `md-ts-mode' language injection handles syntax highlighting.
Skips redraw when only the trailing partial line changed (the preview
shows complete lines only)."
  (let ((raw-text (piem--render-safe-string raw-text)))
    (when-let* ((block (or block (piem--current-tool-block))))
      (let* (;; For language-aware streaming, only show complete lines
             ;; (exclude trailing partial line) to keep the preview
             ;; stable across partial-token deltas.
             (complete-text
              (if (and lang (not (string-suffix-p "\n" raw-text)))
                  (let ((last-nl (cl-position ?\n raw-text :from-end t)))
                    (if last-nl (substring raw-text 0 (1+ last-nl)) ""))
                raw-text))
             (tail-result (piem--get-tail-lines complete-text max-lines))
             (tail-content (or (car tail-result) ""))
             (has-hidden (cdr tail-result))
             (truncation (piem--truncate-to-visual-lines
                          tail-content max-lines
                          (piem--chat-display-width)))
             (display-content
              (string-trim-right
               (or (plist-get truncation :content) "")
               "\n+"))
             (show-hidden-indicator
              (or source-truncated
                  has-hidden
                  (> (plist-get truncation :hidden-lines) 0)))
             (cache-key (list lang show-hidden-indicator display-content))
             (last-tail (piem--tool-block-last-tail block)))
        (unless (equal cache-key last-tail)
          (piem--tool-block-replace-body
           block display-content show-hidden-indicator lang)
          (piem--tool-block-set-last-tail block cache-key))))))

;;;; Coalesced tool update rendering

;; Pi emits one `tool_execution_update' per extension onUpdate call, with a
;; cumulative `:partialResult' (replace semantics).  Rendering every event
;; synchronously multiplied an expensive fenced body replacement by
;; machine-gun update bursts (median inter-update gap ~1 ms, peaks above
;; 40 updates/s in real subagent sessions).  Instead, each event only
;; records the latest partial result for its tool call, and one buffer-local
;; one-shot timer renders the pending previews at a humane cadence.  Typing
;; always wins: the flush re-arms instead of rendering while input is pending.

(defconst piem--tool-update-render-interval 0.25
  "Seconds between coalesced renders of streaming tool output previews.
The pattern mirrors pi's TUI, which coalesces repaints latest-wins behind
a minimum render interval; 250 ms is this frontend's humane cadence for
previews that an authoritative tool_execution_end always supersedes.
Internal constant, not a user option.")

(defvar-local piem--pending-tool-updates nil
  "Alist of (TOOL-CALL-ID . PARTIAL-RESULT) awaiting coalesced rendering.
Only the latest partial result per tool call is kept; superseded snapshots
are dropped.  New entries are pushed, so flushing iterates the reversed
alist to render first-queued-first.")

(defvar-local piem--tool-update-flush-timer nil
  "The one pending one-shot flush timer for tool updates, or nil.
At most one timer exists per chat buffer; updates never cancel or rearm
it.  Only the flush itself schedules a further attempt (typing-wins).")

(defun piem--schedule-tool-update-flush ()
  "Arm the one-shot tool-update flush timer for the current buffer."
  (setq piem--tool-update-flush-timer
        (run-at-time piem--tool-update-render-interval nil
                     #'piem--flush-tool-updates
                     (current-buffer))))

(defun piem--queue-tool-update (tool-call-id partial-result)
  "Record PARTIAL-RESULT as the pending preview for TOOL-CALL-ID.
Latest-wins per tool call; the shared flush timer is armed only when none
is pending, and an armed timer keeps its deadline.  No chat text changes."
  (when (and tool-call-id partial-result)
    (if-let* ((entry (assoc tool-call-id piem--pending-tool-updates)))
        (setcdr entry partial-result)
      (push (cons tool-call-id partial-result)
            piem--pending-tool-updates))
    (unless piem--tool-update-flush-timer
      (piem--schedule-tool-update-flush))))

(defun piem--flush-tool-updates (buffer)
  "Render pending tool update previews in BUFFER, then leave clean state.
Timer callback for `piem--tool-update-flush-timer'.  Clears the
timer slot first so updates arriving during the flush can schedule a fresh
pass.  While user input is pending, renders nothing and schedules one
retry; typing wins over preview refresh.  Errors during rendering cannot
wedge the scheduler: the timer slot and pending map are already cleared
before any preview is painted."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq piem--tool-update-flush-timer nil)
      (if (input-pending-p)
          (when piem--pending-tool-updates
            (piem--schedule-tool-update-flush))
        (let ((pending (nreverse piem--pending-tool-updates)))
          (setq piem--pending-tool-updates nil)
          (dolist (entry pending)
            (piem--display-tool-update
             (cdr entry)
             ;; Falls back to the compatibility block when the tool call
             ;; has no keyed live block.
             (piem--tool-block-get (car entry)))))))))

(defun piem--discard-pending-tool-update (tool-call-id)
  "Drop any pending preview for TOOL-CALL-ID.
Called on tool_execution_end before the authoritative final render.  A
still-armed flush timer is left alone: it fires into the remaining (or
empty) pending map, and flushing nothing is a no-op."
  (when (and tool-call-id piem--pending-tool-updates)
    (setq piem--pending-tool-updates
          (assoc-delete-all tool-call-id
                            piem--pending-tool-updates))))

(defun piem--cancel-tool-update-flush ()
  "Cancel any armed tool-update flush timer and discard pending previews.
Idempotent.  Runs on agent_end and wherever live tool state is torn
down -- buffer kill, session reset, history rebuild -- so no stale
timer or preview survives a session transition."
  (when (timerp piem--tool-update-flush-timer)
    (cancel-timer piem--tool-update-flush-timer))
  (setq piem--tool-update-flush-timer nil
        piem--pending-tool-updates nil))

(defun piem--display-tool-update (partial-result &optional block)
  "Display PARTIAL-RESULT as streaming output in BLOCK.
When BLOCK is nil, fall back to the current compatibility tool block.
PARTIAL-RESULT has the same structure as a tool result plist with
`:content'.  Extracts text from content blocks and delegates to
`piem--display-tool-streaming-text'."
  (when partial-result
    (let* ((content-blocks (plist-get partial-result :content))
           (raw-output (piem--extract-text-from-content content-blocks)))
      (piem--display-tool-streaming-text
       raw-output piem-bash-preview-lines nil block))))

(defun piem--markdown-fence-delimiter (content)
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

(defun piem--wrap-in-src-block (content lang)
  "Wrap CONTENT in a markdown fenced code block with LANG.
Returns markdown string for syntax highlighting."
  (let ((fence (piem--markdown-fence-delimiter content)))
    (format "%s%s\n%s\n%s" fence (or lang "") content fence)))

(defun piem--display-tool-end
    (tool-name args content details is-error &optional block)
  "Display result for TOOL-NAME and finalize BLOCK.
ARGS contains tool arguments, CONTENT is a list of content blocks.
DETAILS contains tool-specific data (e.g., a diff for the edit tool);
for generic tools, non-nil DETAILS are rendered below the content.
IS-ERROR indicates failure.
When BLOCK is nil, fall back to the current compatibility tool block and,
if none exists, render the result at point without a live overlay."
  (let* ((block (or block (piem--current-tool-block)))
         (is-error (eq t is-error))
         (content-blocks (piem--content-block-list content))
         (text-blocks (seq-filter (lambda (c) (equal (plist-get c :type) "text"))
                                  content-blocks))
         (raw-output (mapconcat (lambda (c)
                                  (piem--render-safe-string
                                   (plist-get c :text)))
                                text-blocks "\n"))
         (content-image-previews
          (piem--content-image-previews content-blocks))
         (svg-preview
          (and (null content-image-previews)
               (piem--read-svg-preview
                tool-name args raw-output details is-error)))
         (image-previews
          (if svg-preview (list svg-preview) content-image-previews))
         ;; Determine language for syntax highlighting
         (lang (piem--path-to-language
                (piem--tool-path-string
                 (piem--tool-arg-path args))))
         (edit-diff (and (equal tool-name "edit")
                         (piem--tool-arg-get details :diff)))
         ;; For edit tool with a string diff, we'll apply diff overlays after insertion.
         (is-edit-diff (and (not is-error)
                            (stringp edit-diff)))
         (display-content
          (ansi-color-filter-apply
           (piem--render-safe-string
            (pcase tool-name
              ("edit" (or edit-diff raw-output))
              ("write" (or (piem--tool-arg-get args :content)
                           raw-output))
              ((or "bash" "read") raw-output)
              (_ (if-let* ((details-json
                           (piem--pretty-print-json details)))
                     (concat raw-output "\n\n"
                             (piem--propertize-details-region
                              details-json))
                   raw-output))))))
         (preview-limit (pcase tool-name
                          ("bash" piem-bash-preview-lines)
                          (_ piem-tool-preview-lines)))
         ;; Use visual line truncation with byte limit
         (width (piem--chat-display-width))
         (truncation (piem--truncate-to-visual-lines
                      display-content preview-limit width))
         (hidden-count (plist-get truncation :hidden-lines))
         (needs-collapse (> hidden-count 0))
         (inhibit-read-only t))
    (piem--with-scroll-preservation
      (save-excursion
        (if block
            (let* ((header-end (piem--tool-block-header-end block))
                   (end-marker (piem--tool-block-end-marker block)))
              (goto-char (marker-position header-end))
              (delete-region (marker-position header-end)
                             (marker-position end-marker))
              (if needs-collapse
                  ;; Long output: show preview with toggle button.
                  (let ((preview-content (plist-get truncation :content)))
                    (piem--insert-tool-content-with-toggle
                     preview-content display-content lang is-edit-diff hidden-count nil))
                ;; Short output: show all without toggle.
                (piem--insert-rendered-tool-content
                 (string-trim-right display-content "\n+")
                 lang
                 is-edit-diff))
              (piem--insert-image-previews image-previews)
              (piem--tool-block-set-image-previews
               block image-previews)
              (set-marker end-marker (point))
              (piem--tool-block-refresh-overlay block)
              ;; Note: no [error] badge — error content in the block is sufficient,
              ;; and the overlay face already shifts to piem-tool-block-error.
              (when (and (equal tool-name "read")
                         (piem--tool-arg-get args :offset))
                (piem--tool-block-set-offset
                 block (piem--tool-arg-get args :offset)))
              (when-let* ((line-map (plist-get truncation :line-map)))
                (piem--tool-block-set-line-map block line-map))
              (piem--tool-overlay-finalize
               (if is-error 'piem-tool-block-error
                 'piem-tool-block)
               block)
              (when (eobp)
                (insert "\n")))
          (progn
            (goto-char (point-max))
            (if needs-collapse
                (let ((preview-content (plist-get truncation :content)))
                  (piem--insert-tool-content-with-toggle
                   preview-content display-content lang is-edit-diff hidden-count nil))
              (piem--insert-rendered-tool-content
               (string-trim-right display-content "\n+")
               lang
               is-edit-diff))
            (piem--insert-image-previews image-previews)
            (insert "\n")))))))

(defun piem--ranges-excluding-property (start end prop)
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

(defun piem--font-lock-ensure-excluding-property (start end prop)
  "Fontify START..END while skipping regions where PROP is non-nil.
Stops after the first font-lock error to avoid repeated failures."
  (catch 'piem--font-lock-failed
    (dolist (range (piem--ranges-excluding-property start end prop))
      (condition-case err
          (font-lock-ensure (car range) (cdr range))
        (error
         (when debug-on-error
           (message "piem: toggle fontification failed: %S" err))
         (throw 'piem--font-lock-failed nil))))))

(defun piem--toggle-tool-output (button)
  "Toggle between preview and full content for BUTTON.
Preserves window scroll position during the toggle."
  (let* ((inhibit-read-only t)
         (expanded (button-get button 'piem-expanded))
         (full-content (button-get button 'piem-full-content))
         (preview-content (button-get button 'piem-preview-content))
         (lang (button-get button 'piem-lang))
         (is-edit-diff (button-get button 'piem-is-edit-diff))
         (hidden-count (button-get button 'hidden-count))
         (btn-start (button-start button)))
    (save-excursion
      ;; Find the tool overlay
      (goto-char btn-start)
      (when-let* ((bounds (piem--find-tool-block-bounds))
                  (ov (seq-find (lambda (o) (overlay-get o 'piem-tool-block))
                                (overlays-at (point))))
                  (header-end (overlay-get ov 'piem-header-end)))
        ;; Save window positions relative to content-start
        ;; Windows before the tool block: save absolute position
        ;; Windows inside tool block: will use header position after toggle
        ;; Emacs 29's `font-lock-ensure' requires integer bounds below.
        (let* ((content-start (marker-position header-end))
               (block-start (car bounds))
               (record (piem--tool-block-from-overlay ov))
               (image-previews
                (and record
                     (piem--tool-block-image-previews record)))
               (saved-windows
                (mapcar (lambda (w)
                          (let ((ws (window-start w)))
                            (list w ws (window-point w)
                                  ;; Flag: was window-start before content area?
                                  (< ws content-start))))
                        (get-buffer-window-list (current-buffer) nil t))))
          ;; Replace the complete body, including any image previews after the
          ;; button, so each toggle keeps exactly one copy inside the overlay.
          (delete-region content-start (overlay-end ov))
          (goto-char content-start)
          ;; Toggle: if currently expanded, show collapsed (and vice versa)
          (piem--insert-tool-content-with-toggle
           preview-content full-content lang is-edit-diff hidden-count (not expanded))
          (piem--insert-image-previews image-previews)
          ;; Ensure fontification of inserted content (JIT font-lock is lazy)
          ;; while excluding metadata-like details payload.
          (piem--font-lock-ensure-excluding-property
           content-start (point) 'piem-no-fontify)
          ;; Update both typed and overlay bounds after replacing the body.
          (if record
              (progn
                (set-marker (piem--tool-block-end-marker record)
                            (point))
                (piem--tool-block-refresh-overlay record))
            (move-overlay ov block-start (point)))
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

(defun piem--replace-thinking-block-region (start end rendered)
  "Replace completed-thinking text in START..END with RENDERED.
Returns the new bounds as (START . NEW-END)."
  (let ((inhibit-read-only t)
        new-end)
    (save-excursion
      (goto-char start)
      (delete-region start end)
      (insert rendered)
      (setq new-end (point))
      (condition-case-unless-debug nil
          (font-lock-ensure start new-end)
        (error nil)))
    (cons start new-end)))

(defun piem--replace-thinking-block (block rendered)
  "Replace completed thinking BLOCK with RENDERED text.
Returns the new block bounds as (START . END) and preserves useful window
context after the rewrite."
  (let* ((start (plist-get block :start))
         (end (plist-get block :end))
         (buffer (current-buffer))
         (saved-windows (piem--capture-window-rewrite-states))
         (new-bounds (piem--replace-thinking-block-region
                      start end rendered))
         (delta (- (cdr new-bounds) end)))
    (piem--restore-window-rewrite-states
     buffer
     saved-windows
     (let ((replacements (list (list start end delta))))
       (lambda (pos)
         (piem--adjust-pos-after-region-replacements
          pos replacements))))
    new-bounds))

(defun piem--completed-thinking-blocks ()
  "Return completed thinking blocks in the current buffer in source order."
  (let ((pos (point-min))
        blocks)
    (while (< pos (point-max))
      (if (get-text-property pos 'piem-thinking-block)
          (when-let* ((block (piem--thinking-block-metadata-at-pos pos)))
            (push block blocks)
            (setq pos (plist-get block :end)))
        (setq pos (or (next-single-property-change
                       pos 'piem-thinking-block nil (point-max))
                      (point-max)))))
    (nreverse blocks)))

(defun piem--apply-thinking-display-to-completed-blocks (display)
  "Rewrite every completed thinking block in the current buffer for DISPLAY.
DISPLAY is either `visible' or `hidden'.  Returns replacement records when at
least one completed thinking block changed, otherwise nil.  Unrelated buffer
content is left alone.  Each replacement record is (START END DELTA), using
coordinates from before the rewrites."
  (let (replacements)
    (save-excursion
      (dolist (block (nreverse (piem--completed-thinking-blocks)))
        (unless (eq (plist-get block :display) display)
          (when-let* ((rendered
                       (piem--completed-thinking-rendered-from-normalized
                        (plist-get block :normalized)
                        (plist-get block :order)
                        display)))
            (let* ((start (plist-get block :start))
                   (end (plist-get block :end))
                   (new-bounds (piem--replace-thinking-block-region
                                start end rendered)))
              (push (list start end (- (cdr new-bounds) end))
                    replacements))))))
    replacements))

(defun piem--toggle-thinking-block-at-point ()
  "Toggle the completed-thinking block at point.
Returns non-nil when point was inside a completed thinking block and the block
was toggled successfully."
  (when-let* ((block (piem--thinking-block-metadata-at-pos (point)))
              (normalized (plist-get block :normalized))
              (order (plist-get block :order))
              (display (plist-get block :display))
              (rendered (piem--completed-thinking-rendered-from-normalized
                         normalized
                         order
                         (if (eq display 'hidden) 'visible 'hidden))))
    (let* ((original-pos (point))
           (new-bounds (piem--replace-thinking-block block rendered))
           (new-start (car new-bounds))
           (new-end (cdr new-bounds)))
      (goto-char (max new-start
                      (min original-pos (max new-start (1- new-end))))))
    t))

(defun piem--insert-rendered-tool-content (content lang is-edit-diff)
  "Insert CONTENT rendered for LANG with a trailing newline.
When IS-EDIT-DIFF is non-nil, apply diff overlays to the inserted block."
  (let ((content-start (point)))
    (insert (piem--wrap-in-src-block content lang) "\n")
    (when is-edit-diff
      (piem--apply-diff-overlays content-start (point)))))

(defun piem--tool-hidden-line-label (hidden-count)
  "Return the plain display label for HIDDEN-COUNT hidden lines."
  (format "... (%d more lines)" hidden-count))

(defun piem--insert-tool-content-with-toggle
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
                         (piem--tool-hidden-line-label hidden-count))))
    (piem--insert-rendered-tool-content
     display-content
     lang
     is-edit-diff)
    (insert-text-button
     (propertize button-label 'face 'piem-collapsed-indicator)
     'action #'piem--toggle-tool-output
     'follow-link t
     'piem-tool-toggle t
     'piem-full-content full-content
     'piem-preview-content preview-content
     'piem-lang lang
     'piem-is-edit-diff is-edit-diff
     'piem-expanded expanded
     'hidden-count hidden-count)
    (insert "\n")))

(defun piem--find-tool-block-bounds ()
  "Find the bounds of the tool block at point.
Returns (START . END) if inside a tool block, nil otherwise."
  (let ((overlays (overlays-at (point))))
    (when-let* ((ov (seq-find (lambda (o) (overlay-get o 'piem-tool-block)) overlays)))
      (cons (overlay-start ov) (overlay-end ov)))))

(defun piem--find-toggle-button-in-region (start end)
  "Find a toggle button between START and END."
  (save-excursion
    (goto-char start)
    (let ((found nil))
      (while (and (not found) (< (point) end))
        (let ((btn (button-at (point))))
          (if (and btn (button-get btn 'piem-full-content))
              (setq found btn)
            (forward-char 1))))
      found)))

(defun piem-toggle-tool-section ()
  "Toggle the section at point.
Completed thinking blocks toggle first, then tool output blocks, then the
command falls back to `outline-cycle' for turn folding."
  (interactive)
  (unless (piem--toggle-thinking-block-at-point)
    (let ((original-pos (point)))
      (if-let* ((bounds (piem--find-tool-block-bounds)))
          (if-let* ((btn (piem--find-toggle-button-in-region
                          (car bounds) (cdr bounds))))
              (progn
                (piem--toggle-tool-output btn)
                ;; Try to restore position, clamped to new block bounds.
                ;; Use (1- end) because overlays-at uses half-open [start, end),
                ;; so clamping to exactly end would place cursor outside the
                ;; overlay, breaking the next toggle.
                (when-let* ((new-bounds (piem--find-tool-block-bounds)))
                  (goto-char (min original-pos (1- (cdr new-bounds))))))
            ;; No button found - short output, use outline-cycle
            (outline-cycle))
        ;; Not in a tool block
        (outline-cycle)))))

;;;; Tool Block Cooling
;;
;; Completed tool blocks outside the hot tail (older than the most
;; recent `piem-hot-tail-turn-count' headed turns) are
;; cooled into plain text.  Live agent-end handling queues one block per
;; timer turn; history replay keeps synchronous cooling after invalidating
;; stale queued work.  The cold form keeps the header, visible preview, and
;; lightweight authoritative target metadata, but drops overlays, buttons,
;; full-content payloads, and syntax-tagged rendering.

(defconst piem--tool-cooling-delay 0.05
  "Seconds between deferred rewrites of completed tool blocks.
A small nonzero delay gives input and redisplay a chance to run between
expensive Markdown-changing rewrites.  Internal constant, not a user option.")

(defvar-local piem--tool-cooling-queue nil
  "Completed tool overlays awaiting deferred cold-history rewrites.
Candidates are ordered from the end of the buffer backward and are
revalidated immediately before each rewrite.")

(defvar-local piem--tool-cooling-timer nil
  "The one owned one-shot timer for deferred tool cooling, or nil.")

(defvar-local piem--tool-cooling-generation 0
  "Generation owning the current deferred tool-cooling queue.
Cancellation increments this value so already-dispatched callbacks cannot
act on rebuilt history or a newer queue.")

(defun piem--ensure-cold-tool-property-nonsticky ()
  "Keep cold tool authority from spreading across insertion boundaries."
  (unless (eq t (alist-get 'piem-cold-tool-block
                           text-property-default-nonsticky))
    (setq-local text-property-default-nonsticky
                (cons '(piem-cold-tool-block . t)
                      (assq-delete-all
                       'piem-cold-tool-block
                       (copy-sequence text-property-default-nonsticky))))))

(defun piem--tool-overlay-live-p (overlay)
  "Return non-nil when OVERLAY's tool block is still in the live registry."
  (when-let* ((rec (overlay-get overlay 'piem-tool-block-record))
              (id (piem--tool-block-tool-call-id rec)))
    (piem--tool-block-get id)))

(defun piem--completed-tool-overlay-p (overlay)
  "Return non-nil when OVERLAY is a completed (finalized) tool block.
Live tool blocks that are still executing are excluded."
  (and (overlayp overlay)
       (overlay-buffer overlay)
       (overlay-get overlay 'piem-tool-block)
       (not (eq overlay piem--pending-tool-overlay))
       (not (piem--tool-overlay-live-p overlay))
       (overlay-get overlay 'piem-header-end)))

(defun piem--completed-tool-overlay-before-p (overlay boundary)
  "Return non-nil when tool OVERLAY is completed and before BOUNDARY."
  (and (piem--completed-tool-overlay-p overlay)
       (< (overlay-start overlay) boundary)))

(defun piem--tool-overlays-in-reverse-order (overlays)
  "Return a copy of tool OVERLAYS ordered from buffer end backward."
  (sort (copy-sequence overlays)
        (lambda (a b)
          (> (overlay-start a) (overlay-start b)))))

(defun piem--tool-cooling-boundary ()
  "Return the current usable hot-tail boundary, or nil.
A boundary at `point-min' leaves the whole buffer hot."
  (when (and (markerp piem--hot-tail-start)
             (eq (marker-buffer piem--hot-tail-start)
                 (current-buffer))
             (> (marker-position piem--hot-tail-start) (point-min)))
    (marker-position piem--hot-tail-start)))

(defun piem--completed-tool-overlays-outside-hot-tail ()
  "Return completed tool overlays before the current hot-tail boundary.
The result is ordered from the end of the buffer backward."
  (when-let* ((boundary (piem--tool-cooling-boundary)))
    (piem--tool-overlays-in-reverse-order
     (seq-filter
      (lambda (overlay)
        (piem--completed-tool-overlay-before-p overlay boundary))
      (overlays-in (point-min) boundary)))))

(defun piem--tool-overlay-visible-body (overlay)
  "Return the currently visible body text for completed tool OVERLAY.
Extracts the text between the outer fence lines, removing only the
wrapper newline inserted before the closing fence.  Collapsed blocks
therefore cool into their visible preview only."
  (when-let* ((header-end-marker (overlay-get overlay 'piem-header-end))
              (header-end (and (markerp header-end-marker)
                               (marker-position header-end-marker)))
              (overlay-end (overlay-end overlay)))
    (save-excursion
      (goto-char header-end)
      (when-let* ((opening-fence (piem--fence-line-info-at-point)))
        (forward-line 1)
        (let ((content-start (point))
              (closing-start nil))
          (while (and (not closing-start) (< (point) overlay-end))
            (let ((line-info (piem--fence-line-info-at-point)))
              (when (piem--fence-closing-line-p opening-fence line-info)
                (setq closing-start (line-beginning-position))))
            (unless closing-start
              (forward-line 1)))
          (when closing-start
            (let ((wrapped-body (buffer-substring-no-properties
                                 content-start closing-start)))
              (if (string-suffix-p "\n" wrapped-body)
                  (substring wrapped-body 0 -1)
                wrapped-body))))))))

(defun piem--cold-tool-target-metadata
    (overlay header-end collapsed)
  "Return lightweight target metadata for cold tool OVERLAY.
HEADER-END is the current absolute end of its header.  COLLAPSED is non-nil
when the visible body is a mapped preview.  The result deliberately excludes
buttons, full content, markers, and absolute buffer positions."
  (let ((record (piem--tool-block-from-overlay overlay)))
    (list :order (and record (piem--tool-block-order record))
          :tool-name (overlay-get overlay 'piem-tool-name)
          :path (overlay-get overlay 'piem-tool-path)
          :raw-path (overlay-get overlay 'piem-tool-raw-path)
          :path-error (overlay-get overlay 'piem-tool-path-error)
          :offset (overlay-get overlay 'piem-tool-offset)
          ;; Only collapsed previews need the small visible-line map.
          :line-map (and collapsed
                         (overlay-get overlay 'piem-line-map))
          :header-length (- header-end (overlay-start overlay)))))

(defun piem--tool-overlay-cold-metadata (overlay)
  "Return cold-history metadata for completed tool OVERLAY.
The result carries its visible body, lightweight target metadata and, for a
currently collapsed block, its hidden count.  Expanded blocks return no hidden
count because cold history must stay preview-only."
  (when-let* ((visible-body (piem--tool-overlay-visible-body overlay))
              (header-end (marker-position
                           (overlay-get overlay 'piem-header-end))))
    (let* ((record (piem--tool-block-from-overlay overlay))
           (image-previews
            (and record
                 (piem--tool-block-image-previews record)))
           (button (piem--find-toggle-button-in-region
                    header-end (overlay-end overlay)))
           (collapsed (and button
                           (not (button-get button
                                            'piem-expanded))))
           (hidden-count (and collapsed
                              (button-get button 'hidden-count))))
      (list :visible-body visible-body
            :image-previews image-previews
            :target-metadata
            (piem--cold-tool-target-metadata
             overlay header-end collapsed)
            :hidden-count (and (integerp hidden-count)
                               (> hidden-count 0)
                               hidden-count)))))

(defun piem--cool-tool-overlay (overlay)
  "Rewrite completed tool OVERLAY into its cold plain-history form.
Preserves the header and visible preview, drops overlays, buttons, and
diff annotations."
  (when (piem--completed-tool-overlay-p overlay)
    (when-let* ((metadata (piem--tool-overlay-cold-metadata overlay))
                (visible-body (plist-get metadata :visible-body))
                (header-end (marker-position
                             (overlay-get overlay 'piem-header-end))))
      (let* ((inhibit-read-only t)
             (hidden-count (plist-get metadata :hidden-count))
             (image-previews (plist-get metadata :image-previews))
             (target-metadata (plist-get metadata :target-metadata))
             (cold-body (concat
                         (piem--wrap-in-src-block visible-body nil)
                         "\n"
                         (piem--image-previews-text image-previews)
                         (when hidden-count
                           (concat (piem--tool-hidden-line-label hidden-count)
                                   "\n"))))
             (ov-start (overlay-start overlay))
             (ov-end (overlay-end overlay)))
        (remove-overlays ov-start ov-end 'piem-diff-overlay t)
        (remove-text-properties
         ov-start ov-end '(piem-cold-tool-block nil))
        (delete-overlay overlay)
        (save-excursion
          (goto-char header-end)
          (delete-region header-end ov-end)
          (insert cold-body)
          (piem--ensure-cold-tool-property-nonsticky)
          (add-text-properties
           ov-start (point)
           `(piem-cold-tool-block ,target-metadata)))
        t))))

(defun piem--cool-completed-tool-blocks (overlays)
  "Cool the given completed tool OVERLAYS.
Blocks are processed from the end of the buffer backward so region
rewrites do not disturb remaining candidates."
  (let ((sorted
         (piem--tool-overlays-in-reverse-order
          (seq-filter #'piem--completed-tool-overlay-p overlays))))
    (piem--with-scroll-preservation
      (save-excursion
        (dolist (overlay sorted)
          (piem--cool-tool-overlay overlay))))))

(defun piem--cool-completed-tool-blocks-outside-hot-tail ()
  "Synchronously cool completed tool blocks before the hot-tail boundary.
Uses the same `piem--hot-tail-start' marker that tables use for
resize scope.  History replay uses this synchronous path after cancelling
stale deferred work; live agent_end handling uses the deferred queue."
  (when-let* ((cold-overlays
              (piem--completed-tool-overlays-outside-hot-tail)))
    (piem--cool-completed-tool-blocks cold-overlays)))

(defun piem--cancel-tool-cooling ()
  "Cancel and invalidate deferred completed-tool cooling.
The operation is idempotent with respect to owned work.  Its generation
always advances so a callback already dispatched by Emacs becomes stale."
  (when (timerp piem--tool-cooling-timer)
    (cancel-timer piem--tool-cooling-timer))
  (setq piem--tool-cooling-timer nil
        piem--tool-cooling-queue nil
        piem--tool-cooling-generation
        (1+ piem--tool-cooling-generation)))

(defun piem--fail-tool-cooling (error-data)
  "Fail closed after deferred cooling ERROR-DATA.
Owned work is invalidated before reporting the failure, preventing an error
from leaving a retry loop or wedged timer slot."
  (piem--cancel-tool-cooling)
  (display-warning
   'piem
   (format "Deferred tool block cooling failed: %s"
           (error-message-string error-data))
   :error))

(defun piem--schedule-tool-cooling ()
  "Arm one deferred tool-cooling slice when work lacks an owner."
  (when (and piem--tool-cooling-queue
             (not piem--tool-cooling-timer))
    (setq piem--tool-cooling-timer
          (run-at-time piem--tool-cooling-delay nil
                       #'piem--run-tool-cooling-slice
                       (current-buffer)
                       piem--tool-cooling-generation))))

(defun piem--queue-tool-cooling-outside-hot-tail ()
  "Merge the current outside-hot-tail cohort into deferred cooling.
Existing candidates are deduplicated and retain one buffer-local timer owner.
Newly eligible overlays take their normal reverse-buffer order; stale queued
entries remain at the end for execution-time revalidation."
  (let ((candidates
         (piem--completed-tool-overlays-outside-hot-tail))
        merged)
    (dolist (overlay (append candidates piem--tool-cooling-queue))
      (unless (memq overlay merged)
        (push overlay merged)))
    (setq piem--tool-cooling-queue (nreverse merged))
    (piem--schedule-tool-cooling)))

(defun piem--map-tool-cooling-position
    (position old-start old-end new-end)
  "Map POSITION through one tool-body replacement.
OLD-START and OLD-END delimit the old half-open body range; NEW-END is the
new body end and the body start is unchanged.  Positions before the body stay
fixed.  Positions in the old body retain their relative offset, clamped to the
new body end.  Positions at or after OLD-END shift by the actual length delta."
  (cond
   ((< position old-start)
    position)
   ((< position old-end)
    (+ old-start
       (min (- position old-start)
            (- new-end old-start))))
   (t
    (+ position (- new-end old-end)))))

(defun piem--capture-tool-cooling-view ()
  "Capture buffer point and visible-window positions before tool cooling.
Unlike `piem--with-scroll-preservation', which restores unmapped
`window-point' and is valid for append-only inserts, cooling rewrites
mid-buffer ranges, so positions and window starts must be mapped through
the replacement."
  (list
   :point (point)
   :windows
   (mapcar
    (lambda (window)
      (list :window window
            :following (piem--window-following-p window)
            :start (window-start window)
            :point (window-point window)))
    (get-buffer-window-list (current-buffer) nil t))))

(defun piem--restore-tool-cooling-view
    (view old-start old-end new-end)
  "Restore cooling VIEW through the body replacement bounds.
OLD-START and OLD-END are the old body bounds and NEW-END is its new end.
Every live window still showing the current buffer gets mapped start and point;
a window that was following the tail instead gets point at the new buffer end.
The buffer's own point is mapped too."
  (let ((map-position
         (lambda (position)
           (piem--map-tool-cooling-position
            position old-start old-end new-end))))
    ;; Restore buffer point first.  A selected chat window restored below then
    ;; establishes the same mapped point, or point-max when it was following.
    (goto-char (funcall map-position (plist-get view :point)))
    (dolist (window-state (plist-get view :windows))
      (let ((window (plist-get window-state :window)))
        (when (and (window-live-p window)
                   (eq (window-buffer window) (current-buffer)))
          (set-window-start
           window
           (funcall map-position (plist-get window-state :start))
           t)
          (set-window-point
           window
           (if (plist-get window-state :following)
               (point-max)
             (funcall map-position
                      (plist-get window-state :point)))))))))

(defun piem--cool-tool-overlay-preserving-view (overlay)
  "Cool completed tool OVERLAY while mapping visible view positions.
The header stays in place, so only the replaced body bounds participate in the
mapping.  Return the result of `piem--cool-tool-overlay'."
  (let* ((header-end-marker
          (overlay-get overlay 'piem-header-end))
         (old-start (and (markerp header-end-marker)
                         (marker-position header-end-marker)))
         (old-end (overlay-end overlay)))
    (if (not (and old-start old-end (<= old-start old-end)))
        (piem--cool-tool-overlay overlay)
      (let ((view (piem--capture-tool-cooling-view))
            ;; The primitive deletes at OLD-START, then inserts there.  This
            ;; rear-advancing marker stays at the deletion boundary and moves
            ;; across exactly the newly inserted body.
            (new-end-marker (copy-marker old-start t)))
        (unwind-protect
            (when (piem--cool-tool-overlay overlay)
              (piem--restore-tool-cooling-view
               view old-start old-end (marker-position new-end-marker))
              t)
          (set-marker new-end-marker nil))))))

(defun piem--run-tool-cooling-slice (buffer generation)
  "Cool at most one queued tool overlay in BUFFER for GENERATION.
This is the ordinary one-shot timer callback.  Current input wins without
queue progress.  Otherwise one candidate is removed, revalidated as completed,
current-buffer-owned, and still outside the live hot-tail boundary, then passed
to the unchanged cold rewrite primitive with cooling-specific view mapping.
Timer ownership is cleared before input checks, mutation, or rearming."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (= generation piem--tool-cooling-generation)
        (setq piem--tool-cooling-timer nil)
        (condition-case error-data
            (if (input-pending-p)
                (piem--schedule-tool-cooling)
              (when-let* ((overlay (pop piem--tool-cooling-queue)))
                (when-let* ((boundary (piem--tool-cooling-boundary)))
                  (when (piem--completed-tool-overlay-before-p
                         overlay boundary)
                    (piem--cool-tool-overlay-preserving-view
                     overlay)))
                (piem--schedule-tool-cooling)))
          (quit
           ;; C-g is user intent: pending cooling is discarded silently and
           ;; the quit resignals; errors, by contrast, fail loudly below.
           (piem--cancel-tool-cooling)
           (signal (car error-data) (cdr error-data)))
          (error
           (piem--fail-tool-cooling error-data)))))))

;;;; File Navigation

(defun piem--positive-location-p (value)
  "Return non-nil when VALUE is a positive one-based integer."
  (and (integerp value) (> value 0)))

(defun piem--diff-line-at-point ()
  "Extract line number from diff line at point.
Returns the line number if point is on an added, removed, or context
line, nil otherwise.
Diff format: [+- ] LINENUM content.
For example: '+ 7     code', '-12     code', or '  9     context'."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[ +-] *\\([1-9][0-9]*\\)\\(?:[ \t]+\\|$\\)")
      (string-to-number (match-string 1)))))

(defun piem--fence-line-info-at-point ()
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

(defun piem--fence-closing-line-p (fence line-info)
  "Return non-nil when LINE-INFO closes FENCE.
FENCE and LINE-INFO are plists from
`piem--fence-line-info-at-point'."
  (and line-info
       (= (plist-get line-info :char) (plist-get fence :char))
       (>= (plist-get line-info :len) (plist-get fence :len))
       (string-match-p "^[ \t]*$" (plist-get line-info :trailing))))

(defun piem--code-block-line-at-point (&optional start-pos)
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
        (let ((line-info (piem--fence-line-info-at-point)))
          (cond
           (open-fence
            (cond
             ((piem--fence-closing-line-p open-fence line-info)
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

(defun piem--tool-overlay-collapsed-p (overlay)
  "Return non-nil when OVERLAY is currently collapsed.
Collapsed tool blocks contain a toggle button whose
`piem-expanded' property is nil.  Expanded blocks and
non-collapsible blocks return nil."
  (when-let* ((btn (piem--find-toggle-button-in-region
                   (overlay-start overlay)
                   (overlay-end overlay))))
    (not (button-get btn 'piem-expanded))))

(defun piem--tool-line-from-metadata
    (tool-name stored-offset line-map header-end use-line-map)
  "Calculate the file line at point from tool rendering metadata.
TOOL-NAME and STORED-OFFSET identify tool semantics.  LINE-MAP maps visible
preview rows when USE-LINE-MAP is non-nil.  HEADER-END bounds fence parsing.
Invalid read offsets and positions without a meaningful file row return nil."
  (let ((offset (cond
                 ((not (equal tool-name "read")) 1)
                 ((null stored-offset) 1)
                 ((and (integerp stored-offset) (> stored-offset 0))
                  stored-offset))))
    (if (equal tool-name "edit")
        ;; Edit navigation uses the explicit line number in diff rows.
        (piem--diff-line-at-point)
      (when offset
        (if (and use-line-map line-map header-end)
            ;; Collapsed preview strips blank lines, so its authoritative map
            ;; must not fall through to expanded code-block coordinates.
            (save-excursion
              (let* ((current-line (line-number-at-pos))
                     (header-line (line-number-at-pos header-end))
                     (lines-from-header (- current-line header-line))
                     (map-index (1- lines-from-header)))
                (when (and (>= map-index 0) (< map-index (length line-map)))
                  (let ((mapped-line (aref line-map map-index)))
                    (when (piem--positive-location-p mapped-line)
                      (+ mapped-line (1- offset)))))))
          ;; Expanded/full output preserves blank lines: derive from code block.
          (when-let* ((header-end header-end)
                      (block-line
                       (piem--code-block-line-at-point header-end)))
            (+ block-line (1- offset))))))))

(defun piem--tool-line-at-point (overlay)
  "Calculate the physical file line at point for hot tool OVERLAY.
Any chat restriction is restored exactly after the authoritative lookup."
  (save-restriction
    (widen)
    (let ((line-map (overlay-get overlay 'piem-line-map))
          (header-end (overlay-get overlay 'piem-header-end)))
      (piem--tool-line-from-metadata
       (overlay-get overlay 'piem-tool-name)
       (overlay-get overlay 'piem-tool-offset)
       line-map
       header-end
       (and line-map header-end
            (piem--tool-overlay-collapsed-p overlay))))))

(cl-defun piem--make-file-target
    (source raw emacs-path &key line column range bounds fragment label)
  "Return a resolved file target for SOURCE and RAW candidate.
EMACS-PATH is the normalized local or TRAMP path for Emacs operations.
The returned plist has this contract:

  `:source'     is `:tool', `:link', or `:text';
  `:raw'        is the original metadata, link destination, or text candidate;
  `:display'    is that candidate escaped for prompts and messages;
  `:emacs-path' is the normalized path for Emacs file operations;
  `:shell-path' is the unquoted path in the local or TRAMP shell namespace;
  `:shell-path-error' explains why no safe shell path can be represented;
  `:shell-directory' is the canonical directory selecting that shell host;
  `:line', `:column', and `:range' are optional source locations;
  `:bounds'     is an optional cons of buffer positions;
  `:fragment'   is an optional local-link fragment, outside filesystem paths;
  `:label'      is an optional control-safe Markdown link label.

Keyword arguments LINE, COLUMN, RANGE, BOUNDS, FRAGMENT, and LABEL supply those
fields.  Representable shell paths are absolute or remote-home-rooted, so
leading-dash relative names cannot become options.  A shell-only conversion
error is stored
rather than signaled, leaving `:emacs-path' usable by Emacs-only consumers.
Shell consumers must use `piem--file-target-shell-path' or the
exactly-once quoted `piem--file-target-shell-argument'; `:shell-path'
is not command text.  Constructing a target does not check file existence."
  (let ((anchor (piem--chat-session-directory))
        shell-path shell-path-error)
    (condition-case err
        (setq shell-path
              (piem--shell-command-path emacs-path anchor))
      (user-error
       (setq shell-path-error (error-message-string err))))
    (list :source source
          :raw raw
          :display (piem--escape-control-chars-for-display raw)
          :emacs-path emacs-path
          :shell-path shell-path
          :shell-path-error shell-path-error
          :shell-directory anchor
          :line line
          :column column
          :range range
          :bounds bounds
          :fragment fragment
          :label label)))

(defun piem--file-target-shell-path (target)
  "Return TARGET's unquoted shell-local path, or signal `user-error'.
Shell conversion failures are delayed until this shell-action boundary so
Emacs-only target consumers can always use a valid `:emacs-path'."
  (or (plist-get target :shell-path)
      (user-error "%s" (or (plist-get target :shell-path-error)
                            "File target has no shell path"))))

(defun piem--file-target-shell-argument (target)
  "Return TARGET safely quoted exactly once as one shell operand.
The result is suitable for a `shell-command' run with TARGET's
`:shell-directory' as `default-directory'."
  (piem--shell-quote-path
   (piem--file-target-shell-path target)
   (plist-get target :shell-directory)))

(defun piem--isolated-shell-star-p (command index)
  "Return non-nil when COMMAND has an isolated `*' at INDEX.
This is Dired's shell-agnostic textual rule: both neighbors must independently
be a string edge, an ASCII space, or a tab.  Quotes and backslashes have no
special meaning; explicit marker placement is the user's responsibility."
  (and (eq (aref command index) ?*)
       (or (zerop index)
           (memq (aref command (1- index)) '(?\s ?\t)))
       (or (= (1+ index) (length command))
           (memq (aref command (1+ index)) '(?\s ?\t)))))

(defun piem--simple-shell-command-p (body)
  "Return non-nil when BODY is safe for automatic file-argument appending.
This is an explicit fail-closed ASCII whitelist, not a shell parser:

  BODY := H* COMMAND (H+ OPTION)* H*
  H := ASCII space or tab
  COMMAND := one or more of A-Z a-z 0-9 _ + . / -,
             containing at least one of A-Z a-z 0-9 _ and not being a
             path metatoken such as `.`, `..`, or `/`
  OPTION := `-` followed by one or more of A-Z a-z 0-9 _ + . / : = , -

Thus ordinary commands such as `file`, `cat`, `wc -l`, and path-like command
words are accepted.  Arguments other than options, controls, non-ASCII space,
quotes, escapes, comments, redirections, substitutions, and globs are rejected."
  (when (and
         (stringp body)
         (string-match
          (concat "\\`[ \t]*\\([A-Za-z0-9_+./-]+\\)"
                  "\\(?:[ \t]+-[A-Za-z0-9_+./:=,-]+\\)*"
                  "[ \t]*\\'")
          body))
    (let ((command (match-string 1 body)))
      (and (string-match-p "[A-Za-z0-9_]" command)
           (not (member command '("." ".." "/" "./" "../" "-" "--")))))))

(defun piem--terminal-shell-async-start (command)
  "Return start of COMMAND's narrow native async suffix, or nil.
The suffix is one ampersand preceded by one or more ASCII spaces or tabs and
followed only by ASCII spaces or tabs.  Scan backward once so malformed long
near-suffixes cannot cause regular-expression backtracking."
  (let ((index (length command)))
    (while (and (> index 0)
                (memq (aref command (1- index)) '(?\s ?\t)))
      (setq index (1- index)))
    (when (and (> index 0)
               (eq (aref command (1- index)) ?&))
      (let ((ampersand (1- index)))
        (setq index ampersand)
        (while (and (> index 0)
                    (memq (aref command (1- index)) '(?\s ?\t)))
          (setq index (1- index)))
        (and (< index ampersand) index)))))

(defun piem--shell-command-with-file (command argument)
  "Return validated COMMAND with already-quoted file ARGUMENT supplied.
ARGUMENT is safe shell text and is never quoted again.  Every `*' with
string-edge, ASCII-space, or tab boundaries on both sides is replaced linearly;
quotes and escapes do not alter this Dired-style textual marker grammar.  A
marker permits explicit compound, control, and multiline shell syntax.

Without a marker, append ARGUMENT only when the body satisfies
`piem--simple-shell-command-p'.  Recognize asynchronous execution
only as one terminal ` &' suffix whose ampersand is preceded by ASCII space or
tab; strip that suffix before substitution and validation, then reattach it.
Reject blank bodies and any other raw terminal ampersand before `shell-command'
can classify it asynchronously."
  (let ((async-start (piem--terminal-shell-async-start command))
        async-suffix)
    (when async-start
      (setq async-suffix (substring command async-start)
            command (substring command 0 async-start)))
    (when (or (string-match-p "\\`[ \t]*\\'" command)
              (and (not async-suffix)
                   (string-match-p "\\`[ \t]*&[ \t]*\\'" command)))
      (user-error "Shell command cannot be empty"))
    (let ((index 0)
          (last 0)
          (marker-p nil)
          parts)
      (while (< index (length command))
        (when (and (eq (aref command index) ?*)
                   (piem--isolated-shell-star-p command index))
          (setq marker-p t)
          (push (substring command last index) parts)
          (push argument parts)
          (setq last (1+ index)))
        (setq index (1+ index)))
      (let ((body
             (if marker-p
                 (progn
                   (push (substring command last) parts)
                   (apply #'concat (nreverse parts)))
               (unless (piem--simple-shell-command-p command)
                 (user-error
                  (concat "Compound shell commands require an isolated * "
                          "file placeholder")))
               (concat command " " argument))))
        (when (string-match-p "&[ \t]*\\'" body)
          (user-error
           "Ambiguous terminal ampersand; use a terminal ` &' suffix"))
        (concat body async-suffix)))))

(defun piem--strict-text-file-path-p (path quoted)
  "Return non-nil when PATH has the strict plain-text file grammar.
PATH must be absolute, home-relative, explicitly relative with `./', or have
at least two slash-separated relative components.  When QUOTED is non-nil,
ASCII spaces are also allowed in a final component with a conventional file
extension.  This keeps the accepted quoted form narrow enough to reject common
prose and command tails.  Empty, dot, and dot-dot components are rejected."
  (when (and (stringp path) (not (string-empty-p path)))
    (let ((body (cond
                 ((string-prefix-p "./" path) (substring path 2))
                 ((string-prefix-p "~/" path) (substring path 2))
                 ((string-prefix-p "/" path) (substring path 1))
                 ((string-match-p "/" path) path))))
      (let ((components (and body (split-string body "/" nil))))
        (and body
             (not (string-empty-p body))
             (not (string-suffix-p "/" body))
             (or (not quoted)
                 (cl-every (lambda (component)
                             (not (string-match-p " " component)))
                           (butlast components)))
             (or (not quoted)
                 (not (string-match-p " " path))
                 (string-match-p
                  "\\.[[:alnum:]][-[:alnum:]_.+]*\\'"
                  (car (last components))))
             (cl-every
              (lambda (component)
                (and (not (member component '("" "." "..")))
                     (or (not quoted)
                         (equal component (string-trim component)))
                     (string-match-p
                      (if quoted
                          "\\`[-[:alnum:]_.+ ]+\\'"
                        "\\`[-[:alnum:]_.+]+\\'")
                      component)))
              components))))))

(defconst piem--max-text-file-candidate-length 4096
  "Maximum number of characters in a plain-text file candidate.
This matches the common conservative PATH_MAX scale while bounding regexp
input, allocation, and at-point scans even for generated chat lines.  It is a
character bound because Emacs buffer and string positions count characters.")

(defconst piem--text-file-input-radius
  (+ 2 (* 2 piem--max-text-file-candidate-length))
  "Maximum source-text radius inspected around a file-target point.
One candidate length covers candidate text; the second retains bounded wrapper,
escape-parity, hidden-markup, and quote-authority context.")

(defun piem--parse-text-file-candidate (raw quoted start end)
  "Parse strict plain-text file candidate RAW between START and END.
QUOTED permits spaces in the path.  START and END are caller-owned offsets
returned unchanged as `:bounds'.  Return nil unless all of RAW matches the
path grammar, optionally followed by `:LINE', `:LINE:COLUMN', or
`#LSTART-LEND'.  A `:LINE' or `:LINE:COLUMN' may itself be followed by one
terminal diagnostic-separator colon.  That separator is excluded from `:raw'
and `:bounds'; arbitrary path colons and no-space diagnostic prose remain
invalid.  Candidates longer than
`piem--max-text-file-candidate-length' are rejected before any
regexp runs.  This pure helper performs no buffer access or file I/O."
  (when (and (stringp raw)
             (<= (length raw)
                 piem--max-text-file-candidate-length))
    (let ((case-fold-search nil)
          (path raw)
          (candidate-raw raw)
          (candidate-end end)
          line column range diagnostic-separator)
      (cond
       ((string-match
         "\\`\\(.*\\):\\([1-9][0-9]*\\):\\([1-9][0-9]*\\)\\(:\\)?\\'"
         raw)
        (setq path (match-string 1 raw)
              line (string-to-number (match-string 2 raw))
              column (string-to-number (match-string 3 raw))
              diagnostic-separator (match-beginning 4)))
       ((string-match
         "\\`\\(.*\\):\\([1-9][0-9]*\\)\\(:\\)?\\'" raw)
        (setq path (match-string 1 raw)
              line (string-to-number (match-string 2 raw))
              diagnostic-separator (match-beginning 3)))
       ((string-match
         "\\`\\(.*\\)#L\\([1-9][0-9]*\\)-L\\([1-9][0-9]*\\)\\'" raw)
        (let ((first (string-to-number (match-string 2 raw)))
              (last (string-to-number (match-string 3 raw))))
          (when (<= first last)
            (setq path (match-string 1 raw)
                  line first
                  range (cons first last))))))
      (when diagnostic-separator
        (setq candidate-raw (substring raw 0 -1)
              candidate-end (1- end)))
      (when (and (or (not (string-match-p "#L" raw)) range)
                 (piem--strict-text-file-path-p path quoted))
        (list :raw candidate-raw
              :path path
              :line line
              :column column
              :range range
              :diagnostic-separator (and diagnostic-separator t)
              :bounds (cons start candidate-end))))))

(defun piem--text-file-diagnostic-context-p
    (candidate text lexical-end following-index)
  "Return non-nil when CANDIDATE has valid diagnostic context in TEXT.
Ordinary candidates always pass.  A diagnostic separator must be the actual
last character of the lexical candidate ending at LEXICAL-END, followed at
FOLLOWING-INDEX by exactly one ASCII space and non-whitespace diagnostic text.
Callers place FOLLOWING-INDEX after any authoritative quote wrapper."
  (or (not (plist-get candidate :diagnostic-separator))
      (let ((separator (cdr (plist-get candidate :bounds))))
        (and (= (1+ separator) lexical-end)
             (< (1+ following-index) (length text))
             (eq (aref text separator) ?:)
             (eq (aref text following-index) ?\s)
             (not (piem--text-file-whitespace-p
                   (aref text (1+ following-index))))))))

(defun piem--single-quote-boundary-p (text index opening)
  "Return non-nil when the quote at INDEX in TEXT is a wrapper boundary.
When OPENING is non-nil, accept line start, whitespace, or an opening Markdown
wrapper before the quote.  Otherwise accept line end, whitespace, trailing
punctuation, or a closing Markdown wrapper after it."
  (let ((neighbor-index (if opening (1- index) (1+ index))))
    (or (< neighbor-index 0)
        (>= neighbor-index (length text))
        (memq (aref text neighbor-index)
              (string-to-list
               (if opening " \t([{<" " \t.,;!?)]}>"))))))

(defun piem--escaped-text-quote-p (text index)
  "Return non-nil when the quote at INDEX in TEXT is backslash-escaped.
An overlong backslash run is conservatively treated as escaped."
  (let ((cursor (1- index))
        (remaining piem--max-text-file-candidate-length)
        (slashes 0))
    (while (and (>= cursor 0) (> remaining 0)
                (eq (aref text cursor) ?\\))
      (setq slashes (1+ slashes)
            cursor (1- cursor)
            remaining (1- remaining)))
    (or (and (= remaining 0) (>= cursor 0)
             (eq (aref text cursor) ?\\))
        (cl-oddp slashes))))

(defun piem--text-wrapper-quote-p (text index quote opening)
  "Return non-nil when TEXT at INDEX is a usable QUOTE wrapper.
OPENING selects the boundary direction.  Backslash-escaped quotes are literal.
Single quotes use prose boundaries; backticks use the same conservative
orientation, except an unescaped backtick after an even backslash run may open."
  (and (eq (aref text index) quote)
       (not (piem--escaped-text-quote-p text index))
       (or (piem--single-quote-boundary-p text index opening)
           (and opening (eq quote ?`) (> index 0)
                (eq (aref text (1- index)) ?\\)))))

(defun piem--text-wrapper-bounds-at-index (text index quote)
  "Return non-crossing QUOTE wrapper bounds around INDEX in TEXT.
The returned cons excludes delimiters.  Backtick delimiter runs pair only with
runs of the same length, so ordinary Markdown double-backtick spans work while
embedded shorter runs remain content.  Single quotes remain one-character
prose wrappers.  Pairing scans only bounded TEXT from left to right.  A new
unambiguous opener supersedes an unmatched opener; a matching closer, including
a whitespace-ambiguous delimiter, closes the active opener.  Thus completed
spans on either side of INDEX cannot cross-pair, while an unrelated unmatched
earlier opener cannot poison a later local pair.  Overlong contents still
return bounds so the wrapper remains authoritative-invalid rather than exposing
an inner path token."
  (let ((cursor 0)
        (limit (length text))
        opening found)
    (while (and (not found) (< cursor limit))
      (if (or (not (eq (aref text cursor) quote))
              (piem--escaped-text-quote-p text cursor))
          (setq cursor (1+ cursor))
        (let* ((run-start cursor)
               (run-end
                (if (eq quote ?`)
                    (progn
                      (while (and (< cursor limit)
                                  (eq (aref text cursor) quote))
                        (setq cursor (1+ cursor)))
                      cursor)
                  (1+ cursor)))
               (run-length (- run-end run-start))
               (opens (piem--text-wrapper-quote-p
                       text run-start quote t))
               (closes (piem--text-wrapper-quote-p
                        text (1- run-end) quote nil)))
          (cond
           ((null opening)
            (when opens
              (setq opening (list run-start run-end run-length))))
           ((and closes (= run-length (nth 2 opening)))
            (when (<= (nth 1 opening) index run-start)
              (setq found (cons (nth 1 opening) run-start)))
            (setq opening nil))
           (opens
            (setq opening (list run-start run-end run-length))))
          (setq cursor run-end))))
    found))

(defun piem--text-wrapper-extent (text bounds quote)
  "Return BOUNDS expanded to include surrounding QUOTE delimiters in TEXT."
  (let ((start (1- (car bounds)))
        (end (cdr bounds)))
    (when (eq quote ?`)
      (while (and (> start 0) (eq (aref text (1- start)) quote))
        (setq start (1- start)))
      (while (and (< end (length text)) (eq (aref text end) quote))
        (setq end (1+ end))))
    (cons start (if (eq quote ?`) end (1+ end)))))

(defun piem--quoted-text-file-candidate-at-index (text index)
  "Return a quoted file candidate surrounding INDEX in TEXT, or nil.
Only the nearest unescaped wrapper pair of each supported kind is considered,
so unrelated or unmatched earlier wrappers cannot poison local lookup.
Markdown backtick spans take authority over nested prose single quotes."
  (catch 'authoritative-wrapper
    (dolist (quote '(?` ?'))
      (when-let* ((bounds
                   (piem--text-wrapper-bounds-at-index
                    text index quote)))
        (throw
         'authoritative-wrapper
         (when-let* ((candidate
                      (piem--parse-text-file-candidate
                       (substring text (car bounds) (cdr bounds))
                       t (car bounds) (cdr bounds)))
                     (candidate-bounds (plist-get candidate :bounds))
                     ((<= (car candidate-bounds) index
                          (cdr candidate-bounds)))
                     ((piem--text-file-diagnostic-context-p
                       candidate text (cdr bounds) (1+ (cdr bounds)))))
           (plist-put candidate :extent
                      (piem--text-wrapper-extent
                       text bounds quote))))))))

(defun piem--inside-text-wrapper-p (text index)
  "Return non-nil when INDEX is inside a supported wrapper in TEXT."
  (seq-some (lambda (quote)
              (piem--text-wrapper-bounds-at-index
               text index quote))
            '(?' ?`)))

(defun piem--text-file-whitespace-p (character)
  "Return non-nil when CHARACTER delimits an unquoted candidate."
  (memq character '(?\s ?\t ?\n ?\r)))

(defun piem--unquoted-text-file-candidate-at-index (text index)
  "Return the whitespace-bounded candidate at INDEX in TEXT, or nil.
Scanning stops after `piem--max-text-file-candidate-length'
characters in either direction.  Markdown wrappers and trailing punctuation
are excluded exactly as in the strict first-slice grammar."
  (let* ((length (length text))
         (probe (cond
                 ((and (< index length)
                       (not (piem--text-file-whitespace-p
                             (aref text index))))
                  index)
                 ((and (> index 0)
                       (not (piem--text-file-whitespace-p
                             (aref text (1- index)))))
                  (1- index)))))
    (when probe
      (let ((begin probe)
            (end (1+ probe))
            (remaining piem--max-text-file-candidate-length)
            overlong token-begin token-end)
        (while (and (> begin 0)
                    (not (piem--text-file-whitespace-p
                          (aref text (1- begin))))
                    (> remaining 0))
          (setq begin (1- begin)
                remaining (1- remaining)))
        (when (and (> begin 0)
                   (not (piem--text-file-whitespace-p
                         (aref text (1- begin)))))
          (setq overlong t))
        (setq remaining piem--max-text-file-candidate-length)
        (while (and (< end length)
                    (not (piem--text-file-whitespace-p
                          (aref text end)))
                    (> remaining 0))
          (setq end (1+ end)
                remaining (1- remaining)))
        (when (and (< end length)
                   (not (piem--text-file-whitespace-p
                         (aref text end))))
          (setq overlong t))
        (unless (or overlong
                    (> (- end begin)
                       piem--max-text-file-candidate-length))
          (setq token-begin begin
                token-end end)
          (let (html-closing-tag)
            (while (and (< begin end)
                        (memq (aref text begin) '(?\( ?\[ ?\{)))
              (setq begin (1+ begin)))
            (setq html-closing-tag
                  (and (< (1+ begin) end)
                       (eq (aref text begin) ?<)
                       (eq (aref text (1+ begin)) ?/)))
            (while (and (< begin end) (eq (aref text begin) ?<))
              (setq begin (1+ begin)))
            (while (and (< begin end)
                        (memq (aref text (1- end))
                              (string-to-list ".,;!?)]}>")))
              (setq end (1- end)))
            (when (and (<= begin index end)
                       (not html-closing-tag))
              (let ((raw (substring text begin end)))
                ;; Quote-aware parsing owns tokens containing supported quotes.
                (unless (string-match-p "[`']" raw)
                  (when-let* ((candidate
                               (piem--parse-text-file-candidate
                                raw nil begin end))
                              (candidate-bounds
                               (plist-get candidate :bounds))
                              ((<= begin index (cdr candidate-bounds)))
                              ((or
                                (not (plist-get
                                      candidate :diagnostic-separator))
                                (and (= end token-end)
                                     (piem--text-file-diagnostic-context-p
                                      candidate text end token-end)))))
                    (plist-put candidate :extent
                               (cons token-begin token-end))))))))))))

(defun piem--text-file-candidate-at-index (text index)
  "Return the strict file candidate at INDEX in one-line TEXT, or nil.
Lookup is exact rather than nearest-on-line: INDEX must fall within, or at the
exclusive end boundary of, the candidate text.  Work and allocations are
bounded around INDEX; unrelated candidates are never collected or parsed."
  (when (and (stringp text) (integerp index)
             (<= 0 index) (<= index (length text)))
    (or (piem--quoted-text-file-candidate-at-index text index)
        (piem--unquoted-text-file-candidate-at-index text index))))

(defun piem--bounded-line-window-at-point ()
  "Return bounded current-line window metadata around point.
The plist contains `:start', `:end', `:start-complete', and `:end-complete'.
Complete edges are real line or buffer boundaries; incomplete edges are the
explicit candidate scan limit.  No operation searches farther than that limit."
  (let* ((origin (point))
         (lower (max (point-min)
                     (- origin piem--text-file-input-radius)))
         (upper (min (point-max)
                     (+ origin piem--text-file-input-radius)))
         start end start-complete end-complete)
    (save-excursion
      (goto-char origin)
      (if (search-backward "\n" lower t)
          (setq start (1+ (point))
                start-complete t)
        (setq start lower
              start-complete (= lower (point-min))))
      (goto-char origin)
      (if (search-forward "\n" upper t)
          (setq end (1- (point))
                end-complete t)
        (setq end upper
              end-complete (= upper (point-max)))))
    (list :start start :end end
          :start-complete start-complete :end-complete end-complete)))

(defun piem--raw-text-file-candidate-visible-p
    (candidate window-start)
  "Return non-nil when raw CANDIDATE text is visible at WINDOW-START.
Wrapper delimiters are outside candidate bounds.  Hidden text inside the raw
candidate instead belongs to visible projection, which will derive a candidate
from what the user actually sees."
  (let* ((bounds (plist-get candidate :bounds))
         (start (+ window-start (car bounds)))
         (end (+ window-start (cdr bounds))))
    (equal (plist-get candidate :raw)
           (substring-no-properties
            (piem--visible-text start end)))))

(defun piem--raw-diagnostic-context-visible-p
    (candidate window-start)
  "Return non-nil when CANDIDATE's context at WINDOW-START is visible.
Hidden Markdown may not fabricate the separator, required space, or first
diagnostic character.  Ordinary non-diagnostic candidates always pass."
  (or (not (plist-get candidate :diagnostic-separator))
      (let* ((bounds (plist-get candidate :bounds))
             (separator (+ window-start (cdr bounds)))
             (following
              (+ window-start
                 (cdr (or (plist-get candidate :extent) bounds)))))
        (and (piem--visible-text-span-p separator)
             (piem--visible-text-span-p following)
             (piem--visible-text-span-p (1+ following))))))

(defun piem--target-closing-markdown-source-p (start end)
  "Return non-nil when omitted source in START..END only closes target markup.
Accept emphasis/code delimiter runs or one link-label close plus a simple hidden
destination and trailing delimiters.  This intentionally narrow grammar keeps
unrelated invisible text from fabricating diagnostic context."
  (let ((source (buffer-substring-no-properties start end)))
    (or (string-empty-p source)
        (string-match-p "\\`[*_`]+\\'" source)
        (string-match-p
         "\\`]\\(?:([^ \t\r\n()]*)\\)?[*_`]*\\'" source))))

(defun piem--visible-diagnostic-context-source-p
    (candidate visible-input)
  "Return non-nil when projected CANDIDATE has literal diagnostic text.
VISIBLE-INPUT supplies its visible-to-source position map.  Only narrow
source-proven target-closing Markdown may lie between the separator and its
visible space.  The first diagnostic character must immediately follow that
space in source.  Thus unrelated hidden markup cannot fabricate the grammar."
  (or (not (plist-get candidate :diagnostic-separator))
      (let* ((separator (cdr (plist-get candidate :bounds)))
             (positions (plist-get visible-input :positions))
             (separator-source (aref positions separator))
             (space-source (aref positions (1+ separator)))
             (diagnostic-source (aref positions (+ separator 2))))
        (and (= diagnostic-source (1+ space-source))
             (piem--target-closing-markdown-source-p
              (1+ separator-source) space-source)))))

(defun piem--visible-file-candidate-buffer-bounds
    (candidate visible-input)
  "Map nonempty visible CANDIDATE bounds through VISIBLE-INPUT.
Return the real buffer envelope.  Internal hidden markup remains inside that
envelope, while hidden delimiters and link destinations remain outside it."
  (let* ((bounds (plist-get candidate :bounds))
         (start (car bounds))
         (end (cdr bounds))
         (positions (plist-get visible-input :positions)))
    (when (< start end)
      (cons (aref positions start)
            (1+ (aref positions (1- end)))))))

(defun piem--markdown-code-span-at-point-p (start end)
  "Return non-nil when point is fontified as inline code in START..END.
Check both sides of a visible boundary.  This preserves authoritative code-span
semantics when its delimiters are outside the bounded resolver window."
  (seq-some
   (lambda (position)
     (let ((face (and (<= start position) (< position end)
                      (get-char-property position 'face))))
       (or (eq face 'md-ts-code)
           (and (listp face) (memq 'md-ts-code face)))))
   (list (point) (1- (point)))))

(define-error 'piem-semantic-link-parser-error
  "Semantic Markdown parser failure")

(defvar piem--semantic-link-resolver-parsers nil
  "Dynamically bound identities of live semantic resolver parsers.
The `:active' sentinel distinguishes resolver lookup from direct helper tests.
A parser remains registered only when its direct deletion fails, allowing outer
cleanup to retry that exact identity without claiming unrelated new parsers.")

(defvar piem--semantic-code-span-at-point nil
  "Dynamically record code-span ownership during semantic resolution.
The `:active' sentinel limits this side channel to the public resolver call;
direct capture-helper tests do not mutate global state.")

(defconst piem--semantic-link-query
  '(((inline_link) @link)
    ((image) @link)
    ((full_reference_link) @link)
    ((collapsed_reference_link) @link)
    ((shortcut_link) @link)
    ((code_span) @opaque)
    ((html_tag) @opaque)
    ((link_destination) @opaque)
    ((link_title) @opaque))
  "Tree-sitter query for Markdown links relevant to file ownership.")

(defconst piem--max-semantic-link-host-length (* 256 1024)
  "Maximum characters parsed as one semantic Markdown inline host.
Semantic parsing must see a complete canonical `inline' or `pipe_table_cell'
host: parsing a clipped fragment can lose real ownership or invent links inside
an enclosing construct.  A 262,144-character cap keeps press-time native parser
work and recovery scanning explicitly bounded while covering the existing
100,000-character generated-line case and labels well beyond the old 16,388
character plain-text window.  Hosts beyond this cap fail closed and suppress
strict text fallback.")

(defun piem--semantic-link-child (node type)
  "Return NODE's first named child whose tree-sitter type is TYPE."
  (seq-find (lambda (child)
              (string= (treesit-node-type child) type))
            (treesit-node-children node t)))

(defun piem--semantic-link-code-projection (pairs)
  "Normalize code-span PAIRS while retaining one source position per character.
PAIRS are (CHARACTER . BUFFER-POSITION) entries with delimiters removed.
Markdown code spans render line endings as spaces.  When non-space content is
wrapped by a space at both ends, one space is removed from each end."
  (let (normalized)
    (while pairs
      (let* ((pair (pop pairs))
             (character (car pair)))
        (cond
         ((and (eq character ?\r) pairs (eq (caar pairs) ?\n))
          (push (cons ?\s (cdr pair)) normalized)
          (pop pairs))
         ((memq character '(?\r ?\n))
          (push (cons ?\s (cdr pair)) normalized))
         (t (push pair normalized)))))
    (setq normalized (nreverse normalized))
    (if (and (> (length normalized) 2)
             (eq (caar normalized) ?\s)
             (eq (car (car (last normalized))) ?\s)
             (seq-some (lambda (pair) (not (eq (car pair) ?\s))) normalized))
        (butlast (cdr normalized))
      normalized)))

(defun piem--semantic-link-label-projection (label)
  "Return rendered source projection for tree-sitter LABEL.
The returned plist has rendered `:raw-text', control-safe `:text', and a
`:positions' vector parallel to `:raw-text'.  Its `:bounds' is the source
envelope from the
first through last emitted label character.  Emphasis, strong, and code
delimiters are omitted; punctuation escapes emit only their escaped character;
and nested links/images emit only their recursively rendered label text.  Every
emitted character keeps its exact source position, so internal hidden markup
remains inside the envelope but never becomes actionable.

Traversal uses an explicit work stack, so deeply nested labels remain bounded
by the complete host cap rather than Emacs Lisp recursion depth.  Projection
derives solely from source and tree shape, never from font-lock, invisibility,
faces, display, buttons, or overlays."
  (let ((work (list (list :node label)))
        output)
    (while work
      (let* ((action (pop work))
             (kind (car action)))
        (pcase kind
          (:source
           (let ((start (nth 1 action))
                 (end (nth 2 action)))
             (while (< start end)
               (push (cons (char-after start) start) output)
               (setq start (1+ start)))))
          (:pairs
           (dolist (pair (nth 1 action))
             (push pair output)))
          (:node
           (let* ((node (nth 1 action))
                  (type (treesit-node-type node)))
             (cond
              ((member type '("emphasis_delimiter" "code_span_delimiter"
                              "html_tag")))
              ((string= type "backslash_escape")
               (push (list :source
                           (min (treesit-node-end node)
                                (1+ (treesit-node-start node)))
                           (treesit-node-end node))
                     work))
              ((and (string= type "image")
                    (or (piem--semantic-link-child
                         node "link_destination")
                        (piem--semantic-link-child
                         node "link_label")))
               (when-let* ((description
                            (piem--semantic-link-child
                             node "image_description")))
                 (push (list :node description) work)))
              ((and (member type '("inline_link" "full_reference_link"
                                   "collapsed_reference_link"))
                    (or (not (string= type "inline_link"))
                        (piem--semantic-link-child
                         node "link_destination")))
               (when-let* ((text (piem--semantic-link-child
                                  node "link_text")))
                 (push (list :node text) work)))
              ((string= type "code_span")
               (let* ((delimiters
                       (seq-filter
                        (lambda (child)
                          (string= (treesit-node-type child)
                                   "code_span_delimiter"))
                        (treesit-node-children node t)))
                      (start (and delimiters
                                  (treesit-node-end (car delimiters))))
                      (end (and delimiters
                                (treesit-node-start (car (last delimiters))))))
                 (when (and start end (<= start end))
                   (let (pairs)
                     (while (< start end)
                       (push (cons (char-after start) start) pairs)
                       (setq start (1+ start)))
                     (push (list :pairs
                                 (piem--semantic-link-code-projection
                                  (nreverse pairs)))
                           work)))))
              (t
               (let ((cursor (treesit-node-start node))
                     actions)
                 (dolist (child (treesit-node-children node t))
                   (when (< cursor (treesit-node-start child))
                     (push (list :source cursor (treesit-node-start child))
                           actions))
                   (push (list :node child) actions)
                   (setq cursor (treesit-node-end child)))
                 (when (< cursor (treesit-node-end node))
                   (push (list :source cursor (treesit-node-end node)) actions))
                 (setq work (nconc (nreverse actions) work))))))))))
    (let ((pairs (nreverse output)))
      (when pairs
        (let* ((raw-text (apply #'string (mapcar #'car pairs)))
               (positions (vconcat (mapcar #'cdr pairs))))
          (list :raw-text raw-text
                :text (piem--escape-control-chars-for-display
                       raw-text)
                :positions positions
                :bounds (cons (aref positions 0)
                              (1+ (aref positions
                                        (1- (length positions)))))))))))

(defun piem--semantic-link-parent-owner (node)
  "Return a semantic label owner containing nested NODE, or nil.
Inline and reference hyperlinks own nested image alt text.  A standalone image
likewise owns any nested link or image source in its visible description.  The
ancestor must physically contain NODE in its `link_text' or
`image_description', so destination ancestry can never establish ownership."
  (let ((ancestor (treesit-node-parent node))
        owner)
    (while (and ancestor (not owner))
      (let* ((type (treesit-node-type ancestor))
             (label-type (if (string= type "image")
                             "image_description" "link_text")))
        (when (member type '("inline_link" "image" "full_reference_link"
                             "collapsed_reference_link" "shortcut_link"))
          (when-let* ((label (piem--semantic-link-child
                              ancestor label-type))
                      ((<= (treesit-node-start label)
                           (treesit-node-start node)
                           (treesit-node-end node)
                           (treesit-node-end label))))
            (setq owner ancestor))))
      (setq ancestor (treesit-node-parent ancestor)))
    owner))

(defun piem--semantic-link-fragment-index (destination)
  "Return the first unescaped fragment marker index in DESTINATION.
Backslash-escaped punctuation follows Markdown destination source semantics."
  (let ((index 0)
        (limit (length destination))
        found)
    (while (and (< index limit) (not found))
      (cond
       ((and (eq (aref destination index) ?\\)
             (< (1+ index) limit)
             (string-match-p "[[:punct:]]"
                             (char-to-string
                              (aref destination (1+ index)))))
        (setq index (+ index 2)))
       ((eq (aref destination index) ?#)
        (setq found index))
       (t (setq index (1+ index)))))
    found))

(defun piem--semantic-link-unescape (destination)
  "Decode basic Markdown punctuation escapes in local DESTINATION.
Other backslashes remain literal and are subsequently rejected by the strict
file-path grammar rather than being assigned broader path semantics."
  (let ((index 0)
        (limit (length destination))
        pieces)
    (while (< index limit)
      (if (and (eq (aref destination index) ?\\)
               (< (1+ index) limit)
               (string-match-p "[[:punct:]]"
                               (char-to-string
                                (aref destination (1+ index)))))
          (progn
            (push (char-to-string (aref destination (1+ index))) pieces)
            (setq index (+ index 2)))
        (push (char-to-string (aref destination index)) pieces)
        (setq index (1+ index))))
    (apply #'concat (nreverse pieces))))

(defun piem--semantic-link-malformed-end (start limit)
  "Return malformed inline-link ownership end after shortcut ending at START.
A shortcut immediately followed by `(' is the installed grammar's recovery
shape for an incomplete or malformed inline link.  Scan no farther than LIMIT,
which is the end of a complete capped host.  Backslash escapes skip the next
character; bare destinations balance nested parentheses; an angle destination
is recognized only at destination start and ignores parentheses through its
unescaped `>'; and title quotes/parentheses are recognized only immediately
after destination whitespace.  Return the position after the real outer close,
or LIMIT for an incomplete streamed tail."
  (when (and (< start limit) (eq (char-after start) ?\())
    (let ((cursor (1+ start))
          (depth 1)
          (phase :destination)
          (destination-start t)
          angle quote done)
      (while (and (< cursor limit) (not done))
        (let ((character (char-after cursor)))
          (cond
           (quote
            (cond
             ((eq character ?\\)
              (setq cursor (min limit (+ cursor 2))))
             ((eq character quote)
              (setq quote nil
                    phase :after-title
                    cursor (1+ cursor)))
             (t (setq cursor (1+ cursor)))))
           (angle
            ;; CommonMark angle destinations cannot contain line endings, even
            ;; after a backslash.  Leave invalid angle state at that boundary
            ;; so the next outer close terminates malformed ownership.
            (cond
             ((memq character '(?\n ?\r))
              (setq angle nil
                    phase :trailing
                    cursor (1+ cursor)))
             ((and (eq character ?\\) (< (1+ cursor) limit))
              (if (memq (char-after (1+ cursor)) '(?\n ?\r))
                  (setq angle nil
                        phase :trailing)
                (setq destination-start nil))
              (setq cursor (min limit (+ cursor 2))))
             ((eq character ?>)
              (setq angle nil
                    phase :destination-ended
                    cursor (1+ cursor)))
             (t (setq cursor (1+ cursor)))))
           ;; Escape parity follows naturally: skipping a pair leaves the next
           ;; backslash or close to be interpreted on the following iteration.
           ((eq character ?\\)
            (cond
             ((eq phase :destination) (setq destination-start nil))
             ((memq phase '(:destination-ended :after-destination
                             :after-title))
              (setq phase :trailing)))
            (setq cursor (min limit (+ cursor 2))))
           ((and (eq phase :destination) destination-start
                 (eq character ?<))
            (setq angle t
                  destination-start nil
                  cursor (1+ cursor)))
           ((and (eq phase :destination) (= depth 1)
                 (memq character '(?\s ?\t ?\n ?\r)))
            (setq phase :after-destination
                  cursor (1+ cursor)))
           ((and (eq phase :destination-ended)
                 (memq character '(?\s ?\t ?\n ?\r)))
            (setq phase :after-destination
                  cursor (1+ cursor)))
           ((and (memq phase '(:after-destination :after-title))
                 (memq character '(?\s ?\t ?\n ?\r)))
            (setq cursor (1+ cursor)))
           ((and (eq phase :after-destination)
                 (memq character '(?\" ?')))
            (setq quote character
                  cursor (1+ cursor)))
           ((and (eq phase :after-destination) (eq character ?\())
            (setq phase :parenthesized-title
                  depth (1+ depth)
                  cursor (1+ cursor)))
           ((and (eq phase :parenthesized-title) (eq character ?\())
            (setq depth (1+ depth)
                  cursor (1+ cursor)))
           ((and (eq phase :destination) (eq character ?\())
            (setq destination-start nil
                  depth (1+ depth)
                  cursor (1+ cursor)))
           ((eq character ?\))
            (setq depth (1- depth)
                  cursor (1+ cursor))
            (cond
             ((= depth 0) (setq done t))
             ((and (= depth 1) (eq phase :parenthesized-title))
              (setq phase :after-title))))
           ((memq phase '(:after-destination :after-title))
            (setq phase :trailing
                  cursor (1+ cursor)))
           (t
            (cond
             ((eq phase :destination) (setq destination-start nil))
             ((eq phase :destination-ended) (setq phase :trailing)))
            (setq cursor (1+ cursor))))))
      (if done cursor limit))))

(defun piem--semantic-link-markdown-host-parser ()
  "Return md-ts-mode's trustworthy canonical Markdown host parser.
The canonical host parser is the sole unrestricted Markdown parser whose
actual root covers the accessible buffer and lookup position.  md-ts-mode uses
that shape for its document parser and restricted, no-reuse parsers for local
work.  Requiring this shape avoids parser-list ordering and refuses ambiguous
or partial host state rather than querying the wrong Markdown document."
  (let ((positions (delete-dups
                    (list (point)
                          (max (point-min) (1- (point))))))
        candidates)
    (dolist (parser (treesit-parser-list))
      ;; Test included ranges before asking for a root: arbitrary restricted
      ;; parsers are not trusted and cannot break canonical selection.
      (when (and (eq (treesit-parser-language parser) 'markdown)
                 (null (treesit-parser-included-ranges parser)))
        (let ((root (treesit-parser-root-node parser)))
          (when (and (eq (treesit-node-language root) 'markdown)
                     (<= (treesit-node-start root) (point-min))
                     (>= (treesit-node-end root) (point-max))
                     (seq-every-p
                      (lambda (position)
                        (<= (treesit-node-start root) position
                            (treesit-node-end root)))
                      positions))
            (push parser candidates)))))
    (unless (= (length candidates) 1)
      (signal 'piem-semantic-link-parser-error
              '("No unique canonical Markdown host parser")))
    (car candidates)))

(defun piem--semantic-link-inline-host-node-at-point ()
  "Return the Markdown host node whose inline grammar owns point.
Installed md-ts-mode injects `markdown-inline' only into Markdown `inline' and
`pipe_table_cell' nodes.  Consult its canonical unrestricted host tree so
fenced/indented code, reference definitions, restricted competing parsers, and
other block source cannot be reinterpreted merely because it resembles inline
syntax."
  (let* ((parser (piem--semantic-link-markdown-host-parser))
         (root (treesit-parser-root-node parser)))
    (catch 'host
      (dolist (position (delete-dups
                         (list (point)
                               (max (point-min) (1- (point))))))
        (let ((node (treesit-node-descendant-for-range
                     root position position)))
          (while node
            (when (member (treesit-node-type node)
                          '("inline" "pipe_table_cell"))
              (throw 'host node))
            (setq node (treesit-node-parent node))))))))

(defun piem--semantic-link-host-at-point ()
  "Return complete canonical inline-host metadata at point, or nil.
Only the host types used by md-ts-mode's established local inline range rules,
`inline' and `pipe_table_cell', are accepted.  The returned plist contains the
complete host bounds and `:over-cap' when safely parsing that host would exceed
`piem--max-semantic-link-host-length'.  No clipped source fragment is
ever presented to the inline grammar."
  (when-let* ((host (piem--semantic-link-inline-host-node-at-point)))
    (let ((start (treesit-node-start host))
          (end (treesit-node-end host)))
      (when (<= start (point) end)
        (list :start start :end end
              :over-cap
              (> (- end start)
                 piem--max-semantic-link-host-length))))))

(defun piem--semantic-link-range-projection (start end)
  "Project rendered inline source from START through END in the current buffer.
This is used only for a grammar recovery label whose outer link node was lost
because the inline grammar cannot resolve an inner shortcut reference without
document-wide definitions."
  (let ((parser (treesit-parser-create 'markdown-inline nil t)))
    (when piem--semantic-link-resolver-parsers
      (push parser piem--semantic-link-resolver-parsers))
    (unwind-protect
        (progn
          (treesit-parser-set-included-ranges parser (list (cons start end)))
          (piem--semantic-link-label-projection
           (treesit-parser-root-node parser)))
      (treesit-parser-delete parser)
      (setq piem--semantic-link-resolver-parsers
            (delq parser piem--semantic-link-resolver-parsers)))))

(defun piem--semantic-link-reparse-shortcut-outer (candidate)
  "Return detached outer owner recovered from shortcut CANDIDATE.
CANDIDATE records source `:start', `:open', `:label-end', and `:end'.
Temporarily copy only that bounded source and replace nested shortcut brackets
with equal-width braces.  This models this phase's explicit unresolved-shortcut
deferral without changing the chat buffer or shifting source offsets."
  (let* ((source-start (plist-get candidate :start))
         (open (plist-get candidate :open))
         (label-end (plist-get candidate :label-end))
         (source-end (plist-get candidate :end))
         (source (buffer-substring-no-properties source-start source-end))
         metadata)
    ;; The temporary parse establishes only the recovered outer destination.
    ;; Neutralize every nested square delimiter in its label at equal width;
    ;; the original source is projected independently below, preserving valid
    ;; nested image rendering and literal malformed/shortcut text exactly.
    (let ((cursor (- (1+ open) source-start))
          (limit (- label-end source-start)))
      (while (< cursor limit)
        (pcase (aref source cursor)
          (?\[ (aset source cursor ?{))
          (?\] (aset source cursor ?})))
        (setq cursor (1+ cursor))))
    (with-temp-buffer
      (insert source)
      (let ((parser (treesit-parser-create 'markdown-inline nil t)))
        (when piem--semantic-link-resolver-parsers
          (push parser piem--semantic-link-resolver-parsers))
        (unwind-protect
            (let* ((root (treesit-parser-root-node parser))
                   (outer
                    (seq-find
                     (lambda (capture)
                       (let ((node (cdr capture)))
                         (and (= (treesit-node-start node) (point-min))
                              (= (treesit-node-end node) (point-max))
                              (member (treesit-node-type node)
                                      '("inline_link" "image")))))
                     (treesit-query-capture
                      root piem--semantic-link-query
                      (point-min) (point-max)))))
              (when outer
                (let* ((node (cdr outer))
                       (type (treesit-node-type node))
                       (destination
                        (piem--semantic-link-child
                         node "link_destination")))
                  (when destination
                    (setq metadata
                          (list
                           :type type
                           :destination-start
                           (+ source-start
                              (- (treesit-node-start destination)
                                 (point-min)))
                           :destination-end
                           (+ source-start
                              (- (treesit-node-end destination)
                                 (point-min)))))))))
          (treesit-parser-delete parser)
          (setq piem--semantic-link-resolver-parsers
                (delq parser
                      piem--semantic-link-resolver-parsers)))))
    (if metadata
        (append
         (list :start source-start
               :end source-end
               :label-start (1+ open)
               :label-end label-end
               :label-projection
               (piem--semantic-link-range-projection
                (1+ open) label-end)
               :parent-owner-start nil
               :reference-image nil
               :malformed nil)
         metadata)
      (list :type "shortcut_link" :start source-start :end source-end
            :malformed source-end))))

(defun piem--semantic-link-unescaped-bang-before-p (position start)
  "Return non-nil when POSITION follows an unescaped `!' after START."
  (when (and (> position start) (eq (char-before position) ?!))
    (let ((cursor (- position 2))
          (backslashes 0))
      (while (and (>= cursor start) (eq (char-after cursor) ?\\))
        (setq backslashes (1+ backslashes)
              cursor (1- cursor)))
      (zerop (% backslashes 2)))))

(defun piem--semantic-link-recover-shortcut-outer
    (captures start end position)
  "Recover an outer construct from CAPTURES in START..END at POSITION.
The installed inline grammar treats every shortcut as a link before definition
lookup and therefore cannot emit a containing hyperlink.  This phase explicitly
defers shortcut definitions, so balance source brackets once, find an enclosing
outer label followed by a parenthesized tail, and reparse only that complete
bounded construct with nested shortcuts neutralized."
  (let ((shortcut-starts (make-hash-table :test #'eql))
        (open-for-shortcut (make-hash-table :test #'eql))
        (close-for-open (make-hash-table :test #'eql))
        (opaque-ends (make-hash-table :test #'eql))
        (seen-opens (make-hash-table :test #'eql))
        stack outer-image best)
    (dolist (capture captures)
      (cond
       ((string= (plist-get capture :type) "shortcut_link")
        (puthash (plist-get capture :start) t shortcut-starts))
       ((member (plist-get capture :type)
                '("code_span" "html_tag" "link_destination" "link_title"))
        (puthash (plist-get capture :start)
                 (plist-get capture :end) opaque-ends))))
    (let ((cursor start))
      (while (< cursor end)
        (let ((character (char-after cursor)))
          (cond
           ((gethash cursor opaque-ends)
            (setq cursor (gethash cursor opaque-ends)))
           ((eq character ?\\)
            (setq cursor (min end (+ cursor 2))))
           ((eq character ?\[)
            (when (gethash cursor shortcut-starts)
              ;; Only the nearest lexical opener and outermost image opener
              ;; can establish additional ownership.  Persisting/traversing
              ;; the full stack for every shortcut would be quadratic.
              (puthash cursor
                       (delete-dups
                        (delq nil (list (caar stack) outer-image)))
                       open-for-shortcut))
            (push (cons cursor outer-image) stack)
            (when (and (not outer-image)
                       (piem--semantic-link-unescaped-bang-before-p
                        cursor start))
              (setq outer-image cursor))
            (setq cursor (1+ cursor)))
           ((eq character ?\])
            (when stack
              (let ((entry (pop stack)))
                (puthash (car entry) cursor close-for-open)
                (when (eq (car entry) outer-image)
                  (setq outer-image (cdr entry)))))
            (setq cursor (1+ cursor)))
           (t (setq cursor (1+ cursor)))))))
    (catch 'recovered
      (dolist (capture captures)
        (dolist (open (gethash (plist-get capture :start)
                               open-for-shortcut))
          (when-let* ((label-end (gethash open close-for-open))
                      ((< label-end end))
                      ((eq (char-after (1+ label-end)) ?\())
                      ((not (gethash open seen-opens)))
                      (seen (puthash open t seen-opens))
                      ((<= open position))
                      (source-end
                       (piem--semantic-link-malformed-end
                        (1+ label-end) end))
                      ((< position source-end)))
            (let* ((imagep
                    (piem--semantic-link-unescaped-bang-before-p
                     open start))
                   (source-start (if imagep (1- open) open))
                   (nested-link
                    (and (not imagep)
                         (seq-some
                          (lambda (inner)
                            (and (member (plist-get inner :type)
                                         '("inline_link"
                                           "full_reference_link"
                                           "collapsed_reference_link"))
                                 (< open (plist-get inner :start))
                                 (<= (plist-get inner :end) label-end)))
                          captures))))
              ;; CommonMark permits links inside image descriptions but never
              ;; links inside links.  Do not fabricate an apparent recovered
              ;; hyperlink by neutralizing a completed nested hyperlink.
              (unless nested-link
                (let ((owner
                       (piem--semantic-link-reparse-shortcut-outer
                        (list :start source-start :open open
                              :label-end label-end :end source-end))))
                  (cond
                   ((not (plist-get owner :malformed))
                    (when (or (not best)
                              (and (string= (plist-get owner :type) "image")
                                   (not (string= (plist-get best :type)
                                                 "image")))
                              (< (plist-get owner :start)
                                 (plist-get best :start)))
                      (setq best owner)))
                   ((not best) (setq best owner)))
                  ;; An incomplete malformed tail consumes the host remainder;
                  ;; no later candidate can supply a containing outer close.
                  (when (and (plist-get owner :malformed)
                             (= source-end end))
                    (throw 'recovered best)))))))))
    best))

(defun piem--semantic-link-select-owner (captures start end position)
  "Select one semantic owner from CAPTURES at POSITION in START..END.
Malformed recovery has source-order priority.  Completed recovery extents are
skipped before scanning later captures, so total malformed scanning is linear
in the complete capped host.  Otherwise label ancestry removes nested activation
captures, and explicit grammar type order selects the owner.  Ambiguous owners
of one semantic type fail closed before label projection."
  (let* ((ordered
          (sort (copy-sequence captures)
                (lambda (left right)
                  (< (plist-get left :start) (plist-get right :start)))))
         (parent-owner-starts (make-hash-table :test #'eql))
         (scanned-through (point-min))
         malformed-owner owners)
    (dolist (capture captures)
      (when-let* ((parent-start
                   (plist-get capture :parent-owner-start)))
        (puthash parent-start t parent-owner-starts)))
    ;; Find the earliest malformed extent that reaches point.  Once an extent
    ;; closes before point, captures nested inside it cannot own point and are
    ;; skipped.  This avoids rescanning the same recovery tail for every nested
    ;; shortcut capture.
    (catch 'owned-malformed
      (dolist (capture ordered)
        (let ((type (plist-get capture :type))
              (node-start (plist-get capture :start))
              (node-end (plist-get capture :end)))
          (when (and (not (plist-get capture :parent-owner-start))
                     (>= node-start scanned-through)
                     (<= node-start position)
                     (or (string= type "shortcut_link")
                         (and (string= type "image")
                              (null (plist-get capture :destination-start))
                              (not (plist-get capture :reference-image)))))
            (when-let* ((malformed-end
                         (piem--semantic-link-malformed-end
                          node-end end)))
              (if (< position malformed-end)
                  (progn
                    (setq malformed-owner
                          (append (list :end malformed-end
                                        :malformed malformed-end)
                                  capture))
                    (throw 'owned-malformed malformed-owner))
                (setq scanned-through (max scanned-through malformed-end))))))))
    (or (piem--semantic-link-recover-shortcut-outer
         ordered start end position)
        malformed-owner
        (progn
          ;; Markdown label ancestry is activation ancestry.  Hyperlinks
          ;; (including unsupported shortcut/reference forms) own nested image
          ;; alt text; standalone images own nested link/image descriptions.
          (dolist (capture captures)
            (let ((type (plist-get capture :type))
                  (node-start (plist-get capture :start))
                  (node-end (plist-get capture :end)))
              (when (and (or (not (string= type "shortcut_link"))
                             (gethash node-start parent-owner-starts))
                         (<= node-start position)
                         (< position node-end))
                (push (append (list :end node-end :malformed nil) capture)
                      owners))))
          (setq owners
                (seq-remove
                 (lambda (owner) (plist-get owner :parent-owner-start))
                 owners))
          (catch 'owner
            (dolist (type '("inline_link" "full_reference_link"
                            "collapsed_reference_link" "shortcut_link"
                            "image"))
              (let ((matches
                     (seq-filter
                      (lambda (owner)
                        (string= (plist-get owner :type) type))
                      owners)))
                (when (> (length matches) 1)
                  (signal 'piem-semantic-link-parser-error
                          (list (format "Ambiguous semantic %s owners" type))))
                (when matches (throw 'owner (car matches))))))))))

(defun piem--semantic-link-captures (start end)
  "Return detached installed-grammar owner metadata in complete START..END.
Use a short-lived independent `markdown-inline' parser so raw, fontified, and
streaming buffers have identical semantics.  Query captures are reduced to the
single semantic owner at point before projecting only that owner's label.  This
keeps deeply nested/recovery trees linear and fails ambiguity closed before
expensive projection.  All node types, bounds, labels, and destinations are
copied to a plist before deleting the parser; no caller observes a tree node
after its parser lifetime.  Installed md-ts-mode 0.3 creates its own local
inline parsers lazily during fontification and exposes no public link resolver.
This parser never changes text, overlays, font-lock properties, visibility, or
the mode's parser set."
  (let ((parser (treesit-parser-create 'markdown-inline nil t)))
    (when piem--semantic-link-resolver-parsers
      (push parser piem--semantic-link-resolver-parsers))
    (unwind-protect
        (progn
          (treesit-parser-set-included-ranges parser (list (cons start end)))
          (let* ((captures
                  (mapcar
                   (lambda (capture)
                     (let* ((node (cdr capture))
                            (type (treesit-node-type node))
                            (label-type (if (string= type "image")
                                            "image_description" "link_text"))
                            (label (piem--semantic-link-child
                                    node label-type))
                            (destination
                             (piem--semantic-link-child
                              node "link_destination"))
                            (parent-owner
                             (piem--semantic-link-parent-owner node)))
                       (list :type type
                             :start (treesit-node-start node)
                             :end (treesit-node-end node)
                             :label-start
                             (and label (treesit-node-start label))
                             :label-end (and label (treesit-node-end label))
                             :label-node label
                             :parent-owner-start
                             (and parent-owner
                                  (treesit-node-start parent-owner))
                             :reference-image
                             (and (string= type "image") label
                                  (null destination)
                                  (> (treesit-node-end node)
                                     (1+ (treesit-node-end label))))
                             :destination-start
                             (and destination (treesit-node-start destination))
                             :destination-end
                             (and destination (treesit-node-end destination)))))
                   (treesit-query-capture
                    (treesit-parser-root-node parser)
                    piem--semantic-link-query start end)))
                 (_code-span
                  (when (eq piem--semantic-code-span-at-point
                            :active)
                    (setq piem--semantic-code-span-at-point
                          (and
                           (seq-some
                            (lambda (capture)
                              (and
                               (string= (plist-get capture :type) "code_span")
                               (seq-some
                                (lambda (position)
                                  (<= (plist-get capture :start) position
                                      (1- (plist-get capture :end))))
                                (delete-dups
                                 (list (point)
                                       (max (point-min) (1- (point))))))))
                            captures)
                           t))))
                 (owner (piem--semantic-link-select-owner
                         captures start end (point))))
            (when owner
              (when (and (not (plist-get owner :malformed))
                         (member (plist-get owner :type)
                                 '("inline_link" "image"))
                         (plist-get owner :destination-start)
                         (plist-get owner :label-node)
                         (not (plist-get owner :label-projection)))
                (plist-put
                 owner :label-projection
                 (piem--semantic-link-label-projection
                  (plist-get owner :label-node))))
              ;; Tree nodes must not escape the short-lived parser.
              (plist-put owner :label-node nil)
              (list owner))))
      (treesit-parser-delete parser)
      (setq piem--semantic-link-resolver-parsers
            (delq parser piem--semantic-link-resolver-parsers)))))

(defun piem--semantic-link-owner-at-point (host)
  "Return the semantic activation owner at point inside complete HOST.
The result is detached metadata containing physical, projected-label, and
destination extents.  Label ancestry decides nested ownership explicitly:
hyperlinks own nested image alt text, while standalone images own nested label
constructs.  Unsupported shortcut references do not own ordinary bracketed
text, preserving the strict wrapper behavior.  However, a shortcut link
or non-reference shortcut image immediately followed by an opening parenthesis
is malformed inline recovery and owns its balanced, escape-aware tail.  Full
and collapsed references always own and suppress fallback."
  (car (piem--semantic-link-captures
        (plist-get host :start) (plist-get host :end))))

(defun piem--semantic-link-target (owner)
  "Return the valid local file target for semantic link OWNER, or nil.
Only an inline link or inline image with a strict local destination qualifies.
URL schemes, mailto links, protocol-relative links, fragment-only links, empty
or malformed destinations, bare filenames, and reference forms are owned but
invalid.  A local fragment is returned separately and is never interpreted as
line metadata."
  (let* ((type (plist-get owner :type))
         (label-projection (plist-get owner :label-projection))
         (label-positions (plist-get label-projection :positions))
         (destination-start (plist-get owner :destination-start))
         (destination-end (plist-get owner :destination-end))
         (position (point)))
    (when (and (not (plist-get owner :malformed))
               (member type '("inline_link" "image"))
               label-positions destination-start destination-end
               ;; A point activates only at the leading source boundary of an
               ;; emitted label character.  No trailing boundary is accepted
               ;; when it is physically a hidden Markdown delimiter.
               (seq-contains-p label-positions position #'=))
      (let* ((raw (buffer-substring-no-properties
                   destination-start destination-end))
             (angle (and (> (length raw) 1)
                         (string-prefix-p "<" raw)
                         (string-suffix-p ">" raw)))
             (source (if angle (substring raw 1 -1) raw))
             (fragment-index
              (piem--semantic-link-fragment-index source))
             (path-source (if fragment-index
                              (substring source 0 fragment-index)
                            source))
             (fragment (and fragment-index
                            (substring source (1+ fragment-index))))
             (path (piem--semantic-link-unescape path-source))
             (case-fold-search t))
        (when (and (not (string-empty-p path))
                   (not (string-prefix-p "#" source))
                   (not (string-prefix-p "//" source))
                   (not (string-match-p
                         "\\`[[:alpha:]][[:alnum:]+.-]*:" source))
                   (piem--strict-text-file-path-p path angle))
          (let* ((anchor (piem--chat-session-directory))
                 (emacs-path (piem--emacs-path path anchor))
                 (label (plist-get label-projection :text)))
            (piem--make-file-target
             :link raw emacs-path
             :bounds (plist-get label-projection :bounds)
             :fragment fragment
             :label label)))))))

(defun piem--semantic-link-parser-overlays ()
  "Return every inline-parser overlay md-ts could adopt in this buffer.
Use `overlay-lists' rather than positional overlay APIs: local ranges are
half-open, so `overlays-at' misses a host whose exclusive end is point.  md-ts
adopts any overlapping overlay carrying a matching parser even when host and
timestamp metadata are incomplete, so all such preexisting identities must be
isolated and restored."
  (seq-filter
   (lambda (overlay)
     (when-let* ((parser (overlay-get overlay 'treesit-parser)))
       (eq (treesit-parser-language parser) 'markdown-inline)))
   (append (car (overlay-lists)) (cdr (overlay-lists)))))

(defun piem--semantic-link-parser-state ()
  "Snapshot parser identities/ranges and complete local-overlay state."
  (list
   :parsers
   (mapcar (lambda (parser)
             (cons parser (treesit-parser-included-ranges parser)))
           (treesit-parser-list))
   :overlays
   (mapcar
    (lambda (overlay)
      (let ((parser (overlay-get overlay 'treesit-parser)))
        (list overlay (overlay-start overlay) (overlay-end overlay)
              parser (treesit-parser-included-ranges parser)
              (overlay-get overlay 'treesit-parser-ov-timestamp))))
    (piem--semantic-link-parser-overlays))))

(defun piem--semantic-link-isolate-parser-state (state)
  "Hide preexisting local parsers in STATE during resolver-owned parsing.
A stale md-ts local parser would otherwise be replaced as a side effect of
lazy range discovery.  Temporarily hiding both identifying properties makes
md-ts create disposable resolver-owned state instead."
  (dolist (entry (plist-get state :overlays))
    (let ((overlay (car entry)))
      (overlay-put overlay 'treesit-parser nil)
      (overlay-put overlay 'treesit-parser-ov-timestamp nil))))

(defun piem--semantic-link-cleanup-parser-state (state)
  "Restore preexisting parser STATE after isolated semantic lookup.
Local range discovery is dynamically disabled, so lookup cannot create or adopt
parser overlays.  Preexisting identities are still restored defensively with
their ranges, bounds, and timestamps.  Cleanup is best-effort across every
identity before its first error is re-signaled."
  (let* ((old-parser-state (plist-get state :parsers))
         (old-overlay-state (plist-get state :overlays))
         cleanup-error)
    (cl-labels ((remember-error
                 (error-data)
                 (unless cleanup-error
                   (setq cleanup-error error-data))))
      (unwind-protect
          (progn
            ;; Retry only an explicitly registered resolver parser whose direct
            ;; unwind deletion failed.  Identity, not "newness", establishes
            ;; ownership and preserves foreign parsers created during lookup.
            (dolist (parser
                     (delq :active
                           (copy-sequence
                            piem--semantic-link-resolver-parsers)))
              (condition-case error-data
                  (progn
                    (treesit-parser-delete parser)
                    (setq piem--semantic-link-resolver-parsers
                          (delq
                           parser
                           piem--semantic-link-resolver-parsers)))
                (error (remember-error error-data))))
            (let ((current-parsers (treesit-parser-list)))
              (dolist (entry old-parser-state)
                (let ((parser (car entry))
                      (ranges (cdr entry)))
                  (when (memq parser current-parsers)
                    (condition-case error-data
                        (unless (equal
                                 ranges
                                 (treesit-parser-included-ranges parser))
                          (treesit-parser-set-included-ranges parser ranges))
                      (error (remember-error error-data))))))))
        ;; Restore every preexisting overlay even if disposal or another
        ;; restoration fails.  Identity comes first so an error cannot leave an
        ;; overlay hidden from md-ts and later lifecycle cleanup.
        (dolist (entry old-overlay-state)
          (let ((overlay (nth 0 entry))
                (start (nth 1 entry))
                (end (nth 2 entry))
                (parser (nth 3 entry))
                (ranges (nth 4 entry))
                (timestamp (nth 5 entry)))
            (when (overlay-buffer overlay)
              (condition-case error-data
                  (progn
                    (overlay-put overlay 'treesit-parser parser)
                    (overlay-put overlay 'treesit-parser-ov-timestamp timestamp)
                    (move-overlay overlay start end)
                    (unless (equal
                             ranges
                             (treesit-parser-included-ranges parser))
                      (treesit-parser-set-included-ranges parser ranges)))
                (error (remember-error error-data)))))))
      (when cleanup-error
        (signal (car cleanup-error) (cdr cleanup-error))))))

(defun piem--semantic-link-file-target-at-point ()
  "Return explicit tri-state semantic Markdown link resolution at point.
The `:status' value is exactly one of `:not-a-link', `:owned-valid', or
`:owned-invalid'.  Ownership is source/tree based, independent of font-lock,
invisibility, faces, buttons, file existence, and unreleased md-ts-mode APIs.
This distinction prevents an owned non-file or malformed link from falling
through to a path-like visible label.

A missing/ambiguous canonical host or any tree/query/node failure signals
`piem-semantic-link-parser-error'; parser failure is never semantic
absence.  Only a complete canonical inline/table-cell host is parsed, up to
`piem--max-semantic-link-host-length'; an over-cap host returns
owned-invalid and cannot reach text fallback.  Lookup snapshots parser state,
isolates every preexisting inline overlay md-ts could adopt, and dynamically
disables local range discovery.  It therefore creates no md-ts parser overlays,
including at half-open host endpoints.  Lookup temporarily widens so buffer
narrowing cannot clip semantic ownership; the caller's restriction is restored."
  (save-restriction
    (widen)
    (let ((piem--semantic-link-resolver-parsers (list :active))
          (piem--semantic-code-span-at-point :active)
          state parse-result)
    (condition-case error-data
        (setq state (piem--semantic-link-parser-state))
      (error
       (signal 'piem-semantic-link-parser-error
               (list (error-message-string error-data)))))
    (unwind-protect
        (setq parse-result
              (condition-case error-data
                  (progn
                    (piem--semantic-link-isolate-parser-state state)
                    (cl-labels
                        ((resolve
                          ()
                          (let* ((host
                                  (piem--semantic-link-host-at-point))
                                 (owner
                                  (and
                                   host
                                   (not (plist-get host :over-cap))
                                   (piem--semantic-link-owner-at-point
                                    host))))
                            (cond
                             ((and host (plist-get host :over-cap))
                              (list :status :owned-invalid
                                    :reason :host-over-cap))
                             ((not owner)
                              (append
                               (list :status :not-a-link)
                               (and
                                (eq piem--semantic-code-span-at-point
                                    t)
                                (list :markdown-code-span t))))
                             (t (list :status :owner :owner owner))))))
                      ;; The resolver queries only the canonical block tree and
                      ;; its own short-lived inline parser.  Disable md-ts range
                      ;; discovery dynamically so lookup cannot create, adopt,
                      ;; or partially initialize local parser overlays at all.
                      (let ((treesit-range-settings nil))
                        (resolve))))
                (piem-semantic-link-parser-error
                 (signal (car error-data) (cdr error-data)))
                (error
                 (signal 'piem-semantic-link-parser-error
                         (list (error-message-string error-data))))))
      (condition-case error-data
          (piem--semantic-link-cleanup-parser-state state)
        (piem-semantic-link-parser-error
         (signal (car error-data) (cdr error-data)))
        (error
         (signal 'piem-semantic-link-parser-error
                 (list (error-message-string error-data))))))
    ;; Path normalization is outside the parser-failure boundary: controlled
    ;; path `user-error' values retain their established target contract.
    (if (eq (plist-get parse-result :status) :owner)
        (if-let* ((target
                   (piem--semantic-link-target
                    (plist-get parse-result :owner))))
            (list :status :owned-valid :target target)
          (list :status :owned-invalid))
      parse-result))))

(defun piem--text-file-target-at-point (&optional markdown-code-span)
  "Return a strict markup-visible file target on the current line, or nil.
MARKDOWN-CODE-SPAN means semantic parsing already established code-span
ownership at point.  This buffer layer snapshots a fixed-size window around
point.  Supported raw quote wrappers complete within that snapshot remain
authoritative, including invalid quoted prose or commands; semantically owned
or fontified Markdown code remains authoritative when its delimiters lie
outside the snapshot.  Otherwise it first uses an
already-visible raw token, then projects backing buffer text according to
fontified chat markup, maps point into that projection, delegates pure candidate
parsing, and maps candidate bounds back to real buffer positions.  It never
widens, fontifies, or scans the whole current line.  Hidden
link destinations and syntax do not become candidate text.

The projection follows backing-buffer text properties, not synthetic overlay
replacement strings such as wrapped table displays, which lack character-level
source positions."
  (let* ((window (piem--bounded-line-window-at-point))
         (window-start (plist-get window :start))
         (window-end (plist-get window :end))
         (text (buffer-substring-no-properties window-start window-end))
         (index (- (point) window-start))
         (wrapped (or markdown-code-span
                      (piem--inside-text-wrapper-p text index)
                      (piem--markdown-code-span-at-point-p
                       window-start window-end)))
         candidate bounds input-length)
    ;; A failed wrapper parse owns the result: never expose an inner path from
    ;; fontified `cat src/foo.el' or single-quoted prose.  Without a wrapper, a
    ;; normal visible token takes the cheap path before projection is allocated.
    (unless (piem--position-inside-omitted-text-p
             (point) window-start window-end)
      (let* ((raw-candidate
              (if wrapped
                  (piem--quoted-text-file-candidate-at-index
                   text index)
                (piem--text-file-candidate-at-index text index)))
             (raw-text-visible
              (and raw-candidate
                   (piem--raw-text-file-candidate-visible-p
                    raw-candidate window-start)))
             (raw-context-visible
              (and raw-candidate
                   (piem--raw-diagnostic-context-visible-p
                    raw-candidate window-start)))
             (raw-visible (and raw-text-visible raw-context-visible))
             ;; An otherwise visible diagnostic candidate with hidden separator
             ;; context is authoritative-invalid.  If instead target markup is
             ;; hidden, normal visible projection remains authoritative.
             (hidden-diagnostic-context
              (and raw-candidate raw-text-visible
                   (plist-get raw-candidate :diagnostic-separator)
                   (not raw-context-visible))))
        (when raw-visible
          (let ((raw-bounds (plist-get raw-candidate :bounds)))
            (setq candidate raw-candidate
                  bounds (cons (+ window-start (car raw-bounds))
                               (+ window-start (cdr raw-bounds)))
                  input-length (length text))))
        (when (and (not candidate) (not wrapped)
                   (not hidden-diagnostic-context))
          (let* ((visible-input
                  (piem--visible-text-with-position-map
                   window-start window-end (point)))
                 (visible-text (plist-get visible-input :text))
                 (visible-candidate
                  (piem--text-file-candidate-at-index
                   visible-text (plist-get visible-input :index))))
            (when (and visible-candidate
                       (piem--visible-diagnostic-context-source-p
                        visible-candidate visible-input))
              (setq candidate visible-candidate
                    bounds
                    (piem--visible-file-candidate-buffer-bounds
                     visible-candidate visible-input)
                    input-length (length visible-text))))))
      (when-let* ((candidate candidate)
                  ;; Lexical extent includes wrappers and punctuation trimmed
                  ;; from returned bounds, so clipped windows cannot fabricate
                  ;; an apparently complete candidate.
                  (extent (or (plist-get candidate :extent)
                              (plist-get candidate :bounds)))
                  (complete-start
                   (or (> (car extent) 0)
                       (plist-get window :start-complete)))
                  (complete-end
                   (or (< (cdr extent) input-length)
                       (plist-get window :end-complete)))
                  (bounds bounds)
                  (raw (plist-get candidate :raw))
                  (path (plist-get candidate :path))
                  (anchor (piem--chat-session-directory))
                  (emacs-path (piem--emacs-path path anchor)))
        (piem--make-file-target
         :text raw emacs-path
         :line (plist-get candidate :line)
         :column (plist-get candidate :column)
         :range (plist-get candidate :range)
         :bounds bounds)))))

(defun piem--tool-overlay-at-point ()
  "Return the authoritative hot tool overlay at physical point, or nil.
Any chat restriction is restored exactly after lookup."
  (save-restriction
    (widen)
    (seq-find (lambda (overlay)
                (overlay-get overlay 'piem-tool-block))
              (overlays-at (point)))))

(defun piem--tool-file-target-from-metadata
    (stored-path raw-path path-error bounds line-function)
  "Return an authoritative tool target from stored metadata, or nil.
STORED-PATH, RAW-PATH, and PATH-ERROR retain the render-time path decision.
BOUNDS identify the owning hot or cold block.  LINE-FUNCTION is called only
for a valid path.  A stored error signals `user-error'; explicit path absence
returns nil."
  (cond
   (stored-path
    (unless (stringp stored-path)
      (user-error "Tool path metadata is not a string"))
    (let* ((emacs-path (piem--tool-emacs-path stored-path))
           (raw (if (stringp raw-path) raw-path stored-path)))
      (piem--make-file-target
       :tool raw emacs-path
       :line (funcall line-function)
       :bounds bounds)))
   (path-error
    (user-error "%s" path-error))))

(defun piem--tool-file-target (overlay)
  "Return the authoritative file target for hot tool OVERLAY, or nil."
  (piem--tool-file-target-from-metadata
   (overlay-get overlay 'piem-tool-path)
   (overlay-get overlay 'piem-tool-raw-path)
   (overlay-get overlay 'piem-tool-path-error)
   (cons (overlay-start overlay) (overlay-end overlay))
   (lambda () (piem--tool-line-at-point overlay))))

(defun piem--cold-tool-block-at-point ()
  "Return authoritative cold-tool metadata and physical bounds at point.
Return nil outside a cold tool.  Any chat restriction is restored exactly."
  (save-restriction
    (widen)
    (when-let* ((metadata
                 (get-text-property (point)
                                    'piem-cold-tool-block)))
      (let ((start (or (previous-single-property-change
                        (1+ (point)) 'piem-cold-tool-block
                        nil (point-min))
                       (point-min)))
            (end (or (next-single-property-change
                      (point) 'piem-cold-tool-block
                      nil (point-max))
                     (point-max))))
        (list :metadata metadata :bounds (cons start end))))))

(defun piem--cold-tool-line-at-point (cold-block)
  "Calculate the physical meaningful file line at point for COLD-BLOCK."
  (save-restriction
    (widen)
    (let* ((metadata (plist-get cold-block :metadata))
           (bounds (plist-get cold-block :bounds))
           (header-length (plist-get metadata :header-length))
           (header-end (and (integerp header-length)
                            (+ (car bounds) header-length)))
           (line-map (plist-get metadata :line-map)))
      (piem--tool-line-from-metadata
       (plist-get metadata :tool-name)
       (plist-get metadata :offset)
       line-map header-end line-map))))

(defun piem--cold-tool-file-target (cold-block)
  "Return the authoritative file target for COLD-BLOCK, or nil."
  (let ((metadata (plist-get cold-block :metadata)))
    (piem--tool-file-target-from-metadata
     (plist-get metadata :path)
     (plist-get metadata :raw-path)
     (plist-get metadata :path-error)
     (plist-get cold-block :bounds)
     (lambda () (piem--cold-tool-line-at-point cold-block)))))

(defun piem--file-target-at-point ()
  "Return the file target at point, or nil.
Resolution priority is authoritative hot/cold tool metadata, semantic Markdown
link ownership, then strict visible text.  Invalid or absent tool metadata never
falls through.  Semantic lookup is explicitly tri-state, so an owned non-file,
reference, or malformed link also never falls through to a path-like label."
  (if-let* ((overlay (piem--tool-overlay-at-point)))
      (piem--tool-file-target overlay)
    (if-let* ((cold-block (piem--cold-tool-block-at-point)))
        (piem--cold-tool-file-target cold-block)
      (pcase (piem--semantic-link-file-target-at-point)
        (`(:status :owned-valid :target ,target) target)
        (`(:status :owned-invalid) nil)
        (`(:status :not-a-link . ,properties)
         (piem--text-file-target-at-point
          (plist-get properties :markdown-code-span)))))))

(defconst piem--shell-execution-buffer-variables
  '(process-environment exec-path shell-file-name shell-command-switch
    process-connection-type coding-system-for-read coding-system-for-write
    default-process-coding-system process-coding-system-alist
    inherit-process-coding-system process-adaptive-read-buffering
    async-shell-command-width comint-terminfo-terminal
    tramp-remote-process-environment)
  "Narrow process-launch variables captured for one shell execution.")

(defconst piem--shell-execution-dynamic-variables
  '(connection-local-profile-alist connection-local-criteria-alist
    connection-local-default-application enable-connection-local-variables)
  "Global connection-local configuration captured for one shell execution.")

(defun piem--copy-shell-execution-value (value)
  "Copy VALUE for an execution snapshot, including nested strings."
  (cond
   ((stringp value) (copy-sequence value))
   ((consp value)
    (cons (piem--copy-shell-execution-value (car value))
          (piem--copy-shell-execution-value (cdr value))))
   ((vectorp value)
    (apply #'vector
           (mapcar #'piem--copy-shell-execution-value value)))
   (t value)))

(defun piem--shell-execution-snapshot (directory)
  "Snapshot shell execution state for DIRECTORY in the current chat buffer.
The snapshot is taken before prompting and owns one command launch.  It includes
DIRECTORY; effective process environment, executable path, shell and switch;
process/coding values; and remote connection configuration used by native
file-process dispatch.  Local asynchronous terminal variables are resolved now
so an existing output buffer cannot replace the invocation environment."
  (require 'comint)
  (let* ((directory (copy-sequence directory))
         (remote-p (file-remote-p directory))
         (buffer-variables
          (if remote-p
              piem--shell-execution-buffer-variables
            (remq 'tramp-remote-process-environment
                  piem--shell-execution-buffer-variables)))
         (buffer-values
          (delq nil
                (mapcar
                 (lambda (variable)
                   (and (boundp variable)
                        (cons variable
                              (piem--copy-shell-execution-value
                               (symbol-value variable)))))
                 buffer-variables)))
         (base-environment
          (piem--copy-shell-execution-value process-environment))
         (default-directory directory)
         (async-environment
          (append
           (and (natnump async-shell-command-width)
                (list (format "COLUMNS=%d" async-shell-command-width)))
           (comint-term-environment)
           base-environment)))
    (list
     :directory directory
     :buffer-values buffer-values
     :async-process-environment async-environment
     :dynamic-values
     ;; A remote handler can recalculate effective values from these global
     ;; tables.  Freeze them only for remote dispatch; local launches need no
     ;; broad connection-profile capture.
     (and remote-p
          (delq nil
                (mapcar
                 (lambda (variable)
                   (and (boundp variable)
                        (cons variable
                              (piem--copy-shell-execution-value
                               (symbol-value variable)))))
                 piem--shell-execution-dynamic-variables))))))

(defun piem--call-with-shell-buffer-values (values function)
  "Call FUNCTION after temporarily installing buffer-local VALUES.
VALUES is an alist from an execution snapshot.  Restore both each value and its
original localness after the launch attempt, including when installation fails."
  (let ((buffer (current-buffer))
        originals)
    (unwind-protect
        (progn
          (dolist (entry values)
            (let ((variable (car entry)))
              (push (list variable
                          (local-variable-p variable buffer)
                          (boundp variable)
                          (and (boundp variable) (symbol-value variable)))
                    originals)
              (set (make-local-variable variable) (cdr entry))))
          (funcall function))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (dolist (original originals)
            (cond
             ((not (nth 1 original))
              (kill-local-variable (car original)))
             ((nth 2 original)
              (set (make-local-variable (car original)) (nth 3 original)))
             (t
              (make-local-variable (car original))
              (makunbound (car original))))))))))

(defun piem--async-shell-mode ()
  "Enter the native asynchronous shell output mode for this Emacs version."
  (if (and (>= emacs-major-version 30)
           (boundp 'async-shell-command-mode))
      (funcall async-shell-command-mode)
    (shell-mode)))

(defun piem--async-shell-display-action (&optional deferred)
  "Return native async display action, with version handling for DEFERRED.
Immediate display uses `allow-no-window' on Emacs 29 and later.  Deferred
first-output display gained that action in Emacs 30."
  (and (or (not deferred) (>= emacs-major-version 30))
       '(nil (allow-no-window . t))))

(defun piem--start-snapshotted-async-shell-command
    (snapshot command &optional output-buffer)
  "Start local asynchronous COMMAND under SNAPSHOT with native shell UI.
COMMAND has no terminal ampersand.  OUTPUT-BUFFER, when non-nil, is the buffer
or name reused by native revert behavior.  This mirrors only the Emacs
29.4/30.1 local async branch that surrounds process launch; compatibility
helpers centralize their mode and display differences.  Preserve native buffer
naming, conflict policy, output retention, process name, sentinel, filter,
revert, and display behavior while replacing only process-launch values."
  (let* ((buffer (get-buffer-create
                  (or output-buffer shell-command-buffer-name-async
                      "*Async Shell Command*")))
         (buffer-name (buffer-name buffer))
         (process (get-buffer-process buffer))
         (directory (plist-get snapshot :directory)))
    (when process
      (cond
       ((eq async-shell-command-buffer 'confirm-kill-process)
        (shell-command--same-buffer-confirm "Kill it")
        (kill-process process))
       ((eq async-shell-command-buffer 'confirm-new-buffer)
        (shell-command--same-buffer-confirm "Use a new buffer")
        (setq buffer (generate-new-buffer buffer-name)))
       ((eq async-shell-command-buffer 'new-buffer)
        (setq buffer (generate-new-buffer buffer-name)))
       ((eq async-shell-command-buffer 'confirm-rename-buffer)
        (shell-command--same-buffer-confirm "Rename it")
        (with-current-buffer buffer (rename-uniquely))
        (setq buffer (get-buffer-create buffer-name)))
       ((eq async-shell-command-buffer 'rename-buffer)
        (with-current-buffer buffer (rename-uniquely))
        (setq buffer (get-buffer-create buffer-name)))))
    (with-current-buffer buffer
      (shell-command-save-pos-or-erase)
      (setq default-directory directory)
      (require 'shell)
      (let* ((values (copy-tree (plist-get snapshot :buffer-values)))
             (environment-entry (assq 'process-environment values)))
        (if environment-entry
            (setcdr environment-entry
                    (copy-sequence
                     (plist-get snapshot :async-process-environment)))
          (push (cons 'process-environment
                      (copy-sequence
                       (plist-get snapshot :async-process-environment)))
                values))
        (cl-progv (mapcar #'car values) (mapcar #'cdr values)
          (setq process
                (start-process-shell-command "Shell" buffer command))))
      (setq mode-line-process '(":%s"))
      (piem--async-shell-mode)
      (setq-local revert-buffer-function
                  (lambda (&rest _)
                    (piem--start-snapshotted-async-shell-command
                     snapshot command buffer)))
      (set-process-sentinel process #'shell-command-sentinel)
      (set-process-filter process #'comint-output-filter)
      (if async-shell-command-display-buffer
          (display-buffer buffer (piem--async-shell-display-action))
        (let ((nonce (make-symbol "nonce")))
          (add-function
           :before (process-filter process)
           (lambda (proc _string)
             (let ((output (process-buffer proc)))
               (when (buffer-live-p output)
                 (remove-function (process-filter proc) nonce)
                 (display-buffer
                  output (piem--async-shell-display-action t)))))
           `((name . ,nonce))))))))

(defun piem--call-native-shell-command (snapshot command)
  "Run COMMAND through native shell machinery under execution SNAPSHOT."
  (let* ((dynamic-values (plist-get snapshot :dynamic-values))
         (dynamic-symbols (mapcar #'car dynamic-values))
         (dynamic-bindings (mapcar #'cdr dynamic-values)))
    (cl-progv dynamic-symbols dynamic-bindings
      (piem--call-with-shell-buffer-values
       (plist-get snapshot :buffer-values)
       (lambda ()
         (let* ((default-directory (plist-get snapshot :directory))
                (handler
                 (find-file-name-handler
                  (directory-file-name default-directory) 'shell-command)))
           (if (and (not handler)
                    (string-match "[ \t]*&[ \t]*\\'" command))
               (piem--start-snapshotted-async-shell-command
                snapshot (substring command 0 (match-beginning 0)))
             (shell-command command))))))))

(defun piem--call-with-shell-directory (directory function)
  "Call FUNCTION with DIRECTORY as the current shell working directory.
Stay in the current buffer so its shell and process environment apply.  If a
canonical chat session transition occurs during the call, keep that transition's
new `default-directory' after the temporary binding unwinds."
  (let ((session-directory (piem--chat-session-directory)))
    (unwind-protect
        (let ((default-directory directory))
          (funcall function))
      (let ((current-session-directory
             (piem--chat-session-directory)))
        (unless (equal session-directory current-session-directory)
          (setq default-directory current-session-directory))))))

(defun piem-shell-command-at-point ()
  "Read and run a Dired-inspired command on the file target at point.
One command word followed only by whitespace-delimited options beginning with
`-' automatically receives the safely quoted target.  All other command text,
including ordinary arguments, compound/control syntax, and multiple lines,
must place a textual isolated `*' bounded on each side by a space, tab, or
string edge.  A terminal whitespace-delimited ` &' uses asynchronous shell
output.  Prompting and execution use a snapshot of the target session's local
or TRAMP execution environment."
  (interactive)
  (let* ((target (or (piem--file-target-at-point)
                     (user-error "No file at point")))
         ;; Do this before opening the minibuffer: shell-only target failures
         ;; must not consume input, and TARGET must remain a stable snapshot.
         (argument (piem--file-target-shell-argument target))
         (shell-directory (plist-get target :shell-directory))
         (execution-snapshot
          (piem--shell-execution-snapshot shell-directory))
         (command
          (piem--call-with-shell-directory
           shell-directory
           (lambda ()
             (read-shell-command
              (format "! on %s: " (plist-get target :display))))))
         (shell-command-text
          (piem--shell-command-with-file command argument)))
    ;; Reuse the target snapshot even if the chat changed sessions while the
    ;; prompt was active.
    (piem--call-with-shell-directory
     shell-directory
     (lambda ()
       (piem--call-native-shell-command
        execution-snapshot shell-command-text)))))

(defun piem--goto-file-target-location (line column)
  "Move to optional one-based physical LINE and COLUMN in the current file.
A nil LINE preserves the point, mark, and restriction established by native
file visiting.  Explicit locations are computed against the full file, clamp
at physical EOF or EOL, and deactivate any stale region after a successful
move.  COLUMN uses display columns and never inserts text.  If the resulting
position is inaccessible, obey `widen-automatically': widen when non-nil, or
signal `user-error' without changing point or the restriction when nil.  Range
starts are already represented by LINE; range ends and link fragments do not
affect navigation."
  (when line
    (let ((position
           (save-excursion
             (save-restriction
               (widen)
               (goto-char (point-min))
               (forward-line (1- line))
               (when column
                 ;; Emacs 29's `move-to-column' requires a fixnum, while strict
                 ;; target parsing can produce a larger integer.  Either value
                 ;; is past any real line end, so this preserves EOL clamping.
                 (move-to-column (min (1- column) most-positive-fixnum)))
               (point)))))
      (cond
       ((and (<= (point-min) position) (<= position (point-max))))
       (widen-automatically (widen))
       (t (user-error "Position is outside accessible part of buffer")))
      (goto-char position)
      (deactivate-mark))))

(defun piem--validate-file-target-location (source line column)
  "Validate SOURCE's optional one-based LINE and COLUMN before opening.
Tool rows retain their established controlled error for any absent or malformed
mapped LINE.  Other sources may omit a location, but explicit coordinates must
be positive integers and COLUMN requires LINE."
  (cond
   ((eq source :tool)
    (unless (piem--positive-location-p line)
      (user-error "No file line at point")))
   ((and line (not (piem--positive-location-p line)))
    (user-error "File line must be a positive integer")))
  (cond
   ((and column (not (piem--positive-location-p line)))
    (user-error "File column requires a valid line"))
   ((and column (not (piem--positive-location-p column)))
    (user-error "File column must be a positive integer"))))

(defun piem--visit-file-target (target toggle)
  "Visit TARGET, honoring TOGGLE's window inversion.
Validate locations before requesting a native opener.  After opening, apply a
location only when the resulting buffer actually visits a file; native Dired
buffers and other non-file buffers retain the point and mark chosen by Emacs.
A target without a location preserves native behavior."
  (let ((source (plist-get target :source))
        (path (plist-get target :emacs-path))
        (line (plist-get target :line))
        (column (plist-get target :column)))
    (piem--validate-file-target-location source line column)
    (let ((use-other-window
           (if toggle
               (not piem-visit-file-other-window)
             piem-visit-file-other-window)))
      (funcall (if use-other-window #'find-file-other-window #'find-file)
               path))
    ;; Decide applicability from public post-open buffer semantics.  In
    ;; particular, do not preflight local or remote paths to detect directories.
    (when (and line
               (buffer-file-name (current-buffer))
               (not (derived-mode-p 'dired-mode)))
      (piem--goto-file-target-location line column))))

(defun piem--dispatch-button
    (&optional position use-mouse-action strict-return)
  "Dispatch a remapped chat button at POSITION.
On keyboard RET, a Pi tool toggle retains its standard button action.  Every
other button resolves once through the strict file-target visitor: authoritative
tool ownership is preserved, while only a semantically owned local Markdown
link is accepted outside tools.  Invalid, non-local, reference, malformed, and
non-Markdown buttons fail closed.  Other keyboard keys, mouse events, and direct
calls to `push-button' retain standard behavior.  USE-MOUSE-ACTION has the same
meaning as for `push-button'.  STRICT-RETURN is non-nil only for interactive
RET."
  (interactive
   (list (if (integerp last-command-event) (point) last-command-event)
         nil
         (equal (this-single-command-keys) [?\r])))
  (if (not strict-return)
      (push-button position use-mouse-action)
    (if-let* ((button (button-at (or position (point)))))
        (if (button-get button 'piem-tool-toggle)
            (push-button position use-mouse-action)
          (let ((target (or (piem--file-target-at-point)
                            (user-error "No file at point"))))
            (if (memq (plist-get target :source) '(:tool :link))
                (piem--visit-file-target
                 target current-prefix-arg)
              (user-error "No file at point"))))
      (push-button position use-mouse-action))))

(defun piem-visit-file (&optional toggle)
  "Visit one strict file target at point.
Targets may be file-content rows in tool output, plain path references, or
labels of local Markdown links.  Tool headers and other non-content rows are
not visitable.  Plain path locations use a one-based physical file line and
optional one-based column; a line range visits its first line only.  Explicit
locations obey `widen-automatically' in file-visiting buffers and are ignored in
native Dired buffers.  Targets without a location preserve native point,
mark, and narrowing.  By default, `piem-visit-file-other-window'
selects which native opener Pi requests; Emacs display policy controls final
placement.  With prefix argument TOGGLE, invert the opener request."
  (interactive "P")
  (let ((target (or (piem--file-target-at-point)
                    (user-error "No file at point"))))
    (piem--visit-file-target target toggle)))

;;;; Diff Overlay Highlighting

;; Overlay priorities determine stacking order (higher = on top)
;; Tool-block overlay has no priority (defaults to 0)
(defconst piem--diff-line-priority 10
  "Priority for diff line background overlays.
Higher than tool-block (0) so diff colors show through.")

(defconst piem--diff-indicator-priority 20
  "Priority for diff indicator (+/-) overlays.
Higher than line background so indicator face isn't obscured.")

(defun piem--apply-diff-overlays (start end)
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
        (overlay-put ind-ov 'priority piem--diff-indicator-priority)
        (overlay-put ind-ov 'piem-diff-overlay t)
        ;; Line background face - higher than tool-block but lower than indicator.
        ;; Use theme-derived background-only faces so syntax foregrounds stay visible.
        (overlay-put line-ov 'face (if is-added
                                      'piem-diff-line-added
                                    'piem-diff-line-removed))
        (overlay-put line-ov 'priority piem--diff-line-priority)
        (overlay-put line-ov 'piem-diff-overlay t)))))

;;;; Compaction Display

(defun piem--display-compaction-result (tokens-before summary &optional timestamp)
  "Display a compaction result block in the chat buffer.
TOKENS-BEFORE is the token count before compaction.
SUMMARY is the compaction summary text (markdown).
TIMESTAMP is optional time when compaction occurred."
  (let ((start (with-current-buffer (piem--get-chat-buffer) (point-max))))
    (piem--append-to-chat
     (concat "\n" (piem--make-separator "Compaction" timestamp) "\n"
             (propertize (format "Compacted from %s tokens\n\n"
                                 (piem--format-number (or tokens-before 0)))
                         'face 'piem-tool-name)
             (piem--render-safe-string summary) "\n"))
    (with-current-buffer (piem--get-chat-buffer)
      (piem--decorate-tables-unless-deferred start (point-max)))))

(defun piem--display-branch-summary (summary &optional timestamp)
  "Display a branch SUMMARY block in the chat buffer.
TIMESTAMP is the optional time when the branch summary was created."
  (let ((start (with-current-buffer (piem--get-chat-buffer) (point-max))))
    (piem--append-to-chat
     (concat "\n" (piem--make-separator "Branch Summary" timestamp) "\n"
             (piem--render-safe-string summary) "\n"))
    (with-current-buffer (piem--get-chat-buffer)
      (piem--decorate-tables-unless-deferred start (point-max)))))

(defun piem--handle-compaction-success (tokens-before summary &optional timestamp)
  "Handle successful compaction: display result and notify user.
TOKENS-BEFORE is the pre-compaction token count.
SUMMARY is the compaction summary text.
TIMESTAMP is optional time when compaction occurred."
  (piem--display-compaction-result tokens-before summary timestamp)
  (piem--refresh-header)
  (message "Pi: Compacted from %s tokens" (piem--format-number (or tokens-before 0))))

(defun piem--render-complete-message ()
  "Finalize completed message: ensure trailing newline, decorate tables.
Uses message-start-marker and streaming-marker to find content.
No explicit fontification needed — jit-lock + tree-sitter fontify
at each redisplay cycle during streaming, and any remaining gaps
are fontified at the redisplay after this function returns.
Display-only table decoration is applied after the content is stable."
  (when (and piem--message-start-marker piem--streaming-marker)
    (let ((start (marker-position piem--message-start-marker))
          (end (marker-position piem--streaming-marker)))
      (when (< start end)
        (let ((inhibit-read-only t))
          (piem--with-scroll-preservation
            (save-excursion
              (goto-char end)
              (unless (eq (char-before) ?\n)
                (insert "\n")
                (set-marker piem--streaming-marker (point))))))
        (if (piem--chat-buffer-hidden-p)
            (setq piem--table-decoration-pending t)
          (piem--decorate-tables-in-region
           start (marker-position piem--streaming-marker)))))))

;;;; Tool Property Restoration

(defun piem--restore-tool-properties (beg end)
  "Restore tool header faces after tree-sitter fontification in BEG..END.
Tree-sitter markdown applies `invisible' and `face' properties to markup
patterns in tool headers (for example, `$ echo **hello**').  This strips
that markdown damage and restores the intended `font-lock-face' values for
all overlapping tool headers, live or finalized."
  (let ((inhibit-read-only t))
    (dolist (ov (piem--tool-block-overlays-in-region beg end))
      (when-let* ((ov-start (overlay-start ov))
                  (ov-end (overlay-end ov))
                  (header-end-marker (overlay-get ov 'piem-header-end))
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

(defun piem--extract-history-user-message-text (message)
  "Extract visible user text from history MESSAGE.
Supports both string content and text-block vectors.  Returns nil when
MESSAGE has no visible text content."
  (let ((content (plist-get message :content)))
    (cond
     ((stringp content)
      (unless (string-empty-p content) content))
     ((vectorp content)
      (piem--extract-user-message-text content))
     (t nil))))

(defun piem--completed-thinking-rendered-from-normalized
    (normalized &optional block-order display)
  "Return completed thinking NORMALIZED text rendered for DISPLAY.
BLOCK-ORDER identifies the logical completed-thinking block across rerenders.
Returns nil when NORMALIZED has no visible completed-thinking content."
  (unless (string-empty-p normalized)
    (let ((display (or display (piem--thinking-display-mode))))
      (piem--propertize-completed-thinking
       (pcase display
         ('hidden (piem--thinking-hidden-stub normalized))
         (_ (piem--thinking-blockquote-text normalized)))
       (or block-order (piem--next-thinking-block-order))
       normalized
       display))))

(defun piem--completed-thinking-rendered-text
    (text &optional block-order display)
  "Return completed thinking TEXT rendered for DISPLAY.
BLOCK-ORDER identifies the logical completed-thinking block across rerenders.
Returns nil when TEXT normalizes to no visible thinking content."
  (piem--completed-thinking-rendered-from-normalized
   (piem--thinking-normalize-text text)
   block-order
   display))

(defun piem--render-history-thinking (text)
  "Render completed thinking TEXT during session history replay.
Uses the current buffer's completed-thinking display mode."
  (when-let* ((rendered (piem--completed-thinking-rendered-text text)))
    (piem--render-history-text rendered)))

(defun piem--build-tool-result-index (messages)
  "Build hash-table mapping toolCallId to toolResult message from MESSAGES."
  (let ((index (make-hash-table :test 'equal)))
    (when (vectorp messages)
      (dotimes (i (length messages))
        (let ((msg (aref messages i)))
          (when (equal (plist-get msg :role) "toolResult")
            (puthash (plist-get msg :toolCallId) msg index)))))
    index))

(defun piem--history-postprocess-start ()
  "Return the first position that needs eager history post-processing.
Large resumed histories should render the visible tail promptly.  Older content
can rely on normal jit-lock when visited, while display-only table decoration is
kept to the same hot-tail suffix used by resize refreshes."
  (if (markerp piem--hot-tail-start)
      (marker-position piem--hot-tail-start)
    (point-min)))

(defconst piem--history-table-separator-candidate-re
  "^[ \t>]*|?[-:| \t]*---[-:| \t]*|[-:| \t]*$"
  "Regex matching a cheap markdown pipe-table separator candidate.
This avoids invoking tree-sitter for ordinary prose or shell commands that
contain `|' but cannot be pipe tables.")

(defun piem--history-table-candidate-p (start end)
  "Return non-nil when START..END may contain a markdown pipe table."
  (save-excursion
    (goto-char start)
    (re-search-forward
     piem--history-table-separator-candidate-re end t)))

(defun piem--postprocess-history-buffer ()
  "Run consolidated display post-processing after history replay.
History replay inserts many small user/assistant chunks.  Running fontification
and table decoration after each chunk is expensive in large sessions, so replay
defers that work.  Fontification is left to jit-lock on redisplay; the only
synchronous pass decorates candidate tables in the recent hot tail."
  (let ((start (piem--history-postprocess-start))
        (end (point-max)))
    (when (piem--history-table-candidate-p start end)
      ;; Narrowing keeps tree-sitter's initial parse proportional to the hot
      ;; tail instead of the entire resumed transcript.
      (save-restriction
        (narrow-to-region start end)
        (piem--decorate-tables-in-region start end)))))

(defun piem--render-history-text (text)
  "Render TEXT as markdown content with proper isolation.
Ensures markdown structures don't leak to subsequent content.
Display-only table decoration is applied after deferred history insertion."
  (when (and text (not (string-empty-p text)))
    (let ((start (with-current-buffer (piem--get-chat-buffer) (point-max))))
      (piem--append-to-chat text)
      (with-current-buffer (piem--get-chat-buffer)
        ;; History replay should keep rendering even if markdown
        ;; fontification trips over a tree-sitter/runtime mismatch.
        ;; Preserve debugger behavior when `debug-on-error' is non-nil.
        (unless (piem--history-postprocessing-deferred-p)
          (condition-case-unless-debug nil
              (font-lock-ensure start (point-max))
            (error nil))
          (piem--decorate-tables-in-region start (point-max))))
      ;; Two trailing newlines reset any open markdown list/paragraph context
      (piem--append-to-chat "\n\n"))))

(defun piem--render-history-tool (tool-call result)
  "Render a single tool from history: TOOL-CALL block with its RESULT.
TOOL-CALL is a content block plist with :type \"toolCall\", :id, :name,
and :arguments.  RESULT is the matching toolResult message, or nil."
  (let ((tool-name (plist-get tool-call :name))
        (args (plist-get tool-call :arguments)))
    (piem--display-tool-start tool-name args)
    (if result
        (piem--display-tool-end
         tool-name args
         (plist-get result :content)
         (plist-get result :details)
         (plist-get result :isError))
      (piem--tool-overlay-finalize 'piem-tool-block)
      (let ((inhibit-read-only t))
        (save-excursion (goto-char (point-max)) (insert "\n"))))))

(defun piem--render-history-assistant-content (message results)
  "Render assistant MESSAGE content blocks in source order.
RESULTS maps toolCallId strings to matching toolResult messages."
  (let ((content (plist-get message :content))
        (pending-text nil))
    (cl-labels ((flush-text ()
                  (when pending-text
                    (piem--render-history-text
                     (string-join (nreverse pending-text) ""))
                    (setq pending-text nil))))
      (cond
       ((stringp content)
        (unless (string-empty-p content)
          (piem--render-history-text content)))
       ((vectorp content)
        (dolist (block (piem--content-block-list content))
          (let ((block-type (plist-get block :type)))
            (pcase block-type
              ("text"
               (push (piem--render-safe-string
                      (plist-get block :text))
                     pending-text))
              ("thinking"
               (flush-text)
               (piem--render-history-thinking
                (piem--render-safe-string
                 (plist-get block :thinking))))
              ("toolCall"
               (flush-text)
               (piem--render-history-tool
                block (gethash (plist-get block :id) results))))))
        (flush-text))))))

(defun piem--rewrite-tail-window-p
    (window-point window-end point-max point-row body-height)
  "Return non-nil when WINDOW-POINT or WINDOW-END should follow a rewritten tail.
A WINDOW-POINT at or just before POINT-MAX is tail-following.  A WINDOW-END that
merely reaches POINT-MAX counts only when POINT-ROW already sits in the lower
half of BODY-HEIGHT, so tall windows inspecting mid-buffer context do not get
misclassified as tail-following just because they can also see the tail."
  (or (>= window-point (1- point-max))
      (and (>= window-end (1- point-max))
           (< point-row (max 1 body-height))
           (>= point-row (/ (max 1 body-height) 2)))))

(defun piem--clamp-rewrite-point-row (saved-row above-lines tail-lines body-height)
  "Clamp SAVED-ROW after a buffer rewrite.
ABOVE-LINES counts screen lines before point, TAIL-LINES counts screen lines
from point through the tail, and BODY-HEIGHT is the window body height in
screen lines.

When the whole buffer is shorter than the window, preserving a full window is
impossible, so the row falls back to the highest still-visible row.  Otherwise,
clamp the row so the tail still fills the window after the rewrite."
  (let* ((max-row (min (max 0 (1- body-height)) above-lines))
         (total-lines (+ above-lines tail-lines)))
    (if (< total-lines body-height)
        (min saved-row max-row)
      (let ((min-row (max 0 (- body-height tail-lines))))
        (max min-row (min saved-row max-row))))))

(defun piem--live-thinking-start-at-pos (pos)
  "Return active thinking block start when POS is inside live thinking."
  (when (and (markerp piem--thinking-start-marker)
             (markerp piem--thinking-marker)
             (marker-position piem--thinking-start-marker)
             (marker-position piem--thinking-marker))
    (let ((start (marker-position piem--thinking-start-marker))
          (end (marker-position piem--thinking-marker)))
      (when (and (<= start pos) (< pos end))
        start))))

(defun piem--capture-window-rewrite-state (window point-max)
  "Return saved WINDOW state before a buffer rewrite.
POINT-MAX is the old buffer end before the rewrite begins."
  (let* ((point (window-point window))
         (body-height (max 1 (window-body-height window)))
         (row (count-screen-lines (window-start window)
                                  point
                                  nil
                                  window)))
    (list :window window
          :tail-p (piem--rewrite-tail-window-p
                   point
                   (window-end window t)
                   point-max
                   row
                   body-height)
          :start (window-start window)
          :point point
          :thinking-block (piem--thinking-block-at-pos point)
          :live-thinking-start (piem--live-thinking-start-at-pos point)
          :row row)))

(defun piem--capture-window-rewrite-states ()
  "Return saved rewrite states for visible windows showing the current buffer."
  (let ((old-point-max (point-max))
        (buffer (current-buffer)))
    (mapcar (lambda (win)
              (piem--capture-window-rewrite-state win old-point-max))
            (get-buffer-window-list buffer nil t))))

(defun piem--adjust-pos-after-region-replacements
    (pos replacements)
  "Return POS adjusted after REPLACEMENTS, or nil when POS was deleted.
Each entry in REPLACEMENTS is (START END DELTA), using coordinates from before
any replacement was applied."
  (let ((total-delta 0))
    (catch 'deleted
      (dolist (replacement replacements (+ pos total-delta))
        (pcase-let ((`(,start ,end ,delta) replacement))
          (cond
           ((and (<= start pos) (< pos end))
            (throw 'deleted nil))
           ((>= pos end)
            (setq total-delta (+ total-delta delta)))))))))

(defun piem--map-window-rewrite-pos (pos map-position fallback)
  "Map old POS through MAP-POSITION, or return FALLBACK when POS was deleted."
  (if map-position
      (or (funcall map-position pos) fallback)
    (min pos (point-max))))

(defun piem--resolve-window-rewrite-point
    (window-state &optional map-position)
  "Return the best restored point for WINDOW-STATE after a buffer rewrite.
When point was inside a completed or live thinking block, prefer the rewritten
block start.  Otherwise map the saved numeric point through MAP-POSITION when
provided, or clamp it into the rewritten buffer."
  (or (piem--thinking-block-start
       (plist-get window-state :thinking-block))
      (plist-get window-state :live-thinking-start)
      (piem--map-window-rewrite-pos
       (plist-get window-state :point)
       map-position
       (min (plist-get window-state :point) (point-max)))))

(defun piem--window-start-fills-window-p
    (start point-max body-height window)
  "Return non-nil when START still fills WINDOW after a rewrite.
A filled view leaves at most one blank row in BODY-HEIGHT after POINT-MAX.
Preserving the user's viewport is better than scrolling to command point when
the old start remains useful."
  (>= (count-screen-lines start point-max nil window)
      (1- body-height)))

(defun piem--restore-window-rewrite-state
    (window-state &optional map-position)
  "Restore WINDOW-STATE after a large buffer rewrite.
Tail-following windows stay pinned to the rewritten tail.  Other windows
restore point, then recenter to a clamped screen-line row so the window stays
filled when possible instead of showing a mostly blank tail view.
MAP-POSITION, when non-nil, maps old buffer positions to new ones and returns
nil for positions deleted by the rewrite."
  (let ((win (plist-get window-state :window)))
    (when (and (window-live-p win)
               (eq (window-buffer win) (current-buffer)))
      (with-selected-window win
        (let* ((point-max (point-max))
               (point (piem--resolve-window-rewrite-point
                       window-state map-position)))
          (if (plist-get window-state :tail-p)
              (progn
                (goto-char point-max)
                (recenter -1))
            (goto-char point)
            (let* ((body-height (max 1 (window-body-height win)))
                   (saved-start
                    (piem--map-window-rewrite-pos
                     (plist-get window-state :start)
                     map-position
                     point)))
              (if (piem--window-start-fills-window-p
                   saved-start point-max body-height win)
                  (progn
                    (set-window-start win saved-start t)
                    (set-window-point win (max point saved-start)))
                (let* ((above-lines (count-screen-lines (point-min) point nil win))
                       (tail-lines (max 1 (count-screen-lines point point-max nil win)))
                       (row (piem--clamp-rewrite-point-row
                             (plist-get window-state :row)
                             above-lines
                             tail-lines
                             body-height)))
                  (recenter row))))))))))

(defun piem--restore-window-rewrite-states
    (buffer window-states &optional map-position)
  "Restore WINDOW-STATES for BUFFER after a large rewrite.
MAP-POSITION is passed to `piem--restore-window-rewrite-state'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-selected-window
        (dolist (window-state window-states)
          (piem--restore-window-rewrite-state
           window-state map-position))))))

(defun piem--rerender-canonical-history ()
  "Rebuild the current chat buffer from cached canonical messages.
Visible chat windows keep useful context after the rewrite: windows already at
or showing the tail stay at the rebuilt tail, while other windows restore point
and approximately the same screen-line row, clamped so the window stays filled
when possible."
  (let ((messages piem--canonical-messages))
    (when (vectorp messages)
      (piem--with-window-rewrite-preservation
        (piem--display-session-history messages (current-buffer))))))

(defun piem--set-chat-thinking-display (mode)
  "Set completed-thinking display MODE for the current chat buffer.
Completed thinking already shown in the buffer is rewritten in place so the
whole-buffer toggle applies one mode to every finished thinking block without
rebuilding unrelated chat content. Live thinking stays visible while the
assistant is still working, and MODE is used when that block completes."
  (let ((chat-buf (piem--get-chat-buffer)))
    (unless chat-buf
      (user-error "No pi session buffer"))
    (with-current-buffer chat-buf
      (piem--set-thinking-display mode)
      (let ((buffer (current-buffer))
            (saved-windows (piem--capture-window-rewrite-states)))
        (when-let* ((replacements
                     (piem--apply-thinking-display-to-completed-blocks
                      mode)))
          (piem--restore-window-rewrite-states
           buffer
           saved-windows
           (lambda (pos)
             (piem--adjust-pos-after-region-replacements
              pos replacements))))))
    (message "Pi: This chat now %s completed thinking"
             (if (eq mode 'hidden) "hides" "shows"))))

(defun piem--display-history-messages (messages)
  "Display MESSAGES from session history with full tool rendering.
Consecutive assistant messages are grouped under one header.
Tool calls are rendered with headers, output, overlays, and toggles."
  (let ((prev-role nil)
        (results (piem--build-tool-result-index messages)))
    (dotimes (i (length messages))
      (let* ((message (aref messages i))
             (role (plist-get message :role)))
        (pcase role
          ("user"
           (let* ((content (plist-get message :content))
                  (text (piem--extract-history-user-message-text message))
                  (has-images
                   (piem--content-has-image-p content))
                  (timestamp
                   (piem--ms-to-time (plist-get message :timestamp))))
             (when (or text has-images)
               (piem--display-user-message text timestamp content)))
           (setq prev-role "user"))
          ("assistant"
           (when (not (equal prev-role "assistant"))
             (piem--append-to-chat
              (concat "\n" (piem--make-separator "Assistant") "\n")))
           (piem--render-history-assistant-content message results)
           (setq prev-role "assistant"))
          ("custom"
           (when (plist-get message :display)
             (piem--display-custom-message
              (plist-get message :content)))
           (setq prev-role "custom"))
          ("compactionSummary"
           (let* ((summary (plist-get message :summary))
                  (tokens-before (plist-get message :tokensBefore))
                  (timestamp (piem--ms-to-time (plist-get message :timestamp))))
             (piem--display-compaction-result tokens-before summary timestamp))
           (setq prev-role "compactionSummary"))
          ("branchSummary"
           (let* ((summary (plist-get message :summary))
                  (timestamp (piem--ms-to-time (plist-get message :timestamp))))
             (piem--display-branch-summary summary timestamp))
           (setq prev-role "branchSummary"))
          ("toolResult"
           nil))))))

(defun piem--display-session-history (messages &optional chat-buf)
  "Display session history MESSAGES in the chat buffer.
MESSAGES is a vector of message plists from get_messages RPC.
CHAT-BUF is the target buffer; if nil, uses `piem--get-chat-buffer'.
Note: When called from async callbacks, pass CHAT-BUF explicitly."
  (setq chat-buf (or chat-buf (piem--get-chat-buffer)))
  (when (and chat-buf (buffer-live-p chat-buf))
    (with-current-buffer chat-buf
      (piem--set-canonical-messages messages)
      (let ((inhibit-read-only t)
            ;; A full resume/reload rebuild allocates many short strings,
            ;; overlays, and display properties.  Keep GC out of the hot path;
            ;; Emacs will collect normally after the dynamic binding unwinds.
            (gc-cons-threshold
             (max gc-cons-threshold
                  piem--history-replay-gc-threshold)))
        (piem--clear-render-artifacts)
        (erase-buffer)
        (insert (piem--format-startup-header) "\n")
        (when (vectorp messages)
          (let ((piem--defer-history-postprocessing t))
            (piem--display-history-messages messages)))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (piem--set-message-start-marker nil)
        (piem--set-streaming-marker nil)
        (piem--update-hot-tail-boundary)
        (piem--cool-completed-tool-blocks-outside-hot-tail)
        (piem--postprocess-history-buffer)
        (goto-char (point-max))))))

(provide 'piem-render)

;;; piem-render.el ends here
