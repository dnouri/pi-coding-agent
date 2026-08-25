;;; pi-coding-agent-jsonl.el --- JSONL session reading and tree projection -*- lexical-binding: t; -*-

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

;; Pure functions over pi session JSONL files: reading entries, building
;; the raw nested session tree, projecting that tree to the flat display
;; dialect the tree browser consumes, formatting tool-call previews, and
;; discovering session files on disk (sessions root, munged per-cwd
;; directories, cheap metadata scans).  Ports pi's session-manager
;; getTree (plus label folding), the RPC tree projection, core
;; format-tool-call, and config/session-dir path munging.  Depends only
;; on core; nothing here touches buffers, processes, or state.
;;
;; Normalization conventions (JSON in, plists out):
;;
;; - JSON null decodes to the :null keyword (`json-parse-string') and is
;;   treated exactly like an absent value: nullable fields go through
;;   `pi-coding-agent--jsonl-entry-parent-id' or core's
;;   `pi-coding-agent--normalize-string-or-null', and this module never
;;   emits :null itself.
;; - JSON arrays are vectors, never lists.
;; - Numbers pass through with their zero-ness intact: presence checks
;;   use `numberp' (never truthiness), so an offset of 0 or a limit of 0
;;   still counts as present.  For read's offset and limit the reference
;;   compares against undefined rather than truthiness, so JSON null
;;   counts as present there too and coerces like JS null
;;   (`pi-coding-agent--jsonl-arg-number').
;; - Key orders are canonical.  Raw nodes are (:entry :children :label
;;   :labelTimestamp) with the label pair only when a label is set.
;;   Projected nodes start with the base (:id :parentId :timestamp
;;   :label :children) followed by type-specific payload keys;
;;   :parentId and assistant :stopReason are always present (possibly
;;   nil), every other optional key only when non-nil.  Children are
;;   always vectors.
;;
;; Deliberate deviations from the TypeScript reference, documented
;; rather than fixed:
;;
;; - Malformed or blank JSONL lines are skipped silently while reading a
;;   session file.
;; - Children sort by timestamp STRING comparison while the reference
;;   parses dates numerically; equivalent for the uniform UTC ISO-8601
;;   stamps pi writes, and both keep file order on ties.
;; - Session files older than version 3 are read as-is, without pi's
;;   on-load migrations: hookMessage roles project as unknown and
;;   version 1 files (no ids or parent ids) build as flat root lists.
;; - Null or malformed message payloads (JSON null :message, null
;;   content, null content blocks) degrade to empty previews instead
;;   of throwing like the reference would; session files are parsed
;;   without validation and old or hand-edited files can carry them.
;; - Entries with duplicate ids (pi generates collision-checked unique
;;   ids, so only hand-edited files hit this) keep tree building total:
;;   each entry expands at most once, rather than reproducing the
;;   reference's equally degenerate output (which loops forever when a
;;   duplicate id also closes a cycle).
;; - `pi-coding-agent--jsonl-shorten-path' replaces the HOME (or
;;   USERPROFILE) prefix blindly: /home/tes also matches /home/tester/x.
;;   Faithful port of the upstream quirk.
;; - The reference truncates strings by UTF-16 code units; Elisp
;;   truncates by characters, so previews of astral-plane text can
;;   differ in length.  Accepted; session text is not surrogate-paired
;;   in practice.
;; - `pi-coding-agent-jsonl-read-session-info' reports :modified from
;;   the file mtime instead of the newest message timestamp (one stat
;;   versus a per-line compare).  Append-only files agree, and the
;;   Phase 4 navigation rewrites arguably make mtime more correct.
;;
;; All tree traversals are iterative (explicit stacks, reversed
;; pre-order bottom-up builds); real session trees reach thousands of
;; entries deep.  The single recursion is
;; `pi-coding-agent--jsonl-encode-args', which normalizes nested JSON
;; tool arguments (two or three levels by construction, never session
;; depth).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'pi-coding-agent-core)

;;;; Entry Normalization

(defun pi-coding-agent--jsonl-entry-parent-id (entry)
  "Return the normalized parent id of ENTRY: a string, or nil.
JSON null, absent, and non-string values all read as nil."
  (pi-coding-agent--normalize-string-or-null (plist-get entry :parentId)))

(defun pi-coding-agent--jsonl-filtered-entry-p (type)
  "Return non-nil when an entry of TYPE is filtered from projection.
Label, session_info, and custom entries are bookkeeping: they vanish and
their children are promoted to the nearest visible ancestor."
  (member type '("label" "session_info" "custom")))

;;;; Reading Session Files

(defun pi-coding-agent-jsonl-read-file (path)
  "Read the session file at PATH.
Return a plist with :path, :header, :entries, :leafId, and :name, or
nil when PATH is missing, empty, or has no parseable \"session\" header
line.
:entries is a vector of every successfully parsed non-header line in
file order; :leafId is the :id of the last entry, whatever its type,
mirroring pi's index build; :name is the latest session_info name,
trimmed, nil when absent or blank, mirroring pi's session readers.
Malformed and blank lines are skipped silently."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (let ((header nil)
            (entries nil)
            (name nil))
        (while (not (eobp))
          (let ((data (pi-coding-agent--parse-json-line
                       (buffer-substring-no-properties
                        (point) (line-end-position)))))
            (when (consp data)
              (let ((type (plist-get data :type)))
                (if (equal type "session")
                    (unless header (setq header data))
                  (push data entries)
                  (when (equal type "session_info")
                    (let ((raw (pi-coding-agent--normalize-string-or-null
                                (plist-get data :name))))
                      (setq name
                            (when raw
                              (let ((trimmed (string-trim raw)))
                                (unless (string-empty-p trimmed)
                                  trimmed))))))))))
          (forward-line 1))
        (when header
          (let* ((vector (vconcat (nreverse entries)))
                 (count (length vector)))
            (list :path path
                  :header header
                  :entries vector
                  :leafId (when (> count 0)
                            (plist-get (aref vector (1- count)) :id))
                  :name name)))))))

;;;; Session Discovery

(defconst pi-coding-agent--jsonl-line-type-re
  "[ \t]*{[ \t]*\"type\"[ \t]*:[ \t]*\"%s\""
  "Format string matching a JSONL line whose top-level type appears first.
Pi writes session JSONL with `type' as the first key, so matching that
cheap prefix routes lines without parsing their full payloads.  Local
sibling of menu.el's regexp; kept separate so this module depends only
on core (the duplication is consolidated in Phase 5).")

(defun pi-coding-agent--jsonl-line-type-p (type)
  "Return non-nil when the current line has top-level session TYPE."
  (looking-at-p (format pi-coding-agent--jsonl-line-type-re
                        (regexp-quote type))))

(defun pi-coding-agent--jsonl-parse-current-line ()
  "Return the current line parsed as a plist, or nil when malformed."
  (pi-coding-agent--parse-json-line
   (buffer-substring-no-properties (point) (line-end-position))))

(defun pi-coding-agent-jsonl-sessions-root (&optional anchor)
  "Return pi's sessions root directory as a directory name (trailing slash).
The default is PI_CODING_AGENT_DIR, else ~/.pi/agent, plus
\"sessions\" (a port of pi's getAgentDir), expanded.
ANCHOR, when remote (see `file-remote-p'), roots the scan on that
remote instead: the parent of ANCHOR's own directory — pass a session
FILE, or a directory with a trailing slash.  A local ANCHOR is ignored:
the expanded default always applies."
  (if (and anchor (file-remote-p anchor))
      (file-name-directory
       (directory-file-name (file-name-directory anchor)))
    (concat (expand-file-name (or (getenv "PI_CODING_AGENT_DIR")
                                  "~/.pi/agent"))
            "/sessions/")))

(defun pi-coding-agent-jsonl-session-dir-for-cwd (cwd &optional root)
  "Return the munged session directory name for CWD under ROOT.
A pure string port of pi's getDefaultSessionDirPath: clean CWD's name
\(trailing slashes collapse), strip ONE leading / or backslash, then
replace every /, backslash, and : with a dash, wrapped as --…--.
ROOT defaults to `pi-coding-agent-jsonl-sessions-root'.  The result
carries no trailing slash; Windows drives munge like \"C:\\x\" to
--C--x--, exactly like pi's character class."
  (let* ((base (directory-file-name
                (or root (pi-coding-agent-jsonl-sessions-root))))
         (clean (if (string-empty-p cwd) "" (directory-file-name cwd)))
         (stripped
          (if (and (not (string-empty-p clean))
                   (memq (aref clean 0) '(?/ ?\\)))
              (substring clean 1)
            clean))
         (munged (replace-regexp-in-string "[/\\:]" "-" stripped)))
    (concat (file-name-as-directory base) "--" munged "--")))

(defun pi-coding-agent--jsonl-scan-session-info (path mtime)
  "Scan the current buffer for session metadata.
PATH and MTIME feed the :path and :modified keys; see
`pi-coding-agent-jsonl-read-session-info' for the full contract.
Return the session plist, or nil when no header line was found."
  (goto-char (point-min))
  (let ((header nil)
        (name nil)
        (message-count 0)
        (first-message nil)
        (fallback-message nil)
        (parsed-messages 0))
    (catch 'invalid
      (while (not (eobp))
        (cond
         ((and (null header)
               (pi-coding-agent--jsonl-line-type-p "session"))
          (setq header (pi-coding-agent--jsonl-parse-current-line)))
         ((null header)
          ;; Leading blank lines are tolerable noise; any other
          ;; non-session line before the header means this is not a
          ;; session file.
          (unless (looking-at-p "[ \t]*\\'")
            (throw 'invalid nil)))
         ((pi-coding-agent--jsonl-line-type-p "message")
          (setq message-count (1+ message-count))
          (when (and (null first-message) (< parsed-messages 5))
            (setq parsed-messages (1+ parsed-messages))
            (let ((data (pi-coding-agent--jsonl-parse-current-line)))
              (when (consp data)
                (let* ((message (plist-get data :message))
                       (text (pi-coding-agent--jsonl-extract-text
                              (plist-get message :content))))
                  (cond
                   ((and (equal (plist-get message :role) "user")
                         (not (string-empty-p text)))
                    (setq first-message text))
                   ((null fallback-message)
                    (setq fallback-message text))))))))
         ((pi-coding-agent--jsonl-line-type-p "session_info")
          (let* ((data (pi-coding-agent--jsonl-parse-current-line))
                 (raw (when (consp data)
                        (pi-coding-agent--normalize-string-or-null
                         (plist-get data :name)))))
            ;; Latest-wins replay; absent or blank names clear the key.
            (setq name
                  (when raw
                    (let ((trimmed (string-trim raw)))
                      (unless (string-empty-p trimmed) trimmed))))))
         ;; Later headers, blanks, label/custom/unknown lines: skip
         ;; without parsing.
         (t nil))
        (forward-line 1)))
    (when header
      (let ((id (pi-coding-agent--normalize-string-or-null
                 (plist-get header :id)))
            (cwd (pi-coding-agent--normalize-string-or-null
                  (plist-get header :cwd)))
            (parent (pi-coding-agent--normalize-string-or-null
                     (plist-get header :parentSession)))
            (created (pi-coding-agent--normalize-string-or-null
                      (plist-get header :timestamp)))
            (first (or first-message
                       (and (not (string-empty-p
                                  (or fallback-message "")))
                            fallback-message))))
        (append (list :path path)
                (when id (list :id id))
                (when cwd (list :cwd cwd))
                (when name (list :name name))
                (when parent (list :parentSessionPath parent))
                (when created (list :created created))
                (list :modified
                      (format-time-string "%Y-%m-%dT%H:%M:%SZ" mtime t)
                      :messageCount message-count)
                (when first (list :firstMessage first)))))))

(defun pi-coding-agent-jsonl-read-session-info (path)
  "Read session metadata for the file at PATH, without building trees.
Return a plist in the browse session dialect — (:path :id :cwd :name?
:parentSessionPath? :created? :modified :messageCount :firstMessage?)
— or nil when PATH is unreadable, empty, lacks a leading \"session\"
header line, carries a non-session line before the header, or cannot
be read at all.  Key parity with the session browser is the contract.

The scan is regex-first, mirroring menu.el's
`pi-coding-agent--session-metadata' (duplication is deliberate until
Phase 5): lines route by their top-level type prefix and only
headers, session_info lines, and the first few message lines are
full-parsed.  :messageCount counts message lines by regex alone
\(toolResult included).  :firstMessage full-parses at most 5 message
lines while unset: a user message with extractable text wins,
otherwise the first parsed message of any role is the fallback.
:name replays session_info lines in file order with latest-wins
trimming.  label and custom entries are ignored.  :created is the
header timestamp; :modified is the file mtime as a second-resolution
UTC ISO string (see the deviation note in the Commentary)."
  (condition-case nil
      (when (file-readable-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (pi-coding-agent--jsonl-scan-session-info
           path
           (file-attribute-modification-time
            (file-attributes path)))))
    (error nil)))

;;;; Building Raw Trees

(defun pi-coding-agent--jsonl-entry-< (a b)
  "Return non-nil when the timestamp of entry A precedes entry B's.
Absent timestamps read as the empty string so `string<' never sees nil."
  (string< (or (plist-get a :timestamp) "")
           (or (plist-get b :timestamp) "")))

(defun pi-coding-agent-jsonl-build-tree (entries)
  "Build the raw nested session tree from the flat ENTRIES vector.
Return a plist with :tree and :leafId: TREE is a vector of root nodes
shaped (:entry E :children VECTOR :label S :labelTimestamp S),
exactly the shape of pi's get_tree, and LEAF-ID is the :id of the last
entry in file order, whatever its type.

Roots are entries whose parent is nil, themselves, or unknown, in file
order; children are sorted by timestamp with ties keeping file order.
Label entries replay in file order with latest-wins folding onto their
targetId, an empty label clearing like an absent one; cleared labels
omit both label keys.  Entries unreachable from any root (cycles) are
dropped.  Traversal is iterative."
  (let* ((count (length entries))
         (leaf-id (when (> count 0)
                    (plist-get (aref entries (1- count)) :id)))
         ;; Pass A: index every entry by id, fold labels in file order.
         (id-hash (make-hash-table :test #'equal))
         (labels (make-hash-table :test #'equal))
         (label-timestamps (make-hash-table :test #'equal))
         ;; Pass B: link children to parents (lists pushed in file order).
         (kids (make-hash-table :test #'equal))
         (roots nil))
    (dotimes (i count)
      (let ((entry (aref entries i)))
        (puthash (plist-get entry :id) entry id-hash)
        (when (equal (plist-get entry :type) "label")
          (let ((target (pi-coding-agent--normalize-string-or-null
                         (plist-get entry :targetId)))
                (label (let ((raw (pi-coding-agent--normalize-string-or-null
                                  (plist-get entry :label))))
                         ;; JS truthiness: an empty-string label clears.
                         (and raw (not (string-empty-p raw)) raw))))
            (if label
                (progn
                  (puthash target label labels)
                  (puthash target (plist-get entry :timestamp)
                           label-timestamps))
              (remhash target labels)
              (remhash target label-timestamps))))))
    (dotimes (i count)
      (let* ((entry (aref entries i))
             (id (plist-get entry :id))
             (parent (pi-coding-agent--jsonl-entry-parent-id entry)))
        (if (or (null parent)
                (equal parent id)
                (not (gethash parent id-hash)))
            (push entry roots)
          (puthash parent (cons entry (gethash parent kids)) kids))))
    (setq roots (nreverse roots))
    ;; Pass C: iterative pre-order collection, then build bottom-up by
    ;; walking the reversed pre-order so every child exists before its
    ;; parent needs it.  Each entry expands at most once: without the
    ;; guard, duplicate ids closing a cycle would expand forever.
    (let ((order nil)
          (stack roots)
          (expanded (make-hash-table :test #'eq))
          (child-nodes (make-hash-table :test #'equal))
          (built-roots nil))
      (while stack
        (let ((entry (pop stack)))
          (unless (gethash entry expanded)
            (puthash entry t expanded)
            (let ((id (plist-get entry :id)))
              (push entry order)
              (setq stack
                    (append (sort (nreverse (gethash id kids))
                                  #'pi-coding-agent--jsonl-entry-<)
                            stack))))))
      ;; ORDER now holds the reversed pre-order; iterating it visits
      ;; descendants before parents and later siblings before earlier
      ;; ones, so plain pushes land in natural order.
      (dolist (entry order)
        (let* ((id (plist-get entry :id))
               (label (gethash id labels))
               (node (if label
                         (list :entry entry
                               :children (vconcat (gethash id child-nodes))
                               :label label
                               :labelTimestamp (gethash id label-timestamps))
                       (list :entry entry
                             :children (vconcat (gethash id child-nodes)))))
               (parent (pi-coding-agent--jsonl-entry-parent-id entry)))
          (if (or (null parent)
                  (equal parent id)
                  (not (gethash parent id-hash)))
              (push node built-roots)
            (puthash parent (cons node (gethash parent child-nodes))
                     child-nodes))))
      (list :tree (vconcat built-roots)
            :leafId leaf-id))))

;;;; Text Extraction and Previews

(defun pi-coding-agent--jsonl-extract-text (content &optional max-len)
  "Extract preview text from CONTENT, an AgentMessage content value.
Strings pass through (optionally sliced to MAX-LEN); vectors contribute
the text of blocks whose :type is \"text\" and whose :text is a string,
joined with spaces and then sliced; anything else is the empty string.
MAX-LEN nil means unlimited, MAX-LEN 0 or less means empty."
  (let ((slice (lambda (text)
                 (if (and max-len (< max-len (length text)))
                     (substring text 0 max-len)
                   text))))
    (cond
     ((and max-len (<= max-len 0)) "")
     ((stringp content) (funcall slice content))
     ((vectorp content)
      (let (blocks)
        (dotimes (i (length content))
          (let ((block (aref content i)))
            (when (and (equal (plist-get block :type) "text")
                       (stringp (plist-get block :text)))
              (push (plist-get block :text) blocks))))
        (funcall slice (mapconcat #'identity (nreverse blocks) " "))))
     (t ""))))

(defun pi-coding-agent--jsonl-normalize-preview (text)
  "Flatten TEXT for single-line previews.
Newlines and tabs (but not carriage returns) become spaces and the
result is trimmed."
  (string-trim (replace-regexp-in-string "[\n\t]" " " text)))

;;;; Tool-Call Formatting

(defun pi-coding-agent--jsonl-arg-number (args key)
  "Return the numeric value of ARGS KEY, or nil when not usable.
A number comes back as-is with its zero-ness intact; JSON null comes
back as the :null keyword, which the reference's `!== undefined'
checks treat as present (read offset/limit); absent keys, JSON false,
and non-numbers read as nil."
  (let ((value (plist-get args key)))
    (cond
     ((numberp value) value)
     ;; JSON null is present-but-null for !== undefined checks.
     ((eq value :null) value)
     (t nil))))

(defun pi-coding-agent--jsonl-arg-string (args key)
  "Return the string value of ARGS KEY under JavaScript truthiness.
JSON null, JSON false, zero, and the empty string fall through to nil
so that an `||' chain can skip them; other values are stringified
like `String()'."
  (let ((value (plist-get args key)))
    (cond
     ((memq value '(nil :null :false)) nil)
     ((eq value :true) "true")
     ((stringp value) (unless (string-empty-p value) value))
     ((numberp value) (unless (zerop value) (number-to-string value)))
     (t (format "%s" value)))))

(defun pi-coding-agent--jsonl-shorten-path (path)
  "Replace PATH's home-directory prefix with ~.
Uses HOME, falling back to USERPROFILE.  The prefix match is blind, a
documented port of the upstream quirk: /home/tes matches
/home/tester/x just as well as /home/tester/x matches itself."
  (let ((home (or (getenv "HOME") (getenv "USERPROFILE") "")))
    (if (and (not (string-empty-p home))
             (string-prefix-p home path))
        (concat "~" (substring path (length home)))
      path)))

(defun pi-coding-agent--jsonl-encode-args (args)
  "Normalize parsed-JSON ARGS for `json-encode'.
Map the :true, :false, and :null keywords (how tests spell JSON
booleans and null) to the values `json-encode' expects, recursively."
  (cond
   ((eq args :true) t)
   ((eq args :false) :json-false)
   ((eq args :null) nil)
   ((vectorp args)
    (vconcat (mapcar #'pi-coding-agent--jsonl-encode-args args)))
   ((consp args)
    (let (out)
      (while (consp args)
        (let ((key (pop args)))
          (push key out)
          (push (if (consp args)
                    (pi-coding-agent--jsonl-encode-args (pop args))
                  nil)
                out)))
      (nreverse out)))
   (t args)))

(defun pi-coding-agent-jsonl-format-tool-call (name args)
  "Return the bracket preview for the tool NAME called with ARGS.
Port of pi's format-tool-call: [read: ~/f.py:10-29] and friends.
ARGS is a plist of parsed-JSON tool arguments (nil allowed)."
  (pcase name
    ("read"
     (let* ((path (pi-coding-agent--jsonl-shorten-path
                   (or (pi-coding-agent--jsonl-arg-string args :path)
                       (pi-coding-agent--jsonl-arg-string args :file_path)
                       "")))
            (offset (pi-coding-agent--jsonl-arg-number args :offset))
            (limit (pi-coding-agent--jsonl-arg-number args :limit)))
       (if (null (or offset limit))
           (format "[read: %s]" path)
         (let ((start (if (numberp offset) offset 1))
               (end (cond
                     ((numberp limit) (+ (if (numberp offset) offset 1)
                                         limit -1))
                     ;; JS null limit coerces to 0: end = start - 1.
                     (limit (+ (if (numberp offset) offset 1) -1))
                     ;; Limit absent: no -end suffix.
                     (t nil))))
           (format "[read: %s:%s%s]" path start
                   (if (and end (/= end 0)) (format "-%d" end) ""))))))
    ((or "write" "edit")
     (let ((path (pi-coding-agent--jsonl-shorten-path
                  (or (pi-coding-agent--jsonl-arg-string args :path)
                      (pi-coding-agent--jsonl-arg-string args :file_path)
                      ""))))
       (format "[%s: %s]" name path)))
    ("bash"
     (let* ((command (or (pi-coding-agent--jsonl-arg-string args :command) ""))
            (normalized (pi-coding-agent--jsonl-normalize-preview command))
            (truncated (substring normalized 0 (min 50 (length normalized)))))
       (format "[bash: %s%s]" truncated
               (if (> (length normalized) 50) "..." ""))))
    ("grep"
     (format "[grep: /%s/ in %s]"
             (or (pi-coding-agent--jsonl-arg-string args :pattern) "")
             (pi-coding-agent--jsonl-shorten-path
              (or (pi-coding-agent--jsonl-arg-string args :path) "."))))
    ("find"
     (format "[find: %s in %s]"
             (or (pi-coding-agent--jsonl-arg-string args :pattern) "")
             (pi-coding-agent--jsonl-shorten-path
              (or (pi-coding-agent--jsonl-arg-string args :path) "."))))
    ("ls"
     (format "[ls: %s]"
             (pi-coding-agent--jsonl-shorten-path
              (or (pi-coding-agent--jsonl-arg-string args :path) "."))))
    (_
     (let* ((json (if args
                       (json-encode (pi-coding-agent--jsonl-encode-args args))
                     "{}"))
            (truncated (substring json 0 (min 40 (length json)))))
       (format "[%s: %s%s]" name truncated
               (if (> (length json) 40) "..." ""))))))

;;;; Projection

(defun pi-coding-agent--jsonl-assistant-tool-calls (message)
  "Return the tool-call records of assistant MESSAGE as (ID NAME ARGS).
Only content blocks whose :type is \"toolCall\" with string :id and
:name count; non-object :arguments normalize to the empty plist."
  (let ((content (plist-get message :content))
        (calls nil))
    (when (vectorp content)
      (dotimes (i (length content))
        (let ((block (aref content i)))
          (when (equal (plist-get block :type) "toolCall")
            (let ((id (plist-get block :id))
                  (name (plist-get block :name))
                  (args (plist-get block :arguments)))
              (when (and (stringp id) (stringp name))
                (push (list id name (if (consp args) args '()))
                      calls)))))))
    (nreverse calls)))

(defun pi-coding-agent--jsonl-build-tool-call-map (roots)
  "Return the global toolCallId map for ROOTS: id to (NAME ARGS).
Scans assistant messages with an iterative pre-order walk; when both
branches of a fork reuse an id, the later visit wins."
  (let ((map (make-hash-table :test #'equal))
        (stack (append roots nil)))
    (while stack
      (let* ((node (pop stack))
             (entry (plist-get node :entry)))
        (when (and (equal (plist-get entry :type) "message")
                   (equal (plist-get (plist-get entry :message) :role)
                          "assistant"))
          (dolist (call (pi-coding-agent--jsonl-assistant-tool-calls
                         (plist-get entry :message)))
            (puthash (nth 0 call) (list (nth 1 call) (nth 2 call)) map)))
        (setq stack (append (plist-get node :children) stack))))
    map))

(defun pi-coding-agent--jsonl-project-assistant (base message)
  "Return the projected assistant node for BASE and MESSAGE.
Preview precedence: extracted text, then aborted stop reason, then
errorMessage, then the \"(no content)\" sentinel that the browser filter
hides.  :stopReason is always present; :errorMessage only when set."
  (let* ((text (pi-coding-agent--jsonl-extract-text
                (plist-get message :content) 200))
         (stop-reason (pi-coding-agent--normalize-string-or-null
                       (plist-get message :stopReason)))
         (error-message (pi-coding-agent--normalize-string-or-null
                         (plist-get message :errorMessage)))
         (preview (cond
                   ((and (stringp text) (> (length text) 0))
                    (pi-coding-agent--jsonl-normalize-preview text))
                   ((equal stop-reason "aborted") "(aborted)")
                   ((and error-message (> (length error-message) 0))
                    (pi-coding-agent--jsonl-normalize-preview error-message))
                   (t "(no content)"))))
    (append base
            (list :type "message" :role "assistant"
                  :preview preview :stopReason stop-reason)
            (when error-message (list :errorMessage error-message)))))

(defun pi-coding-agent--jsonl-project-tool-result (base message branch-calls global-calls)
  "Return the projected tool-result node for BASE and MESSAGE.
Resolve MESSAGE's toolCallId against BRANCH-CALLS (the map seen along
the branch path) then GLOBAL-CALLS.  Resolved calls carry :toolName,
:toolArgs, and :formattedToolCall; unresolved ones fall back to the
message's own tool name and a \"[name]\" preview."
  (let* ((call-id (pi-coding-agent--normalize-string-or-null
                   (plist-get message :toolCallId)))
         (info (when call-id
                 (or (gethash call-id branch-calls)
                     (gethash call-id global-calls))))
         (message-name (pi-coding-agent--normalize-string-or-null
                        (plist-get message :toolName)))
         (formatted (when info
                      (pi-coding-agent-jsonl-format-tool-call
                       (nth 0 info) (nth 1 info))))
         (name (if info (nth 0 info) message-name))
         (preview (if formatted
                      (pi-coding-agent--jsonl-normalize-preview formatted)
                    (format "[%s]" (if (stringp name) name "tool")))))
    (append base
            (list :type "tool_result")
            (when name (list :toolName name))
            (when info (list :toolArgs (nth 1 info)))
            (when formatted (list :formattedToolCall formatted))
            (list :preview preview))))

(defun pi-coding-agent--jsonl-project-message (base message branch-calls global-calls)
  "Return the projected node for BASE and the AgentMessage MESSAGE.
BRANCH-CALLS and GLOBAL-CALLS feed tool-result resolution."
  (let ((role (pi-coding-agent--normalize-string-or-null
               (plist-get message :role))))
    (cond
     ((equal role "toolResult")
      (pi-coding-agent--jsonl-project-tool-result
       base message branch-calls global-calls))
     ((equal role "bashExecution")
      (append base
              (list :type "message" :role "bashExecution"
                    :preview (pi-coding-agent-jsonl-format-tool-call
                              "bash"
                              (list :command (plist-get message :command))))))
     ((equal role "assistant")
      (pi-coding-agent--jsonl-project-assistant base message))
     (t
      (let ((preview (pi-coding-agent--jsonl-normalize-preview
                      (pi-coding-agent--jsonl-extract-text
                       (plist-get message :content) 200))))
        (append base
                (list :type "message")
                (pcase role
                  ((or "user" "custom" "branchSummary" "compactionSummary")
                   (list :role role))
                  (_ (append (list :role "unknown")
                             (when role (list :rawRole role)))))
                (list :preview preview)))))))

(defun pi-coding-agent--jsonl-project-entry (base entry)
  "Return the projected node for BASE and the non-message ENTRY."
  (pcase (plist-get entry :type)
    ("compaction"
     (let ((tokens (plist-get entry :tokensBefore)))
       (append base
               (list :type "compaction")
               (when (numberp tokens)
                 (list :tokensBefore tokens)))))
    ("model_change"
     (let ((provider (pi-coding-agent--normalize-string-or-null
                      (plist-get entry :provider)))
           (model-id (pi-coding-agent--normalize-string-or-null
                      (plist-get entry :modelId))))
       (append base
               (list :type "model_change")
               (when provider (list :provider provider))
               (when model-id (list :modelId model-id)))))
    ("thinking_level_change"
     (let ((level (pi-coding-agent--normalize-string-or-null
                   (plist-get entry :thinkingLevel))))
       (append base
               (list :type "thinking_level_change")
               (when level (list :thinkingLevel level)))))
    ("branch_summary"
     (let ((summary (pi-coding-agent--normalize-string-or-null
                     (plist-get entry :summary))))
       (append base
               (list :type "branch_summary")
               (when summary (list :summary summary)))))
    ("custom_message"
     (let ((custom-type (pi-coding-agent--normalize-string-or-null
                         (plist-get entry :customType))))
       (append base
               (list :type "custom_message")
               (when custom-type (list :customType custom-type))
               (list :preview
                     (pi-coding-agent--jsonl-normalize-preview
                      (pi-coding-agent--jsonl-extract-text
                       (plist-get entry :content) 200))))))
    ;; Unknown future entry types keep their type with no payload.
    (_ (append base (list :type (plist-get entry :type))))))

(defun pi-coding-agent--jsonl-resolve-projected-leaf-id (roots leaf-id)
  "Resolve raw LEAF-ID to the nearest visible entry id under ROOTS.
Walks the raw parent chain (over all nodes, filtered ones included)
until a non-filtered entry appears.  Nil or unknown ids resolve to nil."
  (when leaf-id
    (let ((parent-by-id (make-hash-table :test #'equal))
          (visible-ids (make-hash-table :test #'equal))
          (stack (append roots nil))
          (found nil))
      (while stack
        (let* ((node (pop stack))
               (entry (plist-get node :entry))
               (id (plist-get entry :id)))
          (puthash id (pi-coding-agent--jsonl-entry-parent-id entry)
                   parent-by-id)
          (unless (pi-coding-agent--jsonl-filtered-entry-p
                   (plist-get entry :type))
            (puthash id t visible-ids))
          (setq stack (append (plist-get node :children) stack))))
      (let ((current leaf-id))
        (while (and current (not found))
          (if (gethash current visible-ids)
              (setq found current)
            (setq current (gethash current parent-by-id))))
        found))))

(defun pi-coding-agent-jsonl-project-tree (tree &optional leaf-id)
  "Project the raw session TREE to the flat display dialect.
TREE is a vector of raw nodes (:entry :children :label
:labelTimestamp), the output of `pi-coding-agent-jsonl-build-tree' or
pi's get_tree.  Return (:tree :leafId): bookkeeping entries (label,
session_info, custom) are dropped with their children promoted to the
nearest visible ancestor, toolResult messages resolve their toolCallId
branch-locally first, and :parentId points at the nearest visible
ancestor.  LEAF-ID, when non-nil, is a raw leaf id resolved up to the
nearest visible entry.  Traversal is iterative."
  (let* ((global-calls (pi-coding-agent--jsonl-build-tool-call-map tree))
         (empty-map (make-hash-table :test #'equal))
         ;; Work items: (node parent-visible-node branch-calls).
         (stack nil)
         ;; RECORDS ends up holding the reversed pre-order.
         (records nil)
         (child-nodes (make-hash-table :test #'eq))
         (built-roots nil))
    (dolist (node (nreverse (append tree nil)))
      (push (list node nil empty-map) stack))
    (while stack
      (pcase-let ((`(,node ,parent-visible ,branch-calls) (pop stack)))
        (let ((entry (plist-get node :entry)))
          (if (pi-coding-agent--jsonl-filtered-entry-p
               (plist-get entry :type))
              ;; Promote children to the same target, parent, and map.
              (dolist (child (nreverse (append (plist-get node :children)
                                               nil)))
                (push (list child parent-visible branch-calls) stack))
            (let ((child-map branch-calls))
              (when (and (equal (plist-get entry :type) "message")
                         (equal (plist-get (plist-get entry :message) :role)
                                "assistant"))
                (let ((calls (pi-coding-agent--jsonl-assistant-tool-calls
                              (plist-get entry :message))))
                  (when calls
                    (setq child-map (copy-hash-table branch-calls))
                    (dolist (call calls)
                      (puthash (nth 0 call)
                               (list (nth 1 call) (nth 2 call))
                               child-map)))))
              ;; The node itself resolves against its incoming map.
              (push (list node parent-visible branch-calls) records)
              (dolist (child (nreverse (append (plist-get node :children)
                                               nil)))
                (push (list child node child-map) stack)))))))
    ;; Build bottom-up: iterating RECORDS visits descendants before
    ;; parents and later siblings before earlier ones, so plain pushes
    ;; land children in natural order.
    (dolist (record records)
      (pcase-let ((`(,node ,parent-visible ,branch-calls) record))
        (let* ((entry (plist-get node :entry))
               (label (pi-coding-agent--normalize-string-or-null
                       (plist-get node :label)))
               (base (append
                      (list :id (plist-get entry :id)
                            :parentId (when parent-visible
                                        (plist-get (plist-get parent-visible
                                                              :entry)
                                                   :id))
                            :timestamp (plist-get entry :timestamp))
                      (when label (list :label label))
                      (list :children (vconcat (gethash node child-nodes)))))
               (projected
                (if (equal (plist-get entry :type) "message")
                    (pi-coding-agent--jsonl-project-message
                     base (plist-get entry :message) branch-calls global-calls)
                  (pi-coding-agent--jsonl-project-entry
                   base entry))))
          (if parent-visible
              (puthash parent-visible
                       (cons projected (gethash parent-visible child-nodes))
                       child-nodes)
            (push projected built-roots)))))
    (list :tree (vconcat built-roots)
          :leafId (pi-coding-agent--jsonl-resolve-projected-leaf-id
                   tree leaf-id))))

(provide 'pi-coding-agent-jsonl)
;;; pi-coding-agent-jsonl.el ends here
