;;; pi-coding-agent-org.el --- Org links for pi coding agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pi-coding-agent

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Optional Org link support for pi-coding-agent sessions.
;; Loading the base pi-coding-agent package does not require Org; users can load
;; this file from `org-mode' configuration to enable pi: links.

;;; Code:

(require 'pi-coding-agent)
(require 'org)
(require 'subr-x)
(require 'url-util)

(defun pi-coding-agent-org--query-encode (value)
  "URL-encode VALUE for a pi Org link query parameter."
  (url-hexify-string (or value "")))

(defun pi-coding-agent-org--query-decode (value)
  "URL-decode VALUE from a pi Org link query parameter."
  (url-unhex-string (replace-regexp-in-string "+" " " (or value ""))))

(defun pi-coding-agent-org--parse-link (path)
  "Parse a pi Org link PATH.
Returns a plist with :kind and decoded query parameter keys such as :file,
:dir, and :name."
  (let* ((parts (split-string path "\\?" t))
         (kind (car parts))
         (query (cadr parts))
         (plist (list :kind kind)))
    (when query
      (dolist (part (split-string query "&" t))
        (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" part)
          (let ((key (intern (concat ":" (match-string 1 part))))
                (value (pi-coding-agent-org--query-decode
                        (match-string 2 part))))
            (setq plist (plist-put plist key value))))))
    plist))

(defun pi-coding-agent-org-session-link-target (&optional file dir name)
  "Return a pi: session link target from FILE or DIR and NAME.
FILE is preferred as the durable session identity.  DIR and NAME are used for
links to named sessions that do not yet have a known durable session file."
  (concat
   "session?"
   (cond
    ((and file (not (string-empty-p file)))
     (concat "file=" (pi-coding-agent-org--query-encode
                      (expand-file-name file))
             (when dir
               (concat "&dir=" (pi-coding-agent-org--query-encode
                                (file-name-as-directory
                                 (expand-file-name dir)))))
             (when (and name (not (string-empty-p name)))
               (concat "&name=" (pi-coding-agent-org--query-encode name)))))
    ((and dir name (not (string-empty-p name)))
     (concat "dir=" (pi-coding-agent-org--query-encode
                     (file-name-as-directory (expand-file-name dir)))
             "&name=" (pi-coding-agent-org--query-encode name)))
    (dir
     (concat "dir=" (pi-coding-agent-org--query-encode
                     (file-name-as-directory (expand-file-name dir)))))
    (t
     (user-error "No pi session file or directory known")))))

(defun pi-coding-agent-org-session-link (&optional file dir name description)
  "Return an Org bracket link for a pi session.
FILE, DIR, and NAME are passed to `pi-coding-agent-org-session-link-target'.
DESCRIPTION defaults to a generic session label."
  (format "[[pi:%s][%s]]"
          (pi-coding-agent-org-session-link-target file dir name)
          (or description "Open pi session")))

(defun pi-coding-agent-org--current-session-link-data (&optional chat-buffer)
  "Return Org link data for CHAT-BUFFER or the current pi session.
The result is a plist with :link and :description, or nil when point is not in
a pi chat/input session buffer."
  (let ((chat-buf (or chat-buffer (pi-coding-agent--get-chat-buffer))))
    (when (and (buffer-live-p chat-buf)
               (with-current-buffer chat-buf
                 (derived-mode-p 'pi-coding-agent-chat-mode)))
      (with-current-buffer chat-buf
        (let* ((file (pi-coding-agent-session-file chat-buf))
               (dir (pi-coding-agent--chat-session-directory chat-buf))
               (name (pi-coding-agent-session-display-name chat-buf))
               (target (pi-coding-agent-org-session-link-target file dir name))
               (description (if (and name (not (string-empty-p name)))
                                (format "pi session: %s" name)
                              "pi session")))
          (list :link (concat "pi:" target)
                :description description))))))

(defun pi-coding-agent-org-store-session-link (&optional _interactive)
  "Store an Org link to the current pi session.
This is used by `org-store-link' from pi chat and input buffers."
  (when-let* ((data (pi-coding-agent-org--current-session-link-data)))
    (org-link-store-props :type "pi"
                          :link (plist-get data :link)
                          :description (plist-get data :description))
    t))

;;;###autoload
(defun pi-coding-agent-org-open-link (path _)
  "Open pi Org link PATH."
  (let* ((info (pi-coding-agent-org--parse-link path))
         (kind (plist-get info :kind)))
    (pcase kind
      ("session"
       (cond
        ((plist-get info :file)
         (pi-coding-agent-session-open-file
          (plist-get info :file)
          (plist-get info :dir)
          (plist-get info :name)))
        ((plist-get info :dir)
         (pi-coding-agent-session-open
          (plist-get info :dir)
          (plist-get info :name)))
        (t
         (user-error "pi session link lacks file or dir"))))
      (_
       (user-error "Unsupported pi link kind: %s" kind)))))

(defun pi-coding-agent-org-export-link (path description _backend)
  "Export pi Org link PATH with DESCRIPTION."
  (or description (format "pi:%s" path)))

(org-link-set-parameters "pi"
                         :follow #'pi-coding-agent-org-open-link
                         :export #'pi-coding-agent-org-export-link
                         :store #'pi-coding-agent-org-store-session-link)

(provide 'pi-coding-agent-org)
;;; pi-coding-agent-org.el ends here
