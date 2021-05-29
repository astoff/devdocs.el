;;; evdocs.el --- Emacs viewer for DevDocs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Augusto Stoffel

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: help
;; URL: https://github.com/astoff/evdocs
;; Package-Requires: ((emacs "27.1"))
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'seq)
(require 'shr)
(eval-when-compile
  (require 'let-alist))

(defgroup evdocs nil
  "Emacs viewer for DevDocs."
  :group 'help
  :prefix "evdocs-")

(defcustom evdocs-current-docs nil
  "A list of documents relevant to the current buffer."
  :local t
  :type '(list string))

(defcustom evdocs-data-dir (expand-file-name "devdocs" user-emacs-directory)
  "Directory to save documentation files."
  :type 'directory)

(defvar evdocs-site-url "https://devdocs.io"
  "Location of the DevDocs website.")

(defvar evdocs-cdn-url "https://documents.devdocs.io"
  "Location of the DevDocs CDN.")

(defcustom evdocs-timeout 300
  "Number of seconds to keep cached document indexes.")

(defcustom evdocs-separator " » "
  "String used to format a documentation location, e.g. in header line.")

;;; Documentation management

(defvar evdocs--doc-metadata (make-hash-table :test 'equal)
  "A hash table mapping document slugs to their metadata.
To be accessed through the function `evdocs--doc-metadata'.")

(defun evdocs--doc-metadata (doc &optional refresh)
  "Return the metadata for a document DOC.
Also populates `evdocs--doc-metadata' if necessary, either from
data on disk if REFRESH is nil, or from freshly downloaded data
otherwise."
  (when (or refresh (hash-table-empty-p evdocs--doc-metadata))
    (let* ((file (expand-file-name "docs.json" evdocs-data-dir))
           (docs (if (or refresh (not (file-exists-p file)))
                     (with-temp-file file
                       (url-insert-file-contents (format "%s/docs.json" evdocs-site-url))
                       (json-read))
                   (json-read-file file))))
      (clrhash evdocs--doc-metadata)
      (seq-doseq (doc docs)
        (puthash (alist-get 'slug doc) doc evdocs--doc-metadata))))
  (gethash doc evdocs--doc-metadata))

(defun evdocs--doc-title (doc)
  "Title of document with slug DOC"
  (let-alist (evdocs--doc-metadata doc)
    (if .version (concat .name " " .version) .name)))

(defun evdocs--read-document (prompt &optional pred multiple refresh)
  "Query interactively for a DevDocs document."
  (evdocs--doc-metadata nil refresh) ;; Maybe initialize and refresh
  (let (cands)
    (maphash (lambda (k _)
               (when (or (not pred) (funcall pred k))
                 (push (cons (evdocs--doc-title k) k) cands)))
             evdocs--doc-metadata)
    (if multiple
        (delq nil (mapcar (lambda (s) (cdr (assoc s cands)))
                          (completing-read-multiple prompt cands)))
      (cdr (assoc (completing-read prompt cands nil t) cands)))))

(defun evdocs--installed-p (doc)
  "Non-nil if DOC is installed."
  (file-exists-p
   (expand-file-name "metadata" (expand-file-name doc evdocs-data-dir))))

(defun evdocs-delete (doc)
  "Delete DevDocs documentation.
DOC is a document slug."
  (interactive (list (evdocs--read-document "Delete documentation: "
                                            #'evdocs--installed-p)))
  (let ((dest (file-name-as-directory
               (expand-file-name doc evdocs-data-dir))))
    (if (and (file-directory-p dest)
             (file-in-directory-p dest evdocs-data-dir))
        (delete-directory dest t t)
      (user-error (format "Documentation for `%s' is not installed" doc)))))

(defun evdocs-install (doc)
  "Download and install DevDocs documentation.
DOC is a document slug."
  (interactive (list (evdocs--read-document
                      "Install documentation: "
                      (lambda (s) (not (evdocs--installed-p s)))
                      nil 'refresh)))
  (let ((temp (make-temp-file "devdocs-" t)))
    (with-temp-buffer
      (url-insert-file-contents (format "%s/%s/db.json" evdocs-cdn-url doc))
      (seq-doseq (entry (json-read))
        (with-temp-file (expand-file-name
                         (url-hexify-string (format "%s.html" (car entry))) temp)
          (insert (cdr entry)))))
    (url-copy-file (format "%s/%s/index.json" evdocs-cdn-url doc)
                   (expand-file-name "index.json" temp))    
    (with-temp-file (expand-file-name "metadata" temp)
      (prin1 (evdocs--doc-metadata doc) (current-buffer)))
    (rename-file temp (expand-file-name doc evdocs-data-dir) t)
    (clrhash evdocs--index)
    (message "Documentation for `%s' installed." doc)))

;;; Manipulating a single document

(defvar evdocs--doc-index (make-hash-table))

(defun evdocs--doc-index (doc)
  (or (gethash doc evdocs--doc-index)
      (puthash doc
               (json-read-file (expand-file-name (concat doc "/index.json")
                                                 evdocs-data-dir))
               evdocs--doc-index)))

(defun evdocs--entries (doc)
  "Return a list of entries for document DOC."
  (mapcar (let ((docid (cons 'doc doc)))
            (lambda (entry)
              (let-alist entry
                (cons (concat .type ": " .name) (cons docid entry)))))
          (alist-get 'entries (evdocs--doc-index doc))))

(defvar evdocs--entries (make-hash-table))

(defun evdocs--entries (doc)
  "Return a list of entries for document DOC."
  (or (gethash doc evdocs--entries)
      (puthash doc
               (seq-map-indexed
                (let ((docid (cons 'doc doc)))
                  (lambda (entry i)
                    (let-alist entry
                      (thread-last entry
                        (cons `(index . ,i))
                        (cons docid)
                        (cons .name)))))
                (alist-get 'entries (json-read-file
                                     (expand-file-name (concat doc "/index.json")
                                                       evdocs-data-dir))))
               evdocs--entries)))


(defun evdocs--read-entry (prompt)
  (let* ((cands (mapcan #'evdocs--entries evdocs-current-docs)))
    (cdr (assoc (completing-read prompt cands nil t) cands))))

;;; Documentation viewer

(defvar-local evdocs--data nil
  "Alist of data, set buffer-locally when in `evdocs-mode'.")

(defvar evdocs-header-line
  '(:eval (let-alist evdocs--data
            (concat (evdocs--doc-title .doc) " » " .type " » " .name))))

(define-derived-mode evdocs-mode special-mode "DevDocs"
  "Mode for displaying DevDocs documents."
  (setq-local browse-url-browser-function
              (cons '("\\`[^:]*\\'" . evdocs--browse-url)
                    (if (functionp browse-url-browser-function)
                        `(("." . ,browse-url-browser-function))
                      browse-url-browser-function)))
  (setq buffer-undo-list t
        header-line-format evdocs-header-line
        truncate-lines t))

(defun evdocs-goto-target ()
  (interactive)
  (goto-char (point-min))
  (text-property-search-forward 'shr-target-id)
  (beginning-of-line))

(let ((map evdocs-mode-map))
  (define-key map [tab] 'forward-button)
  (define-key map [backtab] 'backward-button))

(defun evdocs--path-file (path)
  "Return the non-fragment part of PATH."
  (substring path 0 (string-match "#" path)))

(defun evdocs--path-fragment (path)
  "Return the fragment part of path, or nil if absent."
  (when-let ((i (string-match "#" path)))
    (substring path (1+ i))))

(defun evdocs--path-expand (path base)
  "Expand PATH relative to a BASE path."
  (pcase (string-to-char path)
    ('?/ path)
    ('?# (concat (evdocs--path-file base) path))
    (_ (concat (file-name-directory base) path))))

(defun evdocs--render (entry)
  "Render a DevDocs documentation entry, returning a buffer.

ENTRY is an alist like those returned by `evdocs--entries',
possibly with an additional ENTRY.fragment which overrides the
ENTRY.path fragment part."
  (or (libxml-available-p)
      (error "This function requires Emacs to be compiled with libxml2"))
  (with-current-buffer (get-buffer-create "*devdocs*")
    (evdocs-mode)
    (let-alist entry
      (let ((shr-target-id (or .fragment (evdocs--path-fragment .path)))
            (buffer-read-only nil)
            (file (expand-file-name (format "%s/%s.html" .doc (url-hexify-string
                                                               (evdocs--path-file .path)))
                                    evdocs-data-dir)))
        (erase-buffer)
        ;; TODO: cl-progv here for shr settings?
        (shr-insert-document
         (with-temp-buffer
           (insert-file-contents file)
           (libxml-parse-html-region (point-min) (point-max)))))
      (set-buffer-modified-p nil)
      (setq-local evdocs--data entry
                  evdocs-current-docs (list .doc))
      (evdocs-goto-target)
      (current-buffer))))

(defun evdocs--browse-url (url &rest _)
  (let-alist evdocs--data
    (let* ((dest (evdocs--path-expand url .path))
           (file (evdocs--path-file dest))
           (frag (evdocs--path-fragment dest))
           (entry (seq-some (lambda (it)
                              (when (let-alist it
                                      (or (string= .path dest)
                                          (string= .path file)))
                                it))
                            (evdocs--entries .doc))))
      (unless entry (error "Can't find `%s'" dest))
      (when frag (push `(fragment . ,frag) entry))
      (evdocs--render entry))))

(defun evdocs-lookup (&optional ask-docs)
  (interactive "P")
  (when (or ask-docs (not evdocs-current-docs))
    (setq-local evdocs-current-docs (evdocs--read-document
                                     "Docs for this buffer: "
                                     #'evdocs--installed-p t)))
  (let* ((entry (evdocs--read-entry "Go to documentation: "))
         (buffer (evdocs--render entry)))
    (with-selected-window (display-buffer buffer)
      (evdocs-goto-target)
      (recenter 0))))

(provide 'evdocs)
;;; evdocs.el ends here
