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

;; evdocs is a documentation viewer similar to Emacs's built-in Info
;; browser, but geared towards documentation obtained from
;; https://devdocs.io.

;; To get started, download some documentation with `evdocs-install`.
;; This will show the available documents and save the selected one to
;; disk.  Once you have the desired documents at hand, use
;; `evdocs-lookup` to search for entries.

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
  "Number of seconds to keep cached document indexes."
  :type 'number)

(defcustom evdocs-separator " » "
  "String used to format a documentation location, e.g. in header line."
  :type 'string)

(defvar evdocs--index (make-hash-table :test 'equal)
  "A hash table to cache document indices.
To be accessed through the function `evdocs--index'.")

(defvar evdocs-history nil
  "History of documentation entries.")

;;; Documentation management

(defvar evdocs--doc-metadata (make-hash-table :test 'equal)
  "A hash table mapping document slugs to their metadata.
To be accessed through the function `evdocs--doc-metadata'.")

(defun evdocs--doc-metadata (doc &optional refresh)
  "Return the metadata for a document DOC.
Also populates the variable `evdocs--doc-metadata' if necessary,
either from data on disk if REFRESH is nil, or from freshly
downloaded data otherwise."
  (when (or refresh (hash-table-empty-p evdocs--doc-metadata))
    (let* ((file (expand-file-name "docs.json" evdocs-data-dir))
           (docs (if (or refresh (not (file-exists-p file)))
                     (with-temp-file file
                       (make-directory (file-name-directory file) t)
                       (url-insert-file-contents (format "%s/docs.json" evdocs-site-url))
                       (json-read))
                   (json-read-file file))))
      (clrhash evdocs--doc-metadata)
      (seq-doseq (doc docs)
        (puthash (alist-get 'slug doc) doc evdocs--doc-metadata))))
  (gethash doc evdocs--doc-metadata))

(defun evdocs--doc-title (doc)
  "Title of document with slug DOC."
  (let-alist (evdocs--doc-metadata doc)
    (if .version (concat .name " " .version) .name)))

(defun evdocs--read-document (prompt &optional predicate multiple refresh)
  "Query interactively for a DevDocs document.
PROMPT and PREDICATE as `completing-read'.
MULTIPLE, if non-nil, allows selecting multiple documents.
REFRESH, if non-nil, downloads the DevDocs document list anew."
  (evdocs--doc-metadata nil refresh) ;; Maybe initialize and refresh
  (let (cands)
    (maphash (lambda (k _)
               (when (or (not predicate) (funcall predicate k))
                 (push (cons (evdocs--doc-title k) k) cands)))
             evdocs--doc-metadata)
    (unless cands (user-error "No documents"))
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
    (message "Installed %s documentation" (evdocs--doc-title doc))))

;;; Document indexes

(defun evdocs--index (doc)
  "Return the index of document DOC.
This is an alist containing `entries' and `types'."
  (if-let ((idx (gethash doc evdocs--index)))
      (prog1 idx
        (timer-set-time (alist-get 'timer idx) evdocs-timeout))
    (let* ((docid (cons 'doc doc))
           (idx (json-read-file (expand-file-name (concat doc "/index.json")
                                                  evdocs-data-dir)))
           (entries (alist-get 'entries idx)))
      (setf (alist-get 'timer idx)
            (run-at-time evdocs-timeout nil
                         (lambda () (remhash doc evdocs--index))))
      (seq-do-indexed (lambda (entry i)
                        (push `(index . ,i) entry)
                        (push docid entry)
                        (aset entries i entry))
                      entries)
      (puthash doc idx evdocs--index))))

;;; Documentation viewer

(defvar-local evdocs--stack nil
  "List of viewed entries, set buffer-locally when in `evdocs-mode'.")

(defvar-local evdocs--forward-stack nil
  "List of viewed entries for `evdocs-go-forward'.")

(defvar evdocs-header-line
  '(:eval (let-alist (car evdocs--stack)
            (concat (evdocs--doc-title .doc)
                    evdocs-separator .type
                    evdocs-separator .name))))

(define-derived-mode evdocs-mode special-mode "DevDocs"
  "Major mode for viewing DevDocs documents."
  (setq-local browse-url-browser-function
              (cons '("\\`[^:]*\\'" . evdocs--browse-url)
                    (if (functionp browse-url-browser-function)
                        `(("." . ,browse-url-browser-function))
                      browse-url-browser-function)))
  (setq buffer-undo-list t
        header-line-format evdocs-header-line
        truncate-lines t))

(defun evdocs-goto-target ()
  "Go to the original position in a DevDocs buffer."
  (interactive)
  (goto-char (point-min))
  (text-property-search-forward 'shr-target-id)
  (beginning-of-line))

(defun evdocs-go-back ()
  "Go to the previously displayed entry in this DevDocs buffer."
  (interactive)
  (unless (cadr evdocs--stack)
    (user-error "No previous entry"))
  (push (pop evdocs--stack) evdocs--forward-stack)
  (evdocs--render (pop evdocs--stack)))

(defun evdocs-go-forward ()
  "Go to the next entry in this DevDocs buffer."
  (interactive)
  (unless (car evdocs--forward-stack)
    (user-error "No next entry"))
  (evdocs--render (pop evdocs--forward-stack)))

(let ((map evdocs-mode-map))
  (define-key map [tab] 'forward-button)
  (define-key map [backtab] 'backward-button)
  (define-key map "l" 'evdocs-go-back)
  (define-key map "r" 'evdocs-go-forward)
  (define-key map "." 'evdocs-goto-target))

;;; Rendering

(defun evdocs--path-file (path)
  "Return the non-fragment part of PATH."
  (substring path 0 (string-match "#" path)))

(defun evdocs--path-fragment (path)
  "Return the fragment part of PATH, or nil if absent."
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

ENTRY is an alist like those in the variable `evdocs--index',
possibly with an additional ENTRY.fragment which overrides the
fragment part of ENTRY.path."
  (or (libxml-available-p)
      (error "This function requires Emacs to be compiled with libxml2"))
  (with-current-buffer (get-buffer-create "*devdocs*")
    (unless (eq major-mode 'evdocs-mode)
      (evdocs-mode))
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
      (setq-local evdocs-current-docs (list .doc))
      (push entry evdocs--stack)
      (evdocs-goto-target)
      (current-buffer))))

(defun evdocs--browse-url (url &rest _)
  "A suitable `browse-url-browser-function' for `devdocs-mode'.
URL can be an internal link in a DevDocs document."
  (let-alist (car evdocs--stack)
    (let* ((dest (evdocs--path-expand url .path))
           (file (evdocs--path-file dest))
           (frag (evdocs--path-fragment dest))
           (entry (seq-some (lambda (it)
                              (when (let-alist it
                                      (or (string= .path dest)
                                          (string= .path file)))
                                it))
                            (alist-get 'entries (evdocs--index .doc)))))
      (unless entry (error "Can't find `%s'" dest))
      (when frag (push `(fragment . ,frag) entry))
      (evdocs--render entry))))

;;; Lookup command

(defun evdocs--entries (doc)
  "A list of entries in DOC, as propertized strings."
  (seq-map (lambda (it)
             (let ((s (let-alist it
                        ;; Disambiguate of entries with same .name
                        (format #("%s\0%c%s" 2 7
                                  (invisible t rear-nonsticky t cursor-intangible t))
                                .name .index .doc))))
               (prog1 s (put-text-property 0 1 'evdocs--data it s))))
           (alist-get 'entries (evdocs--index doc))))

(defun evdocs--get-data (str)
  "Get data stored as a string property in STR."
  (get-text-property 0 'evdocs--data str))

(defun evdocs--annotate (cand)
  "Return an annotation for `evdocs--read-entry' candidate CAND."
  (let-alist (evdocs--get-data cand)
    (concat " " (propertize " " 'display '(space :align-to 40))
     (evdocs--doc-title .doc) evdocs-separator .type)))

(defun evdocs--read-entry (prompt)
  "Read the name of an entry in a document, using PROMPT.
All entries of `evdocs-current-docs' are listed."
  (let* ((cands (mapcan #'evdocs--entries evdocs-current-docs))
         (metadata '(metadata
                     (category . evdocs)
                     (annotation-function . evdocs--annotate)))
         (coll (lambda (string predicate action)
                 (if (eq action 'metadata)
                     metadata
                   (complete-with-action action cands string predicate)))))
    (evdocs--get-data
     (car (member (completing-read prompt coll nil t nil
                                   'evdocs-history
                                   (thing-at-point 'symbol))
                  cands)))))

(defun evdocs-lookup (&optional ask-docs)
  "Look up a DevDocs documentation entry.

Display entries in the documents `evdocs-current-docs' for
selection.  With a prefix argument (or, from Lisp, if ASK-DOCS is
non-nil), first read a list of available documents and set
`evdocs-current-docs' for this buffer."
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
