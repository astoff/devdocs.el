;;; devdocs.el --- Emacs viewer for DevDocs -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: help
;; URL: https://github.com/astoff/devdocs.el
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.3

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

;; devdocs.el is a documentation viewer similar to the built-in Info
;; browser, but geared towards documentation obtained from
;; https://devdocs.io.

;; To get started, download some documentation with `devdocs-install`.
;; This will show the available documents and save the selected one to
;; disk.  Once you have the desired documents at hand, use
;; `devdocs-lookup` to search for entries.

;;; Code:

(require 'org-src)
(require 'seq)
(require 'shr)
(require 'url-expand)
(eval-when-compile
  (require 'let-alist))

(unless (libxml-available-p)
  (display-warning 'devdocs "This package requires Emacs to be compiled with libxml2"))

(defgroup devdocs nil
  "Emacs viewer for DevDocs."
  :group 'help
  :prefix "devdocs-")

(defcustom devdocs-current-docs nil
  "A list of documents relevant to the current buffer.
This variable is normally set by the `devdocs-lookup' command,
but you may also wish to set it via a hook or as file or
directory-local variable."
  :local t
  :type '(list string))

(defcustom devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory)
  "Directory to save documentation files."
  :type 'directory)

(defvar devdocs-site-url "https://devdocs.io"
  "Location of the DevDocs website.")

(defvar devdocs-cdn-url "https://documents.devdocs.io"
  "Location of the DevDocs CDN.")

(defcustom devdocs-cache-timeout 900
  "Number of seconds to keep cached information such as document indexes."
  :type 'number)

(defcustom devdocs-separator " » "
  "String used to format a documentation location, e.g. in header line."
  :type 'string)

(defcustom devdocs-disambiguated-entry-format #("%s (%s)" 3 7 (face italic))
  "How to disambiguate entries with identical names in `devdocs-lookup'.
This string is passed to `format' with two arguments, the entry
name and a count."
  :type '(choice (const :tag "Count in parentheses, italicized"
                        #("%s (%s)" 3 7 (face italic)))
                 (const :tag "Invisible cookie"
                        #("%s (%s)" 2 7 (invisible t)))
                 string))

(defcustom devdocs-fontify-code-blocks t
  "Whether to fontify code snippets inside pre tags.
Fontification is done using the `org-src' library, which see."
  :type 'boolean)

(defcustom devdocs-isearch-wrap #'devdocs-isearch-wrap
  "How to wrap isearch when reaching the beginning or end of a page.
This is used as `isearch-wrap-function' in DevDocs buffers.  If
nil, keep the default wrapping behavior."
  :type '(choice (const :tag "Jump to next or previous page" #'devdocs-isearch-wrap)
                 (const :tag "Wrap as usual, staying in the same page" nil)
                 function))

(defvar devdocs-buffer-name "*devdocs*")

(defvar devdocs-history nil
  "History of documentation entries.")

(defconst devdocs--data-format-version 1
  "Version number of the saved documentation data format.")

;;; Memoization

(defvar devdocs--cache (make-hash-table :test 'equal)
  "Hash table used by `devdocs--with-cache'.")

(defmacro devdocs--with-cache (&rest body)
  "Evaluate BODY with memoization.
The return value is stored and reused if needed again within the
time span specified by `devdocs-cache-timeout'.

Note that the lexical environment is used to associate BODY to
its return value; take the necessary precautions."
  `(if-let ((fun (lambda () ,@body))
            (funrep ,(if (< emacs-major-version 28) ;; Cf. bug#32503
                         '(prin1-to-string fun)
                       'fun))
            (data (gethash funrep devdocs--cache)))
       (prog1 (cdr data)
         (timer-set-time (car data) (time-add nil devdocs-cache-timeout)))
     (let ((val (funcall fun))
           (timer (run-at-time devdocs-cache-timeout nil
                               #'remhash funrep devdocs--cache)))
       (prog1 val
         (puthash funrep (cons timer val) devdocs--cache)))))

;;; Documentation management

(defun devdocs--doc-metadata (slug)
  "Return the metadata of an installed document named SLUG."
  (let ((file (expand-file-name (concat slug "/metadata") devdocs-data-dir)))
    (unless (file-exists-p file)
      (user-error "Document `%s' is not installed" slug))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((metadata (read (current-buffer))))
        (unless (eq (car metadata) devdocs--data-format-version)
          (user-error "Please run `devdocs-update-all'"))
        (cdr metadata)))))

(defun devdocs--installed-docs ()
  "Return a list of installed documents."
  (mapcar #'devdocs--doc-metadata
          (let ((default-directory devdocs-data-dir))
            (seq-filter #'file-directory-p
                        (when (file-directory-p devdocs-data-dir)
                          (directory-files "." nil "^[^.]"))))))

(defun devdocs--available-docs ()
  "Return a list of available documents.
If necessary, download data from `devdocs-site-url'."
  (devdocs--with-cache
   (with-temp-buffer
     (url-insert-file-contents
      (format "%s/docs.json" devdocs-site-url))
     (json-read))))

(defun devdocs--doc-title (doc)
  "Title of document DOC.
DOC is either a metadata alist, or the slug of an installed
document."
  (let-alist (if (stringp doc) (devdocs--doc-metadata doc) doc)
    (if (seq-empty-p .version) .name (concat .name " " .version))))

(defun devdocs--read-document (prompt &optional multiple available)
  "Query interactively for a DevDocs document.

PROMPT is passed to `completing-read'.
Non-nil MULTIPLE allows selecting multiple documents.
Non-nil AVAILABLE means to offer a list of all available documents;
otherwise, offer only installed documents.

Return a document metadata alist if MULTIPLE is nil; otherwise, a
list of metadata alists."
  (let ((cands (mapcar (lambda (it) (cons (alist-get 'slug it) it))
                       (if available
                           (devdocs--available-docs)
                         (or (devdocs--installed-docs)
                             (user-error "No documents in `%s'" devdocs-data-dir))))))
    (if multiple
        (delq nil (mapcar (lambda (s) (cdr (assoc s cands)))
                          (completing-read-multiple prompt cands)))
      (cdr (assoc (completing-read prompt cands nil t) cands)))))

;;;###autoload
(defun devdocs-delete (doc)
  "Delete DevDocs documentation.
DOC is a document metadata alist."
  (interactive (list (devdocs--read-document "Delete documentation: ")))
  (let ((dest (expand-file-name (alist-get 'slug doc) devdocs-data-dir)))
    (if (and (file-directory-p dest)
             (file-in-directory-p dest devdocs-data-dir))
        (delete-directory dest t)
      (user-error "Document `%s' is not installed" (alist-get 'slug doc)))))

;;;###autoload
(defun devdocs-install (doc)
  "Download and install DevDocs documentation.
DOC is a document metadata alist."
  (interactive (list (devdocs--read-document "Install documentation: " nil t)))
  (make-directory devdocs-data-dir t)
  (let* ((slug (alist-get 'slug doc))
         (mtime (alist-get 'mtime doc))
         (temp (make-temp-file "devdocs-" t))
         pages)
    (with-temp-buffer
      (url-insert-file-contents (format "%s/%s/db.json?%s" devdocs-cdn-url slug mtime))
      (dolist (entry (let ((json-key-type 'string))
                       (json-read)))
        (with-temp-file (expand-file-name
                         (url-hexify-string (format "%s.html" (car entry))) temp)
          (push (car entry) pages)
          (insert (cdr entry)))))
    (with-temp-buffer
      (url-insert-file-contents (format "%s/%s/index.json?%s" devdocs-cdn-url slug mtime))
      (let ((index (json-read)))
        (push `(pages . ,(vconcat (nreverse pages))) index)
        (with-temp-file (expand-file-name "index" temp)
          (prin1 index (current-buffer)))))
    (with-temp-file (expand-file-name "metadata" temp)
      (prin1 (cons devdocs--data-format-version doc) (current-buffer)))
    (let ((dest (expand-file-name slug devdocs-data-dir)))
      (when (and (file-directory-p dest)
                 (file-in-directory-p dest devdocs-data-dir))
        (delete-directory dest t))
      (rename-file (file-name-as-directory temp) dest))
    (message "Document `%s' installed" slug)))

;;;###autoload
(defun devdocs-update-all ()
  "Reinstall all documents with a new version available."
  (interactive)
  (when-let ((installed (when (file-directory-p devdocs-data-dir)
                          (directory-files devdocs-data-dir nil "^[^.]")))
             (newer (seq-filter
                     (lambda (doc)
                       (let-alist doc
                         (and (member .slug installed)
                              (< (alist-get 'mtime
                                            (ignore-errors (devdocs--doc-metadata .slug))
                                            0) ;; Update docs with an old data format too
                                 .mtime))))
                     (devdocs--available-docs)))
             ((y-or-n-p (format "Update %s documents %s?"
                                (length newer)
                                (mapcar (lambda (d) (alist-get 'slug d)) newer)))))
    (dolist (doc newer)
      (devdocs-install doc))))

;;; Document indexes

(defun devdocs--index (doc)
  "Return the index of document DOC.
This is an alist containing `entries', `pages' and `types'."
  (let* ((docid (cons 'doc doc))
         (idx (with-temp-buffer
                (insert-file-contents (expand-file-name
                                       (concat (alist-get 'slug doc) "/index")
                                       devdocs-data-dir))
                (read (current-buffer))))
         (entries (alist-get 'entries idx)))
    (prog1 idx
      (seq-do-indexed (lambda (entry i)
                        (push docid entry)
                        (push `(index . ,i) entry)
                        (aset entries i entry))
                      entries))))

;;; Documentation viewer

(defvar-local devdocs--stack nil
  "List of viewed entries, set buffer-locally when in `devdocs-mode'.")

(defvar-local devdocs--forward-stack nil
  "List of viewed entries for `devdocs-go-forward'.")

(defvar devdocs-header-line
  '(:eval (let-alist (car devdocs--stack)
            (concat (devdocs--doc-title .doc)
                    (and .type devdocs-separator) .type
                    (and .name devdocs-separator) .name))))

(define-derived-mode devdocs-mode special-mode "DevDocs"
  "Major mode for viewing DevDocs documents."
  (setq-local
   browse-url-browser-function 'devdocs--browse-url
   buffer-undo-list t
   header-line-format devdocs-header-line
   revert-buffer-function 'devdocs--revert-buffer
   truncate-lines t)
  (when devdocs-isearch-wrap
    (setq-local isearch-wrap-function devdocs-isearch-wrap)))

(defun devdocs-goto-target ()
  "Go to the original position in a DevDocs buffer."
  (interactive)
  (goto-char (point-min))
  (when-let ((pred (if (fboundp 'shr--set-target-ids) #'member t)) ;; shr change in Emacs 29
             (match (text-property-search-forward 'shr-target-id shr-target-id pred)))
    (goto-char (prop-match-beginning match))))

(defun devdocs-go-back ()
  "Go to the previously displayed entry in this DevDocs buffer."
  (interactive)
  (unless (cadr devdocs--stack)
    (user-error "No previous entry"))
  (push (pop devdocs--stack) devdocs--forward-stack)
  (devdocs--render (pop devdocs--stack)))

(defun devdocs-go-forward ()
  "Go to the next entry in this DevDocs buffer."
  (interactive)
  (unless (car devdocs--forward-stack)
    (user-error "No next entry"))
  (devdocs--render (pop devdocs--forward-stack)))

(defun devdocs-next-entry (count)
  "Go forward COUNT entries in this document.

Note that this refers to the index order, which may not coincide
with the order of appearance in the text."
  (interactive "p")
  (let-alist (car devdocs--stack)
    (unless .index
      (user-error "No current entry"))
    (devdocs--render
     (or (ignore-error 'args-out-of-range
           (elt (alist-get 'entries (devdocs--index .doc))
                (+ count .index)))
         (user-error (if (< count 0) "No previous entry" "No next entry"))))))

(defun devdocs-previous-entry (count)
  "Go backward COUNT entries in this document."
  (interactive "p")
  (devdocs-next-entry (- count)))

(defun devdocs-next-page (count)
  "Go forward COUNT pages in this document."
  (interactive "p")
  (let-alist (car devdocs--stack)
    (let* ((pages (devdocs--with-cache
                   (alist-get 'pages (devdocs--index .doc))))
           (page (+ count (seq-position pages (devdocs--path-file .path))))
           (path (or (ignore-error 'args-out-of-range (elt pages page))
                     (user-error (if (< count 0) "No previous page" "No next page")))))
      (devdocs--render `((doc . ,.doc)
                         (path . ,path)
                         (name . ,(format "%s/%s" (1+ page) (length pages))))))))

(defun devdocs-previous-page (count)
  "Go backward COUNT entries in this document."
  (interactive "p")
  (devdocs-next-page (- count)))

(defun devdocs-copy-url ()
  "Copy the URL of the current DevDocs page to the kill ring."
  (interactive)
  (let-alist (or (car devdocs--stack)
                 (user-error "Not in a DevDocs buffer"))
    (let ((url (url-encode-url
                (format "%s/%s/%s"
                        devdocs-site-url
                        .doc.slug
                        (if .fragment
                            (concat (devdocs--path-file .path) "#" .fragment)
                          .path)))))
      (kill-new url)
      (message "Copied %s" url))))

(defun devdocs-isearch-wrap ()
  "Continue isearch in the next or previous document page."
  (let ((reporter (make-progress-reporter "Searching"))
        (entry (car devdocs--stack))
        (direction (if isearch-forward +1 -1)))
    (with-temp-buffer
      (let ((devdocs-buffer-name (buffer-name (current-buffer)))
            (case-fold-search isearch-case-fold-search)
            (isearch-forward t))
        (setq-local devdocs--stack (list entry))
        (while (progn
                 (progress-reporter-update reporter)
                 (when (bound-and-true-p isearch-mb-mode)
                   (let ((inhibit-redisplay nil)) (redisplay))
                   (when quit-flag (signal 'quit nil)))
                 (devdocs-next-page direction)
                 (not (isearch-search-string isearch-string nil t))))
        (setq entry (car devdocs--stack))))
    (progress-reporter-done reporter)
    (devdocs--render entry)
    (setq isearch-wrapped nil)
    (goto-char (if isearch-forward (point-min) (point-max)))))

(let ((map devdocs-mode-map))
  (define-key map [tab] 'forward-button)
  (define-key map [backtab] 'backward-button)
  (define-key map "i" 'devdocs-lookup)
  (define-key map "p" 'devdocs-previous-entry)
  (define-key map "n" 'devdocs-next-entry)
  (define-key map "[" 'devdocs-previous-page)
  (define-key map "]" 'devdocs-next-page)
  (define-key map "l" 'devdocs-go-back)
  (define-key map "r" 'devdocs-go-forward)
  (define-key map "w" 'devdocs-copy-url)
  (define-key map "." 'devdocs-goto-target))

;;; Rendering

(defun devdocs--path-file (path)
  "Return the non-fragment part of PATH."
  (substring path 0 (string-match "#" path)))

(defun devdocs--path-fragment (path)
  "Return the fragment part of PATH, or nil if absent."
  (when-let ((i (string-match "#" path)))
    (substring path (1+ i))))

(defun devdocs--path-expand (path base)
  "Expand PATH relative to a BASE path."
  (pcase (string-to-char path)
    ('?/ path)
    ('?# (concat (devdocs--path-file base) path))
    (_ (substring ;; ugly!
        (url-expander-remove-relative-links ;; undocumented function!
         (concat (file-name-directory base) path))
        1))))

(defun devdocs--shr-tag-pre (dom)
  "Insert and fontify pre-tag represented by DOM."
  (let ((start (point)))
    (shr-tag-pre dom)
    (when-let ((lang (and devdocs-fontify-code-blocks
                          (dom-attr dom 'data-language))))
      (org-src-font-lock-fontify-block (downcase lang) start (point)))))

(defun devdocs--render (entry)
  "Render a DevDocs documentation entry, returning a buffer.

ENTRY is an alist like those in the variable `devdocs--index',
possibly with an additional ENTRY.fragment which overrides the
fragment part of ENTRY.path."
  (with-current-buffer (get-buffer-create devdocs-buffer-name)
    (unless (eq major-mode 'devdocs-mode)
      (devdocs-mode))
    (let-alist entry
      (let ((buffer-read-only nil)
            (shr-external-rendering-functions (cons '(pre . devdocs--shr-tag-pre)
                                                    shr-external-rendering-functions))
            (file (expand-file-name (format "%s/%s.html"
                                            .doc.slug
                                            (url-hexify-string (devdocs--path-file .path)))
                                    devdocs-data-dir)))
        (erase-buffer)
        (setq-local shr-target-id (or .fragment (devdocs--path-fragment .path)))
        ;; TODO: cl-progv here for shr settings?
        (shr-insert-document
         (with-temp-buffer
           (insert-file-contents file)
           (libxml-parse-html-region (point-min) (point-max)))))
      (set-buffer-modified-p nil)
      (setq-local devdocs-current-docs (list .doc.slug))
      (push entry devdocs--stack)
      (devdocs-goto-target)
      (current-buffer))))

(defun devdocs--revert-buffer (&rest _args)
  "Refresh DevDocs buffer."
  (devdocs--render (pop devdocs--stack)))

(defun devdocs--browse-url (url &rest args)
  "A suitable `browse-url-browser-function' for `devdocs-mode'.
URL can be an internal link in a DevDocs document.
ARGS is passed as is to `browse-url'."
  (if (string-match-p ":" url)
      (let ((browse-url-browser-function (default-value 'browse-url-browser-function)))
        (apply #'browse-url url args))
    (let-alist (car devdocs--stack)
      (let* ((dest (devdocs--path-expand url .path))
             (file (devdocs--path-file dest))
             (frag (devdocs--path-fragment dest))
             (entry (seq-some (lambda (it)
                                (when (let-alist it
                                        (or (string= .path dest)
                                            (string= .path file)))
                                  it))
                              (alist-get 'entries (devdocs--index .doc)))))
        (unless entry (error "Can't find `%s'" dest))
        (push `(doc . ,.doc) entry)
        (when frag (push `(fragment . ,frag) entry))
        (devdocs--render entry)))))

;;; Lookup command

(defun devdocs--entries (documents)
  "A list of entries in DOCUMENTS, as propertized strings."
  (let* ((counts (make-hash-table :test 'equal))
         (mkentry (lambda (it)
                    (let* ((name (alist-get 'name it))
                           (count (1+ (gethash name counts 0))))
                      (puthash name count counts)
                      `(,name ,count . ,it))))
         (entries (mapcan (lambda (doc)
                            (mapcar mkentry
                                    (alist-get 'entries (devdocs--index doc))))
                          documents)))
    (mapcar (pcase-lambda (`(,name ,count . ,it))
              (propertize (if (= 1 (gethash name counts))
                              name
                            (format devdocs-disambiguated-entry-format name count))
                          'devdocs--data it))
            entries)))

(defun devdocs--get-data (str)
  "Get data stored as a string property in STR."
  (get-text-property 0 'devdocs--data str))

(defun devdocs--annotate (cand)
  "Return an annotation for `devdocs--read-entry' candidate CAND."
  (let-alist (devdocs--get-data cand)
    (concat " " (propertize " " 'display '(space :align-to 40))
     (devdocs--doc-title .doc) devdocs-separator .type)))

(defun devdocs--relevant-docs (ask)
  "Return a list of relevant documents for the current buffer.
May ask interactively for the desired documents.  If ASK is
non-nil, ask unconditionally."
  (if ask
      (let ((docs (devdocs--read-document "Documents for this buffer: " t)))
        (prog1 docs
          (setq-local devdocs-current-docs
                      (mapcar (lambda (d) (alist-get 'slug d)) docs))))
    (or (mapcar #'devdocs--doc-metadata devdocs-current-docs)
        (devdocs--relevant-docs t)
        (user-error "No documents"))))

(defun devdocs--read-entry (prompt documents initial-input)
  "Read the name of an entry in one of the DOCUMENTS, using PROMPT.

INITIAL-INPUT is passed to `completing-read'"
  (let* ((cands (devdocs--with-cache
                 (devdocs--entries documents)))
         (metadata '(metadata
                     (category . devdocs)
                     (annotation-function . devdocs--annotate)))
         (coll (lambda (string predicate action)
                 (if (eq action 'metadata)
                     metadata
                   (complete-with-action action cands string predicate))))
         (cand (completing-read prompt coll nil t initial-input
                                'devdocs-history
                                (thing-at-point 'symbol))))
    (devdocs--get-data (car (member cand cands)))))

;;;###autoload
(defun devdocs-lookup (&optional ask-docs initial-input)
  "Look up a DevDocs documentation entry.

Display entries in the documents `devdocs-current-docs' for
selection.  With a prefix argument (or, from Lisp, if ASK-DOCS is
non-nil), first read the name of one or more installed documents
and set `devdocs-current-docs' for this buffer.

If INITIAL-INPUT is not nil, insert it into the minibuffer."
  (interactive "P")
  (let* ((entry (devdocs--read-entry "Go to documentation: "
                                     (devdocs--relevant-docs ask-docs)
                                     initial-input))
         (buffer (devdocs--render entry)))
    (with-selected-window (display-buffer buffer)
      (devdocs-goto-target)
      (recenter 0))))

;;;###autoload
(defun devdocs-peruse (doc)
  "Read a document from the first page."
  (interactive (list (devdocs--read-document "Peruse documentation: ")))
  (let ((pages (alist-get 'pages (devdocs--index doc))))
    (pop-to-buffer
     (devdocs--render `((path . ,(seq-first pages))
                        (doc . ,doc)
                        (name . ,(format "%s/%s" 1 (length pages))))))))

;;; Compatibility with the old devdocs package

;;;###autoload
(defun devdocs-search (query)
  "Search for QUERY in the DevDocs website."
  (interactive (list (read-from-minibuffer
                      (format "Search %s: " devdocs-site-url)
                      nil nil nil nil (thing-at-point 'symbol))))
  (browse-url (format "%s/#q=%s" devdocs-site-url (url-hexify-string query))))

(provide 'devdocs)
;;; devdocs.el ends here
