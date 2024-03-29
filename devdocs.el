;;; devdocs.el --- Emacs viewer for DevDocs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: help
;; URL: https://github.com/astoff/devdocs.el
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.6.1

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

(require 'seq)
(require 'shr)
(require 'url-expand)
(eval-when-compile
  (require 'let-alist)
  (require 'subr-x))

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
                        #("%s#%s" 2 5 (invisible t)))
                 string))

(defcustom devdocs-fontify-code-blocks t
  "Whether to fontify code snippets inside pre tags.
Fontification is done using the `org-src' library, which see."
  :type 'boolean)

(defcustom devdocs-window-select nil
  "Whether to select the DevDocs window for viewing."
  :type 'boolean)

(defface devdocs-code-block '((t nil))
  "Additional face to apply to code blocks in DevDocs buffers.")

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

(defalias 'devdocs--json-parse-buffer
  (if (json-available-p)
      (lambda () (json-parse-buffer :object-type 'alist))
    (require 'json)
    #'json-read))

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
     (devdocs--json-parse-buffer))))

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
DOC is a document slug or metadata alist.  If the document is
already installed, reinstall it."
  (interactive (list (devdocs--read-document "Install documentation: " nil t)))
  (make-directory devdocs-data-dir t)
  (unless (listp doc)
    (setq doc (or (seq-find (lambda (it) (string= doc (alist-get 'slug it)))
                            (devdocs--available-docs))
                  (user-error "No such document: %s" doc))))
  (let* ((slug (alist-get 'slug doc))
         (mtime (alist-get 'mtime doc))
         (temp (make-temp-file "devdocs-" t))
         pages)
    (with-temp-buffer
      (url-insert-file-contents (format "%s/%s/db.json?%s" devdocs-cdn-url slug mtime))
      (dolist-with-progress-reporter
          (entry (devdocs--json-parse-buffer))
          "Installing documentation..."
        (with-temp-file (expand-file-name
                         (url-hexify-string (format "%s.html" (car entry))) temp)
          (push (symbol-name (car entry)) pages)
          (insert (cdr entry)))))
    (with-temp-buffer
      (url-insert-file-contents (format "%s/%s/index.json?%s" devdocs-cdn-url slug mtime))
      (let ((index (devdocs--json-parse-buffer)))
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

(defun devdocs--index (doc kind)
  "Return an index of document DOC, where KIND is `entries', `pages' or `types'."
  (if kind
      (alist-get kind (devdocs--with-cache (devdocs--index doc nil)))
    (let* ((docmeta (cons 'doc doc))
           (indexes (with-temp-buffer
                      (insert-file-contents (expand-file-name
                                             (concat (alist-get 'slug doc) "/index")
                                             devdocs-data-dir))
                      (read (current-buffer))))
           (entries (alist-get 'entries indexes)))
      (prog1 indexes
        (seq-do-indexed (lambda (entry i) (aset entries i (cons docmeta entry)))
                        entries)))))

;;; Documentation viewer

(defvar-local devdocs--stack nil
  "List of viewed entries, set buffer-locally when in `devdocs-mode'.")

(defvar-local devdocs--forward-stack nil
  "List of viewed entries for `devdocs-go-forward'.")

(defvar devdocs-header-line
  '(:eval (let-alist (car devdocs--stack)
            (concat (devdocs--doc-title .doc)
                    (and .type devdocs-separator) .type
                    devdocs-separator (or .name .path)))))

(define-derived-mode devdocs-mode special-mode "DevDocs"
  "Major mode for viewing DevDocs documents."
  :interactive nil
  (if (boundp 'browse-url-handlers) ;; Emacs ≥ 28
      (setq-local browse-url-handlers
                  `((devdocs--internal-url-p . devdocs--internal-url-handler)
                    ,@browse-url-handlers))
    (setq-local browse-url-browser-function
                `(("\\`[^:]+\\'" . devdocs--internal-url-handler)
                  ,@(if (functionp browse-url-browser-function)
                        `(("" . ,browse-url-browser-function))
                      browse-url-browser-function))))
  (setq-local
   buffer-undo-list t
   header-line-format devdocs-header-line
   revert-buffer-function #'devdocs--revert-buffer
   truncate-lines t))

(defun devdocs-goto-target ()
  "Go to the original position in a DevDocs buffer."
  (interactive)
  (goto-char (point-min))
  (when-let ((frag (let-alist (car devdocs--stack)
                     (or .fragment (devdocs--path-fragment .path))))
             (shr-target-id (url-unhex-string frag))
             (pred (if (fboundp 'shr--set-target-ids) #'member t)) ;; shr change in Emacs 29
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
    (let* ((entries (devdocs--index .doc 'entries))
           (pred (lambda (entry _) (string= (alist-get 'path entry) .path)))
           (current (seq-position entries nil pred)))
      (unless current (user-error "No current entry"))
      (devdocs--render
       (or (ignore-error args-out-of-range (elt entries (+ count current)))
           (user-error "No %s entry" (if (< count 0) "previous" "next")))))))

(defun devdocs-previous-entry (count)
  "Go backward COUNT entries in this document."
  (interactive "p")
  (devdocs-next-entry (- count)))

(defun devdocs-goto-page (doc page)
  "Go to a given PAGE (a number or path) of DOC.
Interactively, read a page name with completion."
  (interactive (let-alist (car devdocs--stack)
                 (list .doc (completing-read "Go to page: "
                                             (append (devdocs--index .doc 'pages) nil)
                                             nil t nil 'devdocs-history))))
  (let* ((path (cond ((stringp page) page)
                     ((numberp page) (elt (devdocs--index doc 'pages) page))))
         (entry (or (seq-find (lambda (entry) (string= (alist-get 'path entry) path))
                              (devdocs--index doc 'entries))
                    `((doc . ,doc) (path . ,path)))))
    (devdocs--render entry)))

(defun devdocs-first-page (doc)
  "Go to first page of DOC."
  (interactive (list (alist-get 'doc (car devdocs--stack))))
  (devdocs-goto-page doc 0))

(defun devdocs-last-page (doc)
  "Go to last page of DOC."
  (interactive (list (alist-get 'doc (car devdocs--stack))))
  (devdocs-goto-page doc (1- (length (devdocs--index doc 'pages)))))

(defun devdocs-next-page (count)
  "Go forward COUNT pages in this document."
  (interactive "p")
  (let-alist (car devdocs--stack)
    (let* ((pages (devdocs--index .doc 'pages))
           (dest (+ count (seq-position pages (devdocs--path-file .path)))))
      (cond ((< dest 0) (user-error "No previous page"))
            ((<= (length pages) dest) (user-error "No next page")))
      (devdocs-goto-page .doc dest))))

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

(let ((map devdocs-mode-map))
  (define-key map [tab] #'forward-button)
  (define-key map [backtab] #'backward-button)
  (define-key map "d" #'devdocs-peruse)
  (define-key map "i" #'devdocs-lookup)
  (define-key map "p" #'devdocs-previous-entry)
  (define-key map "n" #'devdocs-next-entry)
  (define-key map "g" #'devdocs-goto-page)
  (define-key map "[" #'devdocs-previous-page)
  (define-key map "]" #'devdocs-next-page)
  (define-key map "<" #'devdocs-first-page)
  (define-key map ">" #'devdocs-last-page)
  (define-key map "l" #'devdocs-go-back)
  (define-key map "r" #'devdocs-go-forward)
  (define-key map "w" #'devdocs-copy-url)
  (define-key map "." #'devdocs-goto-target))

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
    (_ (string-remove-prefix
        "/"
        (url-expander-remove-relative-links ;; undocumented function!
         (concat (file-name-directory base) path))))))

(defun devdocs--shr-tag-pre (dom)
  "Insert and fontify pre-tag represented by DOM."
  (let ((start (point)))
    (if-let ((lang (and devdocs-fontify-code-blocks
                        (dom-attr dom 'data-language)))
             (mode (or (cdr (assoc lang '(("cpp" . c++-mode)
                                          ("shell" . sh-mode))))
                       (intern (concat lang "-mode"))))
             (buffer (and (fboundp mode) (current-buffer))))
        (insert
         (with-temp-buffer
           (shr-tag-pre dom)
           (let ((inhibit-message t)
	         (message-log-max nil))
             (ignore-errors (delay-mode-hooks (funcall mode)))
             (font-lock-ensure))
           (buffer-string)))
      (shr-tag-pre dom))
    (add-face-text-property start (point) 'devdocs-code-block t)))

(defun devdocs--render (entry)
  "Render a DevDocs documentation entry, returning a buffer.

ENTRY is an alist like those in the entry index of the document,
possibly with an additional ENTRY.fragment which overrides the
fragment part of ENTRY.path."
  (with-current-buffer (get-buffer-create "*devdocs*")
    (unless (eq major-mode 'devdocs-mode)
      (devdocs-mode))
    (let-alist entry
      (let ((inhibit-read-only t)
            (shr-external-rendering-functions `((pre . devdocs--shr-tag-pre)
                                                ,@shr-external-rendering-functions))
            (file (expand-file-name (format "%s/%s.html"
                                            .doc.slug
                                            (url-hexify-string (devdocs--path-file .path)))
                                    devdocs-data-dir)))
        (erase-buffer)
        ;; TODO: cl-progv here for shr settings?
        (shr-insert-document
         (with-temp-buffer
           (insert-file-contents file)
           (libxml-parse-html-region (point-min) (point-max)))))
      (set-buffer-modified-p nil)
      (setq-local devdocs-current-docs (list .doc.slug))
      (push entry devdocs--stack)
      (setq-local list-buffers-directory (format-mode-line devdocs-header-line
                                                           nil nil
                                                           (current-buffer)))
      (devdocs-goto-target)
      (current-buffer))))

(defun devdocs--revert-buffer (&rest _args)
  "Refresh DevDocs buffer."
  (devdocs--render (pop devdocs--stack)))

(defun devdocs--internal-url-p (url)
  "Return t if URL seems to be an internal DevDocs link."
  (not (string-match-p "\\`[a-z]+:" url)))

(defun devdocs--internal-url-handler (url &rest _)
  "Open URL of an internal link in a DevDocs document."
  (let-alist (car devdocs--stack)
    (let* ((dest (devdocs--path-expand url .path))
           (file (devdocs--path-file dest))
           (frag (devdocs--path-fragment dest))
           (entry (seq-find (lambda (it)
                              (let-alist it
                                (or (string= .path dest)
                                    (string= .path file))))
                            (devdocs--index .doc 'entries))))
      (unless entry (error "Can't find `%s'" dest))
      (when frag (push `(fragment . ,frag) entry))
      (devdocs--render entry))))

;;; Lookup commands

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
                                    (devdocs--index doc 'entries)))
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
May ask interactively for the desired documents, remembering the
choice for this buffer.  If ASK is non-nil, ask unconditionally."
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
    (devdocs--get-data (or (car (member cand cands))
                           (user-error "Not an entry!")))))

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
         (buffer (devdocs--render entry))
         (window (display-buffer buffer)))
    (when window
      (with-selected-window window
        (devdocs-goto-target)
        (recenter 0))
      (when devdocs-window-select
        (select-window window)))))

;;;###autoload
(defun devdocs-peruse (doc)
  "Read a document from the first page."
  (interactive (list (devdocs--read-document "Peruse documentation: ")))
  (pop-to-buffer (devdocs-goto-page doc 0)))

;; Don't show devdocs-mode specific commands in M-x
(dolist (sym '(devdocs-goto-target devdocs-go-back devdocs-go-forward
               devdocs-next-entry devdocs-previous-entry devdocs-goto-page
               devdocs-first-page devdocs-last-page devdocs-next-page
               devdocs-previous-page devdocs-copy-url))
  (put sym 'completion-predicate (lambda (_ buffer)
                                   (provided-mode-derived-p
                                    (buffer-local-value 'major-mode buffer)
                                    'devdocs-mode))))

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
