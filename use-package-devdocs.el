;;; use-package-devdocs.el --- Emacs viewer for DevDocs -*- lexical-binding: t -*-

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
(require 'use-package-core)

;;;###autoload
(defun use-package-normalize/:devdocs (name-symbol keyword arg)
  "Normalize use-package customize keyword."
  (let ((error-msg (format  "%s :%s wants a \"docset\" definition or (\"docset\" \"docset\" ...) list or a (mode-name \"docset\" \"docset\" ...) list or a list of these." name-symbol keyword)))
    (unless (listp arg)
      (use-package-error error-msg))
    (seq-map
     (lambda (def)
       (cond ((stringp def) (cons name-symbol arg))
             ((listp def) (if (symbolp (car def)) def
                            (cons name-symbol def)))
             (t (use-package-error error-msg))))
     arg)))

;;;###autoload
(defun use-package-handler/:devdocs (name keyword args rest state)
  "Generate use-package customize keyword code."
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (seq-map (lambda (def)
                (let ((mode (intern (format "%s-hook" (car def))))
                      (hook (intern (format "use-package-devdocs-setup-%s" (car def))))
                      (docsets (cdr def)))
                  `(progn
                     ;; install docs
                     (seq-do (lambda (docset)
                               (devdocs-ensure-installed docset)) (quote ,docsets))
                     ;; configure docs for given mode
                     (defun ,hook ,()
                       (unless (boundp 'devdocs-current-docs)
                         (setq-local devdocs-current-docs nil))
                       (seq-do (lambda (docset)
                                 (add-to-list 'devdocs-current-docs docset))
                               (quote ,docsets)))
                     (add-hook (quote ,mode) (function ,hook)))))
              args)
     body)))

(add-to-list 'use-package-keywords :devdocs t)

(provide 'use-package-devdocs)
;;; use-package-devdocs.el ends here
