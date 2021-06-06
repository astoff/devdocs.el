devdocs.el — Emacs viewer for DevDocs
=====================================

<a href="http://elpa.gnu.org/packages/devdocs.html"><img alt="GNU ELPA" src="https://elpa.gnu.org/packages/devdocs.svg"/></a>
<a href="https://melpa.org/#/devdocs"><img alt="MELPA" src="https://melpa.org/packages/devdocs-badge.svg"/></a>

devdocs.el is a documentation viewer similar to the built-in Info
browser, but geared towards documentation obtained from the [DevDocs]
website.  The stable version is available from [GNU ELPA] and a
development version is available from [MELPA]; to install, type `M-x
package-install RET devdocs RET`.

To get started, download some documentation with `M-x devdocs-install`.
This will first query https://devdocs.io for the available documents,
and save to disk the selected document.

Once you have the desired documents at hand, call `M-x devdocs-lookup`
to search for entries.

In any given buffer, the first call to `devdocs-lookup` will query for
a list of documents to search (you can select more than one option by
entering a comma-separated list).  This selection will be remembered
in subsequent calls to `devdocs-lookup`, unless a prefix argument is
given; in this case you can select a new list of documents.
Alternatively, you can set the `devdocs-current-docs` variable
directly, say via [dir-local variables] or a mode hook:

```elisp
(add-hook 'python-mode-hook
          (lambda () (setq-local devdocs-current-docs '("python~3.9"))))
```

In the `*devdocs*` buffer, navigation keys similar to Info and
`*Help*` buffers are available; press `C-h m` for details.  Internal
hyperlinks are opened in the same viewing buffer, and external links
are opened as `browse-url` normally would.

[DevDocs]: https://devdocs.io
[GNU ELPA]: https://elpa.gnu.org/packages/devdocs.html
[MELPA]: https://melpa.org/#/devdocs
[dir-local variables]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
