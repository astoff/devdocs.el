devdocs.el — Emacs viewer for DevDocs
=====================================

devdocs.el is a documentation viewer similar to the built-in Info
browser, but geared towards documentation obtained from the [DevDocs]
website.

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
(with-eval-after-load 'evdocs
  (add-hook 'python-mode-hook
            (lambda () (add-to-list 'evdocs-current-docs "python~3.9"))))
```

In the `*devdocs*` buffer, navigation keys similar to Info and
`*Help*` buffers are available; press `C-h m` for details.  Internal
hyperlinks are opened in the same viewing buffer, and external links
are opened as `browse-url` normally would.

[DevDocs]: https://devdocs.io
[dir-local variables]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
