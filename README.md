# emacs-universal-sidecar

A universal "sidecar" buffer for emacs in the spirit of the `org-roam-mode` buffer.

## Using Org-Roam Buffer Sections

The additional file `roam-sidecar.el` can be used to show sections from the `org-roam-mode` buffer in `universal-sidecar`.
This can be done either through manual use of the `universal-sidecar-roam-section` function, or through taking an existing configuration (`org-roam-mode-sections`).

To use `universal-sidecar-roam-section`, a minimum configuration is:

```elisp
(add-to-list 'universal-sidecar-sections '(universal-sidecar-roam-section org-roam-backlinks-section))
```

Note, that if you would pass arguments to the normal org-roam section, you may do so after the section name in `universal-sidecar-org-roam-section`.

Finally, your sections can be added en-masse with:

```elisp
(setq universal-sidecar-sections
      (universal-sidecar-convert-roam-sections org-roam-mode-sections))
```

