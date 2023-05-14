# emacs-universal-sidecar

A universal "sidecar" buffer for emacs in the spirit of the `org-roam-mode` buffer.

## Installation

This package has one main requirement: `magit`, for `magit-section`.
Assuming this package is satisfied, the `universal-sidecar.el` file may be placed on the load path and `require`d.

## Usage

The `universal-sidecar-toggle` command will bring up a per-frame "sidecar" buffer.
These sidecar buffers are used to show information about or related to the focused buffer.
Information is shown in *sections*, which are configured using the `universal-sidecar-sections` variable.
The behavior of this variable, and expected interface is described below in configuration.

Additionally, to make sure that the sidecar buffer is updated, it's necessary to advise several functions.
This can be done automatically using the `universal-sidecar-update-insinuate` function, which will advise functions listed in `universal-sidecar-insinuate-commands`.
This may be undone with `universal-sidecar-unadvise-commands`.

### Configuration

The name of the sidecar buffer is configured using `universal-sidecar-buffer-name-format`, which must contain `%F`, a representation of the description of the frame.

Which sections are shown is configured using `universal-sidecar-sections`, which is a list of functions or functions-with-arguments.
For example, let's consider the section `buffer-git-status`, which shows git status.
This section allows a keyword argument, `:show-renames`, which defaults to t.
If we want the default behavior, we would configure it using

```elisp
(add-to-list 'universal-sidecar-sections 'buffer-git-status)
```

However, if we want the opposite behavior (don't show renames), we'd configure it as shown below.

```elisp
(add-to-list 'universal-sidecar-sections '(buffer-git-status :show-renames t))
```

Note that using `add-to-list` is generally bad practice, as the sections will be run in the order they're present in the list.

Finally, sidecar buffers are displayed using `display-window`.
This means that how the buffer is displayed is easily configurable from `display-buffer-alist`.
The author's configuration is shown below as an example.
In particular, using the `display-buffer-in-side-window` display action is suggested, as it's generally not helpful to select the sidecar window through normal window motion commands

```elisp
(add-to-list 'display-buffer-alist
             '("\\*sidecar\\*"
               (display-buffer-in-side-window)
               (slot . 0)
               (window-width . 0.2)
               (window-height . 0.2)
               (preserve-size t . t)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))
```

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

