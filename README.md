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

## Configuration

There are two main direct configuration points: the buffer name format, and the sections list.

The buffer name format is what allows multiple sidecar buffers to be shown, presently, one per frame.
This is a format string which must contain `%F`.

The sections list, on the otherhand, is more complex.
This is how you determine what is shown in a sidecar for a buffer.
Generally speaking, this is a list of functions which take a minimum of two arguments: the buffer and the sidecar.
Using information in the buffer, the sidecar will be populated.
Each section function will be run in the order of the list: it's important to carefully consider what order you put these in.
More about defining sections is written below.

Additionally, the sidecar buffer is shown using `display-buffer`.
The author's recommended configuration is shown below.

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

Finally, advice is used to help ensure that the sidecar buffer gets updated appropriately.
This can be done semi-automatically using the `universal-sidecar-update-insinuate` function.
This will automatically advise functions in the `universal-sidecar-insinuate-commands` list with after advice, which calls `universal-sidecar-render`.

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

