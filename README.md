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

## Section Functions

The basic installation of `universal-sidecar` does not include any section functions.
This is to reduce the number of dependencies of the package itself so that it may be used as a library for others, or to help integrate multiple packages.
A `universal-sidecar-sections.el` package is available as well, which will have simple section definitions that may be of use.

However, implementation of functions is generally straight-forward.
First, sections are simply functions which take a minimum of two arguments, `buffer`, or the buffer we're generating a sidecar for, and `sidecar`, the sidecar buffer.
When writing these section functions, it is recommended to avoid writing content to `sidecar` until it's verified that the information needed is available.
That is **don't write sections without bodies**.

To aid in defining sections, the `universal-sidecar-define-section` and `universal-sidecar-insert-section` macros are available.
The first defines a section which can be added to `universal-sidecar-sections`.
The second simplifies writing sections by adding proper separators and headers to the sidecar buffer.
We will demonstrate both below.

```elisp
(universal-sidecar-define-section fortune-section (file title)
                                  (:major-modes org-mode
                                   :predicate (not (buffer-modified-p)))
  (let ((title (or title
                   (and file
                        (format "Fortune: %s" file))
                   "Fortune"))
        (fortune (shell-command-to-string (format "fortune%s"
                                                  (if file
                                                      (format " %s" file)
                                                    "")))))
    (universal-sidecar-insert-section fortune-section title
      (insert fortune))))
```

Note: the arguments (`file` and  `title`) are *keyword* arguments.
Additionally, you specify that this section only applies when `buffer` is a descendent of `:major-modes` which can be either a symbol or a list of symbols.
`:predicate` is used to specify a somewhat more complex predicate to determine if the section should be generated.

This section could be added in any of the following ways:

```elisp
(add-to-list 'universal-sidecar-sections 'fortune-section)
(add-to-list 'universal-sidecar-sections '(fortune-section :file "definitions"))
(add-to-list 'universal-sidecar-sections '(fortune-section :title "O Fortuna!"))
(add-to-list 'universal-sidecar-sections '(fortune-section :file "definitions" :title "Random Definition"))
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

## Elfeed Related Papers Sections

The usecase that started this project: I wanted to be able to see possibly related papers that I've read when I read through the ArXiv RSS feeds.
I initially wrote a basic command which could be run manually for each Elfeed article, yet this is somewhat painful.
Thus came `universal-sidecar`, and this particular sidecar section.
This extension is fairly simple, and builds on top of the `bibtex-completion` library, so it's necessary to configure it appropriately.
Once `bibtex-completion` is configured and the `elfeed-related-works-section` is added to `universal-sidecar-sections`, if an article's authors have other works in your Bibtex databases, they will be shown.
Note, however, that as of now, search is only by author last name.

## Elfeed Score Section

The section `elfeed-score-section` shows the score of the currently shown elfeed entry and why it is scored that way.
