# emacs-universal-sidecar [![MELPA](https://melpa.org/packages/universal-sidecar-badge.svg)](https://melpa.org/#/universal-sidecar) [![builds.sr.ht status](https://builds.sr.ht/~swflint/emacs-universal-sidecar.svg)](https://builds.sr.ht/~swflint/emacs-universal-sidecar?)

A universal "sidecar" buffer for emacs in the spirit of the `org-roam-mode` buffer.

![Logo](images/logo.png)

## Bug Reports and Patches

If you find an error or have a patch to improve these packages, please send an email to `~swflint/emacs-utilities@lists.sr.ht`.

## Installation

This package has one main requirement: `magit`, for `magit-section`.
Assuming this package is satisfied, the `universal-sidecar.el` file may be placed on the load path and `require`d.

## Usage

The `universal-sidecar-toggle` command will bring up a per-frame "sidecar" buffer.
These sidecar buffers are used to show information about or related to the focused buffer.
Information is shown in *sections*, which are configured using the `universal-sidecar-sections` variable.
The behavior of this variable, and expected interface is described below in configuration.

Additionally, to make sure that the sidecar buffer is updated, it's necessary to advise several functions.
This can be done automatically using the `universal-sidecar-insinuate` function, which will advise functions listed in `universal-sidecar-advise-commands`.
This may be undone with `universal-sidecar-uninsinuate`.
Additionally, `universal-sidecar-insinuate` will add `universal-sidecar-refresh` to the `focus-in-hook`, and will set an idle timer to refresh all sidecar buffers (idle time configured with `universal-sidecar-refresh-time`).
Buffers can be ignored by modifying the `universal-sidecar-ignore-buffer-regexp`, or using the (irregular) `universal-sidecar-ignore-buffer-functions` hook.
This hook will be run with an argument (the buffer) and run until a non-nil result.

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

Next, the displayed buffer name is generated using `universal-sidecar-buffer-id-format` and `universal-sidecar-buffer-id-formatters`.
These may be customized to your liking.
Note: `universal-sidecar-buffer-id-formatters` is an alist of character/function pairs.
The functions should take as their first (and only mandatory) argument the buffer for which the sidecar is being displayed.

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

Finally, errors in sections or section definitions are by default logged to the `*Warnings*` buffer.
This is done in a way to allow for debugging.
Moreover, the logging can be disabled by setting `universal-sidecar-inhibit-section-error-log` to non-nil, in which case (unless debugging is enabled) these errors will be ignored.

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

Finally, section text can be formatted and fontified as if it was in some other mode, for instance, `org-mode` using `universal-sidecar-fontify-as`.
An example is shown below.

```elisp
(universal-sidecar-fontify-as org-mode ((org-fold-core-style 'overlays))
  (some-function-that-generates-org-text)
  (some-post-processing-of-org-text))
```

## Including Citation Data

The `universal-sidecar-citeproc` library simplifies the use of `citeproc.el` to format citation data.
To this end, the `universal-cidecar-citeproc` customization group allows citeproc-related paths to be set once and easily reused (in particular, it is important to remind users to set the `universal-sidecar-citeproc-locales` and `universal-sidecar-citeproc-styles` variables).
Additionally, it provides a single place for users to set their default preferred citation style (as an absolute path, or relative to `universal-sidecar-citeproc-styles`).

Users are encouraged to customize in the following pattern.

```elisp
(setopt universal-sidecar-citeproc-locales "~/citeproc/locales/"
        universal-sidecar-citeproc-styles "~/citeproc/styles/"
        universal-sidecar-citeproc-default-style "ieee.csl")
```

For developers, there are two main functions, `universal-sidecar-citeproc-get-processor` and `universal-sidecar-citeproc-org-output`.
The former will create a citation processor (see `citeproc` library documentation) from a list of data sources, and style/locale information.
The latter will take a processor and generate an `org-mode` formatted (and fontified) string for insertion into a sidecar.
These may be used as follows:

```elisp
(defun demo-citeproc-formatter (keys bibliography)
  (let ((processor (universal-sidecar-citeproc-get-processor bibliography)))
    (citeproc-add-uncited keys processor)
    (universal-sidecar-citeproc-org-output processor)))
```

## Using Org-Roam Buffer Sections [![MELPA](https://melpa.org/packages/universal-sidecar-roam-badge.svg)](https://melpa.org/#/universal-sidecar-roam)

The additional file `universal-sidecar-roam.el` can be used to show sections from the `org-roam-mode` buffer in `universal-sidecar`.
This can be done either through manual use of the `universal-sidecar-roam-section` function, or through taking an existing configuration (`org-roam-mode-sections`).

To use `universal-sidecar-roam-section`, a minimum configuration is:

```elisp
(add-to-list 'universal-sidecar-sections '(universal-sidecar-roam-section org-roam-backlinks-section))
```

Note, that if you would pass arguments to the normal org-roam section, you may do so after the section name in `universal-sidecar-org-roam-section`.

Finally, your sections can be added en-masse with:

```elisp
(setq universal-sidecar-sections
      (universal-sidecar-roam-convert-roam-sections org-roam-mode-sections))
```

Additionally, the `universal-sidecar-buffer-id-formatters` variable can have a "node title or buffer name" formatter, using the `universal-sidecar-roam-buffer-name` function.

## Org Appointments Section [![MELPA](https://melpa.org/packages/org-appointments-sidecar-badge.svg)](https://melpa.org/#/org-appointments-sidecar)

This package displays upcoming appointments/events from the Org Agenda in the Universal Sidecar.
It's based on [`org-upcoming-modeline`](https://github.com/unhammer/org-upcoming-modeline/) but allows the display of multiple upcoming (and active) events.
Basic configuration is as simple as adding `org-appointments-sidecar` to `universal-sidecar-sections` at the appropriate spot.

More advanced customization is available as follows:

 - To ignore events based on their TODO states or tags, customize the variables `org-appointments-sidecar-ignored-states` and `org-appointments-ignored-tags`, respectively.
 These are simply lists of ignored states/tags.
 - The duration of the considered period is controlled by `org-appointments-sidecar-look-back` and `org-appointments-sidecar-look-ahead` which define the start and end of the period relative to NOW respectively.
 They are cons cells of the form (DURATION . UNIT), where DURATION is an integer, and UNIT is a symbol in (second minute hour day).
 - Display of events is controlled by `org-appointments-sidecar-format-function`, the default of which is `org-appointments-sidecar-format-appointment`.
 In general, this function should take an `org-element` heading, and return org-formatted text.
 For further customization (ex, developing ones own formatting function), see the docstring.
 - If using the default format function, the variable `org-appointments-sidecar-show-properties` is used to select and display various properties from the heading.
 It should be a list of either strings naming properties, or cons cells of property name/display name pairs.

## Elfeed Related Papers Sections [![MELPA](https://melpa.org/packages/universal-sidecar-elfeed-related-badge.svg)](https://melpa.org/#/universal-sidecar-elfeed-related)

The usecase that started this project: I wanted to be able to see possibly related papers that I've read when I read through the ArXiv RSS feeds.
I initially wrote a basic command which could be run manually for each Elfeed article, yet this is somewhat painful.
Thus came `universal-sidecar`, and this particular sidecar section.
This extension is fairly simple, and builds on top of the `bibtex-completion` library, so it's necessary to configure it appropriately.
Once `bibtex-completion` is configured and the `universal-sidecar-elfeed-related-section` is added to `universal-sidecar-sections`, if an article's authors have other works in your Bibtex databases, they will be shown.
Note, however, that as of now, search is only by author last name.

## Elfeed Score Section [![MELPA](https://melpa.org/packages/universal-sidecar-elfeed-score-badge.svg)](https://melpa.org/#/universal-sidecar-elfeed-score)

The section `universal-sidecar-elfeed-score-section` shows the score of the currently shown elfeed entry and why it is scored that way.

## Org Mode Citations Section [![MELPA](https://melpa.org/packages/org-cite-sidecar-badge.svg)](https://melpa.org/#/org-cite-sidecar)

**Notice:** This package is now deprectated and will be removed from MELPA around 18 March 2024.
Please use [`org-cite-overlay-sidecar`](https://git.sr.ht/~swflint/org-cite-overlay) in its place, which does roughly the same thing, but in a more flexible way, and can utilize information from the `org-cite-overlay` package (same URL).

This package can be used to show an Org-Mode document's citations in `universal-sidecar`.
This is done through the use of `citeproc` and can be shown quite flexibly.
A minimum configuration is as follows:

```elisp
(setq universal-sidecar-citeproc-locales "~/.emacs.d/csl-data/locales/"
      ;; set to your directories for locale and style data
      universal-sidecar-citeproc-styles "~/.emacs.d/csl-data/styles/")
(add-to-list 'universal-sidecar-sections 'org-cite-sidecar)
```

It is important to set the `universal-sidecar-citeproc-locales` and `universal-sidecar-citeproc-styles` variables to the directories where you have cloned the CSL locale and style data repositories (see docstrings for links).

Additionally, there are two arguments to the section which are not exposed as customization variables:

 - `:style` allows you to select a prefered CSL style within `universal-sidecar-citeproc-styles`.
 Default is `universal-sidecar-citeproc-default-style`.
- `:header` allows you to change the header of the section from the default "References".

## Ebib Formatted Reference Section [![MELPA](https://melpa.org/packages/ebib-sidecar-badge.svg)](https://melpa.org/#/ebib-sidecar)

This package can be used to show a formatted reference to the bib entry at point in `ebib`.
This is done using the `citeproc` library and can be flexibly configured.
Handling of locale and style management is performed using `universal-sidecar-citeproc`, and its variables (`universal-sidecar-citeproc-styles` and `universal-sidecar-citeproc-locales`) must be configured.

A minimum configuration is shown as follows:

```elisp
(setopt universal-sidecar-citeproc-locales "~/.emacs.d/csl-data/locales/" ;set to your directories for locale and style data
        universal-sidecar-citeproc-styles "~/.emacs.d/csl-data/styles/")
(add-to-list 'universal-sidecar-sections 'ebib-sidecar)
```

Additionally, there are two arguments to the section which are not exposed as customization variables:

- `:style` allows you to select a prefered CSL style to override `universal-sidecar-citeproc-default-style`.
- `:header` allows you to change the header of the section from the default "References".

Finally, update based on ebib motion commands can be enabled either manually (using `universal-sidecar-advise-commands`) or automatically by also adding `ebib-sidecar-insinuate` somewhere in your init file.

## EBDB Mua Sidecar [![MELPA](https://melpa.org/packages/ebdb-mua-sidecar-badge.svg)](https://melpa.org/#/ebdb-mua-sidecar)

[EBDB](https://github.com/girzel/ebdb/) records for MUA buffers can be displayed using Universal Sidecar.
As of now, it only supports `message-mode`, `gnus-article-mode` and `gnus-summary-mode` (patches welcome to support others, though I suspect it's just the major mode names that need set).
To use this section, add `ebdb-mua-sidecar` to your `universal-sidecar-sections`, and have `ebdb-mua-sidecar-insinuate` run somewhere in your init.

The following options are available:

- `:header` allows you to change the header of the section from the default `Addressees:`.
- `:updatep` (default `'existing`), determines how records are updated (see also `ebdb-update-records`).
- `:formatter` (default `ebdb-default-multiline-formatter`) determines how records are formatted.
  This should be an instance of `ebdb-record-formatter`.

## Wordcount Section [![MELPA](https://melpa.org/packages/wordcount-section-badge.svg)](https://melpa.org/#/wordcount-section)

Word count information for a buffer can be shown using `wordcount-section`.
Basic installation is as follows.

```elisp
(add-to-list 'universal-sidecar-sections #'wordcount-section)
```

The wordcount display can be customized in three ways:

 - The `:header` keyword argument can be used to change the section header.
 - The format of word count information is controlled by the variable `wordcount-section-format`, which supports the following percent escapes:
| %w | Word Count      |
| %s | Sentence Count  |
| %l | Line Count      |
| %c | Character Count |
 - The `wordcount-section-modes` variable controls the modes that have wordcounts shown. 
   This list is passed to `derived-mode-p` in the target buffer.

## Basic and Example Sections

These are found in `universal-sidecar-sections.el`.

### Buffer Tail

`(universal-sidecar-tail-buffer-section :show-buffer :n-lines :title)`

Show the last `n-lines` of `show-buffer` in the sidecar with `title`.
If `show-buffer` doesn't exist, or is empty, no section will be shown.
Additionally, `show-buffer` can be a buffer object, a string buffer name, a symbol whose value is a buffer, or a zero-argument function which will be called to get a buffer.

### Org Clock Section

`universal-sidecar-org-buffer-clock-section`

Show a display of current org-mode clocking data.
In particular, the total time clocked in for today, the time since starting the task today, and the total time spent on the currently-clocked task (as well as the currently clocked task) are shown.

If you use this, it is helpful to include `(universal-sidecar-org-clock-insinuate)` somewhere in your config.
