# ROOT-mode

ROOT-mode is an Emacs major-mode for interacting with ROOT
(https://root.cern/). Specifically, this package provides the
facilities to run the ROOT command line interface/REPL.

## Installation & Configuration

The package is not on MELPA, so you can either clone the repository
and manually load the file in Emacs, or use something like
use-package/straight. Here is one example of installing/configuring
the package with straight.el:

```lisp
(use-package root-mode
  :straight (root-mode :type git :host github :repo "jaypmorgan/root-mode")
  :config
  (setq root-filepath "/path/to/root"))
```

There are only a few variables provided by the package:

- **root-filepath** -- this is the absolute/relative path to the root
  executable. This could be left as the default "root" value if ROOT
  is available on your $PATH.
- **root-command-option** -- these are the command line options used
  when starting ROOT. By default there are no options.
- **root-prompt-regex** -- this is the regular expression used to find
  the input prompt of the ROOT REPL. If there is a customised prompt,
  this will need to be updated to suit the customisation.
  
## Using the package

Below is listed the various commands provided by the package.

| Command    | Description                             |
|:-----------|:----------------------------------------|
| `run-root` | Start the ROOT REPL in the same window. |
