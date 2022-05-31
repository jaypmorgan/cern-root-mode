# CERN-ROOT-mode

[![MELPA](https://melpa.org/packages/cern-root-mode-badge.svg)](https://melpa.org/#/cern-root-mode)

CERN-ROOT-mode is an Emacs major-mode for interacting with ROOT
(https://root.cern/). Specifically, this package provides the
facilities to run the ROOT command line interface/REPL.

![Example demonstration](./docs/images/brief-example.gif)

## Installation & Configuration

You can install the package from
[melpa](https://melpa.org/#/cern-root-mode), or use straight.el and
download the package directly from this github repository. Here is one
example of installing/configuring the package with straight.el:

```lisp
(use-package cern-root-mode
  :bind (:map c++-mode-map
	     (("C-c C-c" . cern-root-eval-defun)
	      ("C-c C-b" . cern-root-eval-buffer)
	      ("C-c C-l" . cern-root-eval-file)
	      ("C-c C-r" . cern-root-eval-region)))
  :straight (cern-root-mode :type git :host github :repo "jaypmorgan/cern-root-mode")
  :config
  (setq cern-root-filepath "/path/to/root"))
```

There are only a few variables provided by the package:

- **cern-root-filepath** -- (string, default `"root"`) this is the
  absolute/relative path to the root executable. This could be left as
  the default "root" value if ROOT is available on your $PATH.
- **cern-root-command-option** -- (string, default `""`) these are the
  command line options used when starting ROOT. By default there are
  no options.
- **cern-root-prompt-regex** -- (string, default `"^\\[[0-9;^k]+m?"`) this
  is the regular expression used to find the input prompt of the ROOT
  REPL. If there is a customised prompt, this will need to be updated
  to suit the customisation.
- **cern-root-buffer-name** -- (string, default `"*ROOT*"`) what to call
  the ROOT repl buffer when it starts. This could be left as the
  default `*ROOT*`, but its there for you to modify as you please.
- **cern-root-terminal-backend** -- (symbol, default `'terminal`) the
  terminal emulator to run the ROOT instance in. There are only two
  current defined [`vterm`, `inferior`]. By default, the terminal
  backend is set to `inferior`. Though, there is still some issues in
  getting the auto-complete to work correctly. If you really need
  auto-complete, I would recommend switching the backend to `'vterm`
  (requires you've already installed vterm).
  
## Using the package

Below is listed the various commands provided by the package.

| Command                         | Description                                                                                                        |
|:--------------------------------|:-------------------------------------------------------------------------------------------------------------------|
| `cern-root-run`                      | Start the ROOT REPL in the same window.                                                                            |
| `cern-root-run-other-window`         | Start the ROOT REPL in a different window.                                                                         |
| `cern-root-switch-to-repl`           | Move the cursor to the ROOT REPL.                                                                                  |
| `cern-root-eval-region`              | Evaluate a marked region in ROOT.                                                                                  |
| `cern-root-eval-line`                | Evaluate this line in the REPL.                                                                                    |
| `cern-root-eval-string`              | (non-interactive function) Evaluate a string in the REPL.                                                          |
| `cern-root-eval-defun`               | Evaluate the current function in ROOT (current as defined by cursor position).                                     |
| `cern-root-eval-defun-maybe`         | If the cursor is within a function declaration, then evaluate this function, else we'll evaluate this single line. |
| `cern-root-eval-buffer`              | Evaluate the current buffer in ROOT.                                                                               |
| `cern-root-eval-file`                | Evaluate a file (using the '.L' syntax), prompt for a file.                                                        |
| `cern-root-change-working-directory` | Change the working directory of the root session, prompt for a directory.                                          |
| `cern-root-list-input-history`       | List the previously input statements and for selection.                                                            |

## Org-mode

CERN-ROOT-mode provides the functionality to execute C++ source code blocks
using the ROOT REPL instead of the default C++ executable. This can
make the process of writing C++ in org-mode more fluid.

![Example demonstration of evaluating in org-mode](./docs/images/org-mode-example.gif)

To write C++ code blocks that then are executed using ROOT, specify
`cern-root` as the language in the `begin_src` header. For example:

```
#+begin_src cern-root
// write your code here.
#+end_src
```

Upon executing this code block (such as with `C-c C-c`), this code
will be executed in the ROOT REPL instead.

Code can be written and re-used over multiple code blocks with the use
of the `:session` argument (with an optional name). By providing this
argument, a ROOT REPL instance will be created that can be used within
the context of many source blocks. For example, let's create another
code block with this session argument:

```
#+begin_src cern-root :session *my-root-session*
void test() {
	// print something
	printf("This is something");
}
#+end_src
```

When we first run this code block, CERN-ROOT-mode will create a new
instance of the ROOT REPL in the buffer named '*my-root-session*' (you
can switch to this buffer if you want to interact directly outside of
the code blocks!). This means that, by specifying the same argument to
a different code block, we can call this `test` function.

```
#+begin_src cern-root :session *my-root-session*
test();  // call the test function
#+end_src
```

This gets us a long way to achieving the same functionality as `root
--notebook`, but in Emacs.

## Running tests

To test the functionality of the package, some unit tests have been
stored in the `tests/` directory. To run these unit tests, you can
either evaluate the specific buffer in Emacs and run `ert` (see
<https://www.gnu.org/software/emacs/manual/html_node/ert/Running-Tests-Interactively.html>
for more information), or you can them all from the command line
using the Makefile:

```bash
make test
```

## Comparison with other tools

- **inferior-cling**: https://github.com/brianqq/inferior-cling. This
  package hasn't been updated in quite a while, and is light on
  functionality.  CERN-ROOT-mode attempts to go beyond this to make the
  process of interacting with the REPL more seamless.  There are also
  other ongoing issues such as duplicated input
  (https://root-forum.cern.ch/t/interactive-input-in-emacs-is-echoed-progressively/24113)
  in dumb terminals (https://github.com/root-project/cling/pull/99)
  which have yet to be merged in cling. CERN-ROOT-mode solves these
  problems while we're waiting for the pull-request to be
  accepted. CERN-ROOT-mode also aims to support org-babel to allow one to
  execute C++ source code blocks from within an org-mode document.
