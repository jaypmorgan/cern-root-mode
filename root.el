;;; root.el --- Major-mode for running C++ code with ROOT  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jay Morgan

;; Author: Jay Morgan <jaymorgan@debian>
;; Keywords: languages, tools

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

;; ROOT (https://root.cern/) is a framework for performing data
;; analysis. This package means to integrate this framework within the
;; ecosystem of Emacs. More specifically, root.el provides functions
;; to run C++ code within the ROOT REPL and execute org-mode source
;; code blocks, replicating the jupyter environment in `root
;; --notebook'.

;;; Code:

(defcustom root nil
  "Major-mode for running C++ code with ROOT"
  :group 'languages)

(defcustom root-filepath "root"
  "Path to the ROOT executable"
  :type 'string
  :group 'root)

(defcustom root-command-options ""
  "Command line options for running ROOT"
  :type 'string
  :group 'root)

(defcustom root-prompt-regex "^\\(?:root \\[[0-9]+\\]\\)"
  "Regular expression to find prompt location in ROOT-repl."
  :type 'string
  :group 'root)

(defcustom root-terminal-backend 'inferior
  "Type of terminal to use when running ROOT"
  :type 'symbol
  :options '(inferior vterm)
  :group 'root)

(defcustom root-buffer-name "*ROOT*"
  "Name of the newly create buffer for ROOT"
  :type 'string
  :group 'root)

;;; end of user variables

(defmacro remembering-position (&rest body)
  `(save-window-excursion (save-excursion ,@body)))

(defun pushnew (element lst)
  (if (member element lst)
      lst
    (push member lst)))

(defvar root--backend-functions
  '((vterm . ((start-terminal . root--start-vterm)
	      (send-function . root--send-vterm)))
    (inferior . ((start-terminal . root--start-inferior)
		 (send-function . root--send-inferior))))
  "Mapping from terminal type to various specific functions")

(defun root--get-functions-for-terminal (terminal)
  (cdr (assoc terminal root--backend-functions)))

(defun root--get-function-for-terminal (terminal function-type)
  (cdr (assoc function-type (root--get-functions-for-terminal terminal))))

(defun root--get-function-for-current-terminal (function-type)
  (root--get-function-for-terminal root-terminal-backend function-type))

(defalias 'root--ctfun 'root--get-function-for-current-terminal
  "ctfun -- current terminal function")

(defun root--send-vterm (proc input)
  "Send a string to the vterm REPL."
  (remembering-position
   (root-switch-to-repl)
   (vterm-send-string input)
   (vterm-send-return)))

(defun root--send-inferior (proc input)
  "Send a string to an inferior REPL."
  (comint-send-string proc (format "%s\n" input)))

(defun root--send-string (proc input)
  "Send a string to the ROOT repl."
  (funcall (root--ctfun 'send-function) proc input))

(defun root--start-vterm ()
  "Run an instance of ROOT in vterm"
  (with-current-buffer (vterm root-buffer-name)
    (vterm-send-string root-filepath)
    (vterm-send-return)))

(defun root--start-inferior ()
  "Run an inferior instance of ROOT"
  (let ((root-exe root-filepath)
	(buffer (comint-check-proc "ROOT")))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'root-mode))
	     (comint-check-proc (current-buffer)))
	 (get-buffer-create (or buffer root-buffer-name))
       (current-buffer)))
    (unless buffer
      (make-comint-in-buffer "ROOT" buffer root-exe nil root-command-options)
      (root-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode & comint functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun root--initialise ()
  (setq comint-process-echoes t
	comint-use-prompt-regexp t))

(defvar root-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for ROOT")

(define-derived-mode root-mode comint-mode
  "ROOT"
  "Major for `run-root'.

\\<root-mode-map>"
  nil "ROOT"
  (setq comint-prompt-regexp root-prompt-regex
	comint-prompt-read-only nil
	process-connection-type 'pipe)
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) root-prompt-regex)
  (add-hook 'comint-dynamic-complete-functions 'root--comint-dynamic-completion-function nil 'local)
  (set (make-local-variable 'comint-input-sender) 'root--send-string)
  (set (make-local-variable 'company-backends) (pushnew 'company-capf company-backends))
  (add-hook 'root-mode-hook 'root--initialise))

;; (defun org-babel-execute:root (body params)
;;   "Execute a block of C++ code with ROOT in org-mode."
;;   (message "Executing C++ source code block in ROOT"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar root--keywords nil)

(defvar root--completion-buffer-name "*ROOT Completions*")

(defun root--completion-filter-function (text)
  (setf root--keywords text))

(defun root--clear-completions ()
  (when (get-buffer root--completion-buffer-name)
    (with-current-buffer root--completion-buffer-name
      (erase-buffer))))

(defun root--get-partial-input (beg end)
  (buffer-substring-no-properties beg end))

(defun root--remove-ansi-escape-codes (string)
  (let ((regex "\\[[0-9;^k]+m?"))
    (s-replace-regexp regex "" string)))

(defun root--get-completions-from-buffer ()
  (with-current-buffer root--completion-buffer-name
    (while (not comint-redirect-completed)
      (sleep 0.01))
    (setq root--keywords (split-string (root--remove-ansi-escape-codes (buffer-string)) "\n"))))

(defun root--comint-dynamic-completion-function ()
  (cl-return)
  (when-let* ((bound (bounds-of-thing-at-point 'symbol))
	      (beg   (car bound))
	      (end   (cdr bound)))
    (when (> end beg)
      (let ((partial-input (root--get-partial-input beg end)))
	(message partial-input)
	(root--clear-completions)
	(comint-redirect-send-command-to-process
	 (format "%s\t" partial-input) root--completion-buffer-name root-buffer-name "" nil)
	(list beg end root--keywords . nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun run-root ()
  "Run an inferior instance of ROOT"
  (interactive)
  (funcall (root--ctfun 'start-terminal)))

;;;###autoload
(defun run-root-other-window ()
  "Run an inferior instance of ROOT in an different window"
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (run-root))

(defun root-switch-to-repl ()
  "Switch to the ROOT REPL"
  (interactive)
  (let ((win (get-buffer-window root-buffer-name)))
    (if win
	(select-window win)
      (switch-to-buffer root-buffer-name))))

(defun root-eval-region (beg end)
  "Evaluate a region in ROOT"
  (interactive "r")
  (kill-ring-save beg end)
  (comint-send-region root-buffer-name beg end))

(defun root-eval-defun ()
  "Evaluate a function in ROOT"
  (interactive)
  (remembering-position
   (mark-defun)
   (root-eval-region (region-beginning) (region-end))))

(defun root-eval-buffer ()
  "Evaluate the buffer in ROOT"
  (interactive)
  (remembering-position
   (mark-whole-buffer)
   (root-eval-region (region-beginning) (region-end))))

(defun root-eval-file (filename)
  "Evaluate a file in ROOT"
  (interactive "fFile to load")
  (comint-send-string root-buffer-name (concat ".U " filename "\n"))
  (comint-send-string root-buffer-name (concat ".L " filename "\n")))

(defun root-change-working-directory (dir)
  "Change the working directory of ROOT"
  (interactive "DChange to directory")
  (comint-send-string root-buffer-name (concat "gSystem->cd(\"" dir "\");\n")))

(defun root-list-input-history ()
  "List the history of previously entered statements"
  (interactive)
  (comint-dynamic-list-input-ring))

(provide 'root-mode)
;;; root.el ends here
