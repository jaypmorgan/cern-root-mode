;;; root.el --- Major-mode for running C++ code with ROOT  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jay Morgan

;; Author: Jay Morgan <jay@morganwastaken.com>
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

(require 's)
(require 'ob) ;; org-babel
(require 'comint)
(require 'cl-lib)
(require 'cc-cmds)
(require 'vterm nil 'noerror)

(defcustom root-mode nil
  "Major-mode for running C++ code with ROOT"
  :group 'languages
  :type 'mode)

(defcustom root-filepath "root"
  "Path to the ROOT executable"
  :type 'string
  :group 'root-mode)

(defcustom root-command-options ""
  "Command line options for running ROOT"
  :type 'string
  :group 'root-mode)

(defcustom root-prompt-regex "^\\(?:root \\(?:(cont'ed, cancel with .@) \\)?\\[[0-9]+\\]\s?\\)"
  "Regular expression to find prompt location in ROOT-repl."
  :type 'string
  :group 'root-mode)

(defcustom root-terminal-backend 'inferior
  "Type of terminal to use when running ROOT"
  :type 'symbol
  :options '(inferior vterm)
  :group 'root-mode)

(defcustom root-buffer-name "*ROOT*"
  "Name of the newly create buffer for ROOT"
  :type 'string
  :group 'root-mode)

;;; end of user variables

(defmacro remembering-position (&rest body)
  `(save-window-excursion (save-excursion ,@body)))

(defun push-new (element lst)
  (if (memq element lst)
      lst
    (push element lst)))

(defun pluck-item (el lst)
  (cdr (assoc el lst)))

(defun make-earmuff (name)
  "Give a string earmuffs, i.e. some-name -> *some-name*"
  (if (or (not (stringp name))  ;; only defined for strings
	  (= (length name) 0)   ;; but not empty strings
	  (and (string= "*" (substring name 0 1))
	       (string= "*" (substring name (1- (length name))))))
      name
    (format "*%s*" name)))

(defun make-no-earmuff (name)
  "Remove earmuffs from a string if it has them, *some-name* -> some-name"
  (if (and (stringp name)
	   (> (length name) 0)
	   (string= "*" (substring name 0 1))
	   (string= "*" (substring name (1- (length name)))))
      (substring name 1 (1- (length name)))
    name))

(defun ->> (&rest body)
  (cl-reduce (lambda (prev next) (funcall (eval next) prev))
	     (cdr body)
	     :initial-value (eval (car body))))

(defvar root--backend-functions
  '((vterm . ((start-terminal . root--start-vterm)
	      (send-function . root--send-vterm)
	      (previous-prompt . vterm-previous-prompt)
	      (next-prompt . vterm-next-prompt)))
    (inferior . ((start-terminal . root--start-inferior)
		 (send-function . root--send-inferior)
		 (previous-prompt . comint-previous-prompt)
		 (next-prompt . comint-next-prompt))))
  "Mapping from terminal type to various specific functions")

(defun root--get-functions-for-terminal (terminal)
  (pluck-item terminal root--backend-functions))

(defun root--get-function-for-terminal (terminal function-type)
  (pluck-item function-type (root--get-functions-for-terminal terminal)))

(defun root--get-function-for-current-terminal (function-type)
  (root--get-function-for-terminal root-terminal-backend function-type))

(defalias 'root--ctfun 'root--get-function-for-current-terminal
  "ctfun -- current terminal function")

(defun root--send-vterm (proc input)
  "Send a string to the vterm REPL."
  (remembering-position
   (root-switch-to-repl)
   (when (fboundp 'vterm-send-string)
     (vterm-send-string input)
     (vterm-send-return))))

(defun root--send-inferior (proc input)
  "Send a string to an inferior REPL."
  (comint-send-string proc (format "%s\n" input)))

(defun root--preinput-clean (input)
  ;; move the template definition onto the same line as the function declaration
  (replace-regexp-in-string "template<\\(.*\\)>\n" "template<\\1>" (format "%s" input)))

(defun root--send-string (proc input)
  "Send a string to the ROOT repl."
  (funcall (root--ctfun 'send-function) proc (root--preinput-clean input)))

(defun root--start-vterm ()
  "Run an instance of ROOT in vterm"
  (let ((vterm-shell root-filepath))
    (vterm root-buffer-name)))

(defun root--start-inferior ()
  "Run an inferior instance of ROOT"
  (let ((root-exe root-filepath)
	(buffer (comint-check-proc (make-no-earmuff root-buffer-name)))
	(created-vars (root--set-env-vars)))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'root-mode))
	     (comint-check-proc (current-buffer)))
	 (get-buffer-create (or buffer root-buffer-name))
       (current-buffer)))
    (unless buffer
      (make-comint-in-buffer (make-no-earmuff root-buffer-name) buffer root-exe nil root-command-options)
      (root-mode))
    (when created-vars
      (sleep-for 0.1)  ;; give enough time for ROOT to start before removing vars
      (root--unset-env-vars))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun root--make-text-property (input)
  (propertize "root_input_2"
	      'buffer-state input
	      'face 'kaiti-red
	      'read-only t))

(defun root--text-property-get-contents (text)
  (get-text-property 0 'buffer-state text))

(defun root--make-temporary-buffer (text)
  (remembering-position
   (let* ((temp-file (format "%s.C" (make-temp-file "root")))
	  (buf       (find-file temp-file)))
     (switch-to-buffer buf) 
     (insert (root--text-property-get-contents text))
     (write-file temp-file nil)
     (kill-buffer buf)
     temp-file)))

(defun root--send-input-buffer (input)
  (let* ((text (root--make-text-property input))
	 (file (root--make-temporary-buffer text)))
    (remembering-position
     (root-switch-to-repl)
     (let ((pos (point)))
       (print pos)
       (root-eval-file file)
       (sleep-for 0.5)
       (goto-char (point-max))
       (let ((end-post (point)))
	 (delete-region pos end-post)
	 (goto-char (point-max))
	 (let ((new-point (point)))
	   (root--send-string root-buffer-name "\n")
	   (sleep-for 0.5)
	   (goto-char new-point)
	   (insert text)
	   (goto-char (point-max))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode & comint functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar root--rcfile "./.rootrc")

(defun root--set-env-vars ()
  "Setup the environment variables so that no colours or bold
fonts will be used in the REPL. This prevents comint from
creating duplicated input in trying to render the ascii colour
codes.

Function returns t if the variables have been set, else nil. This
return value is very useful for deciding if the variables should
be unset, as we will want not want to remove the user's existing
rcfiles."
  (if (not (file-exists-p root--rcfile))  ;; don't clobber existing rcfiles
    (let ((vars (list "PomptColor" "TypeColor" "BracketColor" "BadBracketColor" "TabComColor"))
	  (val  "default")
	  (buf  (create-file-buffer root--rcfile)))
      (with-current-buffer buf
	(insert (apply 'concat (mapcar (lambda (v) (format "Rint.%s\t\t%s\n" v val)) vars)))
	(write-file root--rcfile nil))
      (kill-buffer buf))
    nil))

(defun root--unset-env-vars ()
  (delete-file root--rcfile))

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
  (set (make-local-variable 'comint-input-sender) 'root--send-string)
  (add-hook 'root-mode-hook 'root--initialise))

;; (defun org-babel-execute:root (body params)
;;   "Execute a block of C++ code with ROOT in org-mode."
;;   (message "Executing C++ source code block in ROOT"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar root--keywords nil)

(defvar root--completion-buffer-name "*ROOT Completions*")

(defun root--get-last-output ()
  ;; TODO: needs improvement to better capture the last output
  (remembering-position
   (root-switch-to-repl)
   (goto-char (point-max))
   (let* ((regex (format "%s.*" (substring root-prompt-regex 1)))
	  (np (re-search-backward regex))
	  (pp (progn (re-search-backward regex)
		    (forward-line)
		    (point))))
     (buffer-substring-no-properties pp np))))

(defun root--completion-filter-function (text)
  (setf root--keywords text))

(defun root--clear-completions ()
  (when (get-buffer root--completion-buffer-name)
    (with-current-buffer root--completion-buffer-name
      (erase-buffer))))

(defun root--get-partial-input (beg end)
  (buffer-substring-no-properties beg end))

(defun root--remove-ansi-escape-codes (string)
  (let ((regex "\\[[0-9;^kD]+m?"))
    (s-replace-regexp regex "" string)))

(defun root--get-completions-from-buffer ()
  (with-current-buffer root--completion-buffer-name
    (while (not comint-redirect-completed)
      (sleep-for 0.01))
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
;; Org-babel definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("root" . c++)))

;;;###autoload
(defun org-babel-execute:root (body params)
  "Execute a C++ org-mode source code block with ROOT."
  (message "Executing C++ source code block with ROOT")
  (print params)
  (let ((session (pluck-item :session params)))
    (if (not (string= session "none"))  ;; handle the user set session parameter
	(org-babel-execute--root-session session body params)
      (org-babel-execute--root-temp-session body params))))

(defun org-babel-root--cmdline-clean-result (string filename)
  (replace-regexp-in-string (format "\nProcessing %s...\n" filename) "" string))

(defun org-babel-root--cmdline-simple-wrapper (body func)
  (format "void %s() {\n%s\n}" func body))

(defun org-babel-root--kill-session ()
  (root--send-string root-buffer-name ".q"))

(defun org-babel-root--start-session ()
  (remembering-position (run-root)))

(defun org-babel-execute--root-temp-session (body params)
  (ignore params)
  (unwind-protect
      (progn
	(org-babel-root--start-session)
	(root--send-string root-buffer-name body)
	(sleep-for 0.5)  ;; wait for the output to be printed in the REPL
	(let ((output (root--get-last-output)))
	  (org-babel-root--kill-session)
	  output))
    (org-babel-root--kill-session)))

(defun org-babel-execute--root-session (session body params)
  (ignore params)
  (let ((root-buffer-name (make-earmuff session)))
    (unless (get-buffer root-buffer-name)
      (remembering-position
       (run-root)))
    (root--send-string root-buffer-name body)
    (sleep-for 0.5)
    (root--get-last-output)))

(defun org-babel-execute--root-no-session (body params)
  (ignore params)
  (let* ((file (org-babel-temp-file "root" ".C"))
	 (func (string-replace ".C" "" (car (last (split-string file "/")))))
	 (cmd  (format "%s -b -l -q %s" root-filepath file)))
    (org-babel-with-temp-filebuffer file
      (insert (org-babel-root--cmdline-simple-wrapper body func))
      (save-buffer))
    (org-babel-root--cmdline-clean-result (org-babel-eval cmd "") file)))

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
  (let ((string (format "%s" (buffer-substring beg end))))
    (root-switch-to-repl)
    (root--send-string root-buffer-name string)))

(defun root-eval-string (string)
  "Send and evaluate a string in the ROOT REPL."
  (root--send-string root-buffer-name string))

(defun root-eval-line ()
  "Evaluate this line in ROOT"
  (interactive)
  (remembering-position
   (let ((beg (progn (beginning-of-line) (point)))
	 (end (progn (end-of-line) (point))))
     (root-eval-region beg end))))

(defun root-eval-defun ()
  "Evaluate a function in ROOT"
  (interactive)
  (remembering-position
   (c-mark-function)
   (root-eval-region (region-beginning) (region-end))))

(defun root-eval-defun-maybe ()
  "Evaluate a defun in ROOT if in declaration else just the line"
  (interactive)
  (condition-case err
      (root-eval-defun)
    ('error (progn (ignore err)
		   (root-eval-line)))))

(defun root-eval-buffer ()
  "Evaluate the buffer in ROOT"
  (interactive)
  (remembering-position
   (root-eval-region (point-min) (point-max))))

(defun root-eval-file (filename)
  "Evaluate a file in ROOT"
  (interactive "fFile to load: ")
  (comint-send-string root-buffer-name (concat ".U " filename "\n"))
  (comint-send-string root-buffer-name (concat ".L " filename "\n")))

(defun root-change-working-directory (dir)
  "Change the working directory of ROOT"
  (interactive "DChange to directory: ")
  (comint-send-string root-buffer-name (concat "gSystem->cd(\"" (expand-file-name dir) "\")\n")))

(defun root-list-input-history ()
  "List the history of previously entered statements"
  (interactive)
  (comint-dynamic-list-input-ring))

(provide 'root-mode)
;;; root.el ends here
