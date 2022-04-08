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

(defvar root-filepath "root"
  "* Path to the ROOT executable")

(defvar root-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    map)
  "Basic mode map for ROOT")

(defvar root-prompt-regex "^\\(?:\\root\\s\\[\\d+\\]\\)"
  "* Regular expression to find prompt location in ROOT-repl.")

(defun run-root ()
  "Run an inferior instance of ROOT"
  (interactive)
  (let ((root-exe root-filepath)
	(buffer (comint-check-proc "ROOT")))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'root-mode))
	     (comint-check-proc (current-buffer)))
	 (get-buffer-create (or buffer "*ROOT*"))
       (current-buffer)))
    (unless buffer
      (make-comint-in-buffer "ROOT" buffer root-exe "")
      (root-mode))))

(defun root--initialise ()
  (setq comint-process-echoes t
	comint-use-prompt-regexp t))

(define-derived-mode root-mode comint-mode
  "ROOT"
  "Major for `run-root'.

\\<root-mode-map>"
  nil "ROOT"
  (setq comint-prompt-regexp root-prompt-regex
	comint-prompt-read-only t)
  (set (make-local-variable 'paragraph-separate "\\'"))
  (set (make-local-variable 'font-lock-defaults (c++-font-lock-keywords)))
  (set (make-local-variable 'paragraph-start root-prompt-regex))
  (add-hook 'root-mode-hook 'root--initialise))

;; (defun org-babel-execute:root (body params)
;;   "Execute a block of C++ code with ROOT in org-mode."
;;   (message "Executing C++ source code block in ROOT"))

(provide 'root-mode)
;;; root.el ends here
