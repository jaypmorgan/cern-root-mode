;;; org-babel-root.el --- org-babel functions for evaluating C++ code in ROOT  -*- lexical-binding: t; -*-

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

;; Further to the code written in root.el, org-babel-root.el provides
;; the facilities to execute C++ org-mode source code blocks using the
;; ROOT REPL as opposed to existing C++ org-babel mode. This can make
;; writing C++ in org-mode a smoother experience.

;;; Requirements:

;; - root-mode :: Installed via this repository

;; - org-babel :: Installed from ELPA.

;;; Code:

(require 'ob) ;; org-babel
(load "./root.el")

;;(org-babel-add-interpreter "root")
(add-to-list 'org-src-lang-modes '("root" . c++))

;; no session, create temporary file and execute with macro file

(defun org-babel-execute:root (body params)
  "Execute a C++ org-mode source code block with ROOT."
  (message "Executing C++ source code block with ROOT")
  (print params)
  (let ((session (pluck-item :session params)))
    (if (not (string= session "none"))  ;; handle the user set session parameter
	(org-babel-execute--root-session session body params)
      (org-babel-execute--root-temp-session body params))))

(defun org-babel-root--cmdline-clean-result (string filename)
  (string-replace (format "\nProcessing %s...\n" filename) "" string))

(defun org-babel-root--cmdline-simple-wrapper (body func)
  (format "void %s() {\n%s\n}" func body))

(defun org-babel-root--kill-session ()
  (root--send-string root-buffer-name ".q"))

(defun org-babel-root--start-session ()
  (remembering-position (run-root)))

(defun org-babel-execute--root-temp-session (body params)
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
  (let ((root-buffer-name (make-earmuff session)))
    (unless (get-buffer root-buffer-name)
      (remembering-position
       (run-root)))
    (root--send-string root-buffer-name body)
    (sleep-for 0.5)
    (root--get-last-output)))

(defun org-babel-execute--root-no-session (body params)
  (let* ((file (org-babel-temp-file "root" ".C"))
	 (func (string-replace ".C" "" (car (last (split-string file "/")))))
	 (cmd  (format "%s -b -l -q %s" root-filepath file)))
    (org-babel-with-temp-filebuffer file
      (insert (org-babel-root--cmdline-simple-wrapper body func))
      (save-buffer))
    (org-babel-root--cmdline-clean-result (org-babel-eval cmd "") file)))

(provide 'org-babel-root)
;;; org-babel-root.el ends here
