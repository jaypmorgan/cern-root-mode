(defvar root-filepath "/home/jaymorgan/Téléchargements/root-6.26.00/root_install/bin/root"
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
