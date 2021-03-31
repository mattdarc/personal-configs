;;; package -- Summary
;;; Commentary:
;;Spacemacs configuration for p4 and sbtools

;;; Code:
(message "Loading work.el")

;; If I am not at work, then I need to define all the p4 commands myself to communicate over ssh
;; TODO add an sbreviewboard function to run directly from emacs

(defun sbt/shell-command-string (command)
  "Create the initial string for a shell @command in the context of a setup environment"
  (concat ". /hub/share/sbtools/bash_setup_env.bash && " command))

(defun sbt/rehash (file)
  "Rehash the current matlab file buffer"
  (shell-command (sbt/shell-command-string (concat "sbrehash " (replace-regexp-in-string
						  "/ssh:work:" "" file))))
  (message (concat "Successfully rehashed " (file-name-nondirectory file)))
  )

(defun sbt/p4-command-string (action)
  "Create the string to execute the p4 @action on the current buffer"
  (interactive)
  (sbt/shell-command-string (concat "p4 " action " "
				    (replace-regexp-in-string
				     "/ssh:work:" "" buffer-file-truename)))
  )

(defun sbt/p4-command (action)
  "Execute the given p4 action."
  (shell-command (sbt/p4-command-string action))
  )

(defun sbt/p4-edit ()
  (interactive)
  (sbt/p4-command "edit")
  (read-only-mode -1)
  )

(defun sbt/p4-add ()
  (interactive)
  (sbt/p4-command "add")
  (read-only-mode -1)
  )

(defun sbt/p4-revert ()
  (interactive)
  (sbt/p4-command "revert")
  (revert-buffer :ignore-auto :noconfirm)
  (read-only-mode t)
  )

(defun sbt/p4-diff ()
  (interactive)
  (let* ((filename (file-name-nondirectory buffer-file-truename))
	 (old-shell-file-name shell-file-name)
	 (shell-file-name "/bin/sh")
	 (string (shell-command-to-string (sbt/p4-command-string "diff -du")))
	 (buffer (get-buffer-create (concat "*p4 diff<" filename ">*")))
	 (shell-file-name old-shell-file-name)
	 )
    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-max))
      (insert string))
    (unless (get-buffer-window "*p4 diff*")
      (with-current-buffer buffer (diff-mode))
      (display-buffer buffer '(display-buffer-pop-up-window . nil))))
  )

(defun sbt/p4-diff-all-opened ()
  (interactive)
  (let* ((filename (file-name-nondirectory buffer-file-truename))
	 (old-shell-file-name shell-file-name)
	 (shell-file-name "/bin/sh")
	 (string (shell-command-to-string (sbt/shell-command-string "p4 opened | sed -e 's/#.*//' | p4 -x - diff -du")))
	 (buffer (get-buffer-create (concat "*p4 diff<OPENED>*")))
	 (shell-file-name old-shell-file-name)
	 )
    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-max))
      (insert string))
    (unless (get-buffer-window "*p4 diff*")
      (with-current-buffer buffer (diff-mode))
      (display-buffer buffer '(display-buffer-pop-up-window . nil))))
  )

(defun sbt/sbindent ()
  (interactive)
  (let ((clang-format-style "/ssh:work/~/.sbindent.cfg")
	(clang-format)
	(clang-format-style nil)))
  )

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 ;; editing
 "m e" 'sbt/p4-edit
 "m r" 'sbt/p4-revert
 "m d" 'sbt/p4-diff
 "m a" 'sbt/p4-add
 "m A" 'sbt/p4-diff-all-opened
 ;; "s b" 'sb-debug-gdb
 ;; "y i" 'yas-insert-snippet
 ;; "y e" 'yas-expand

 ;; ;; LSP shortcuts
 ;; "l d" 'lsp-find-definition
 ;; "l i" 'lsp-find-implementation
 ;; "l t" 'lsp-find-type-definition

 ;; ;; Compilation
 ;; "m c c" 'sbcc-file
 ;; "m f" 'clang-format
 )

(add-hook 'c++-mode-hook #'smartparens-mode)
(add-hook 'c++-mode-hook #'rainbow-delimiters-mode)
    ;; )
;; )

(message "Finished loading work.el")

(provide 'work)
;;; work.el ends here
