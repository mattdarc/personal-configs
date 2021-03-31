(defun my/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun aborn/backward-kill-word ()
  "Customize/Smart backward-kill-word."
  (interactive)
  (let* ((cp (point))
         (backword)
         (end)
         (space-pos)
         (backword-char (if (bobp)
                            ""           ;; cursor in begin of buffer
                          (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
        (progn
          (save-excursion
            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
          (setq ab/debug backword)
          (save-excursion
	    (when (and backword  ;; when backword contains space
		       (s-contains? " " backword))
	      (setq space-pos (1+ (ignore-errors (search-backward-regexp "[[:space:]][[:word:]]"))))))
          (save-excursion
            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                   (substr (when pos (buffer-substring pos cp))))
              (when (or (and substr (s-blank? (s-trim substr)))
                        (s-contains? "\n" backword))
                (setq end pos))))
          (if end
              (kill-region cp end)
            (if space-pos
                (kill-region cp space-pos)
              (backward-kill-word 1))))
      (kill-region cp (- cp 1)))         ;; word is non-english word
    ))

(defun yank-current-line-position()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'"
  (interactive)
  (let ((path-with-line-number
         (concat (file-name-nondirectory buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard")))
  )

(defun yank-buffer-name ()
  "Copy the buffer name for re-use in Emacs"
  (interactive)
  (let ((name (file-name-nondirectory buffer-file-name)))
    (when name (kill-new name))
    (message (concat name " copied to clipboard")))
  )

;; from https://stackoverflow.com/questions/4642835/how-to-change-the-cursor-color-on-emacs
(defvar blink-cursor-colors (list  "#33ccff" "#92c48f" "#6785c5" "#be369c" "#d9ca65")
  "On each blink the cursor will cycle to the next color in this list.")

(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  "Zarza wrote this cyberpunk variant of timer `blink-cursor-timer'.
Warning: overwrites original version in `frame.el'.

This one changes the cursor color on each blink. Define colors in `blink-cursor-colors'."
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
    (setq blink-cursor-count (+ 1 blink-cursor-count))
    )
  (internal-show-cursor nil (not (internal-show-cursor-p)))
  )

(global-set-key  [C-backspace]
            'aborn/backward-kill-word)

(defun connect-remote()
  "Connect to remote through ssh"
  (interactive)
  (dired "/ssh:work:/"))

(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals:
https://emacs.stackexchange.com/questions/13970/fixing-double-capitals-as-i-type - u/dan"
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -4 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(defun go-outside()
  "Just run this when I work outside"
  (interactive)
  (load-theme 'doom-solarized-light t)
  )

(defun my/font-lock-restart ()
  (interactive)
  (setq font-lock-mode-major-mode nil)
  (font-lock-fontify-buffer))

(defun my/click ()
  (interactive)
  (execute-kbd-macro (kbd "<escape>"))
  )

(defvar sb-ccls-clients (make-hash-table :test 'equal)
  "Hash table so I can check to see if a client was already registered for a sandbox")

(require 'work)
(defun my/ccls-root (bufname)
  (let* ((path (replace-regexp-in-string
		"/ssh:work:" "" (file-name-directory bufname)))
	 (is-remote (not (eq (file-name-directory bufname) path)))
	 (dot-ccls (locate-dominating-file path ".ccls-root"))
	 (ccls-root (if dot-ccls (directory-file-name dot-ccls) nil)))
    ccls-root)
  )

(defun my/ccls-server-id (ccls-root)
  (concat "ccls-" (replace-regexp-in-string (file-name-directory ccls-root) "" ccls-root)))

(defun my/ccls-activation-fn (filename mode req-id)
  (string= (my/ccls-server-id (my/ccls-root filename)) req-id))

(defun my/ccls-connect-server ()
  "Lazily register a ccls client for the current context. If in a sandbox, register a new one, else just use ccls"
  (interactive)
  (if buffer-file-truename
	(let ((ccls-root (my/ccls-root buffer-file-truename)))
	  (if ccls-root
	      (let* 
		  ((server-id (my/ccls-server-id ccls-root))
		   ;; NOTE: the args need to be single quoted if they are not provided as a list
		   (loc-ccls-args (replace-regexp-in-string "sbroot" ccls-root
	 						    (concat
	 						     "--init={\"cache\":{\"directory\":\"sbroot/.sbtools/sbcpptags/ccls\"},"
	 						     "\"clang\":{\"pathMappings\":[\"/local-ssd/mdarcang/.Bslmulticore.latest_pass.sbsyncmaster.inprogress/>sbroot/\","
	 						     "\"/local-ssd/mdarcang/.Bslmulticore.latest_pass.sbsyncmaster/>sbroot/\",\"/local-ssd/mdarcang/Bslmulticore.latest_pass/>sbroot/\"]},"
	 						     "\"compilationDatabaseDirectory\":\"sbroot/.sbtools/sbcpptags\","
	 						     "\"index\":{\"threads\":12,\"trackDependency\":0}}"
	 						     )))
		   (ccls-command (concat ccls-executable " " (if (equal (getenv "USER") "mdarcang") loc-ccls-args "")))
		   (server-exists (gethash server-id sb-ccls-clients)))
		
		(message "Server ID %s" server-id)
		
		(if ccls-root
		    (progn
		      (unless server-exists
			(lsp-register-client
			 (make-lsp-client
			  :server-id (make-symbol server-id)
			  ;; :new-connection (lsp-tramp-connection ccls-command)
			  :new-connection (lsp-stdio-connection `(,ccls-executable ,loc-ccls-args))
			  :major-modes '(c++-mode c-mode)
			  :activation-fn `(lambda (filename mode) (my/ccls-activation-fn filename mode ,server-id))
			  ;; :root ccls-root
			  ;; :remote? is-remote
			  ))
			(puthash server-id t sb-ccls-clients)
			(message "Registered LSP server %s" server-id))
		      (message "Connecting to LSP server %s" server-id)
		      (lsp-deferred))
		  ;; (message "Missing .ccls-root file. Place a '.ccls-root' file in the root directory of this project")
		  )
		))))
  )

(defun my/ccls-print-servers ()
  (interactive)
  (message "Installed CCLS servers %s" (maphash (lambda (k v)
						  (print k))
						sb-ccls-clients))
  )

(defun my/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defvar evil-window-modifier 1)

;; (evil-define-command my/window-increase-height (count)
;;   "Increase current window height by COUNT."
;;   :repeat nil
;;   (interactive "p")
;;   (evil-resize-window (+ (window-height) (* evil-window-modifier count))))
;;
;; (evil-define-command my/window-decrease-height (count)
;;   "Decrease current window height by COUNT."
;;   :repeat nil
;;   (interactive "p")
;;   (evil-resize-window (- (window-height) (* evil-window-modifier count))))
;;
;; (evil-define-command my/window-increase-width (count)
;;   "Increase current window width by COUNT."
;;   :repeat nil
;;   (interactive "p")
;;   (evil-resize-window (+ (window-width) (* evil-window-modifier count)) t))
;;
;; (evil-define-command my/window-decrease-width (count)
;;   "Decrease current window width by COUNT."
;;   :repeat nil
;;   (interactive "p")
;;   (evil-resize-window (- (window-width) (* evil-window-modifier count)) t))

(provide 'functions)
