(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(setq local-libs "~/.emacs.d/lib")
(use-package work      :load-path local-libs)
(use-package zml-mode  :load-path local-libs)
(use-package jack-mode :load-path local-libs)
(use-package functions :load-path local-libs)
(use-package c-style   :load-path local-libs)
(use-package cmake-mode)

;; initial window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; misc
(setq-default )
(tool-bar-mode -1)
(add-to-list 'default-frame-alist
	     '(vertical-scroll-bars . nil))
(toggle-scroll-bar -1)
(setq mouse-avoidance-mode t)

(global-hl-line-mode t)
(set-face-underline 'hl-line nil)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(setq make-backup-files nil)
(setq browse-url-browser-function 'browse-url-firefox)
(setq vc-follow-symlinks t)
(setq custom-file "~/.emacs.d/lib/custom.el")
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

;; so these apply to all frames
(defun default-frame (frame)
  (with-selected-frame frame
    (set-frame-font "Source Code Pro 10" nil t)
    (setq frame-title-format '(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))
    (setq blink-cursor-mode t)
    )
  )

(if (daemonp)
   (add-hook 'after-make-frame-functions 'default-frame)
 (default-frame (selected-frame))
 )

;; disable auto-refine-mode
(add-hook 'diff-mode-hook (lambda () (diff-auto-refine-mode -1)))

;; default fill column for fill-paragraph
(setq-default fill-column 90)

;; line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'flyspell-mode)

;; This allows emacs to automatically update packages, but on this system I do not have permission
;; to update markdown-mode

;; (use-package auto-package-update
;;    :config
;;    (setq auto-package-update-delete-old-versions t
;;          auto-package-update-interval 4)
;;    (auto-package-update-maybe))

(use-package general
  :init
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  :config
  (general-override-mode)
  (general-define-key
   :states '(normal visual)
   :keymaps '(override evil-normal-state-map)
   :prefix "SPC"
    ;; buffer operations
    "b o" 'my/kill-other-buffers
    "b r" 'my/revert-buffer-no-confirm
    "b k" 'kill-this-buffer
    "r r" 'replace-regexp
    "<" 'counsel-switch-buffer
    ">" 'counsel-switch-buffer-other-window

    ;; general operations
    "q f" 'delete-frame
    "q q" 'evil-quit-all
    "e r" 'eval-region
    "e b" 'eval-buffer
    "s t" 'term
    ":" 'eval-expression
    "h k" 'describe-key
    "h f" 'describe-function
    "h v" 'describe-variable
    "r g" 'rgrep

    ;; evil window
    "w +" 'my/window-increase-height
    "w -" 'my/window-decrease-height
    "w >" 'my/window-increase-width
    "w <" 'my/window-decrease-width
    "w l" 'evil-window-right
    "w h" 'evil-window-left
    "w k" 'evil-window-up
    "w j" 'evil-window-down
    "w H" 'evil-window-move-far-left
    "w L" 'evil-window-move-far-right
    "w K" 'evil-window-move-very-top
    "w J" 'evil-window-move-very-bottom
    "w R" 'evil-window-rotate-upwards
    "w =" 'evil-auto-balance-windows
    "w v" 'evil-window-vsplit
    "w s" 'evil-window-split
    "w n" 'evil-window-new
    "w N" 'evil-window-vnew
    "w q" 'evil-window-delete

    ;; text operations
    "c u" 'upcase-dwim
    "c c" 'capitalize-dwim
    "c w" 'delete-trailing-whitespace

    ;; programming
    "!" 'shell-command

    ;; dired
    "d" 'dired
    )

  (general-define-key
   :states 'insert
   :prefix "<C>"
   "<backspace>" 'aborn/backward-kill-word
    )

  (general-define-key
   :states 'visual
   "TAB" 'indent-region)

  (general-define-key
   :states 'normal
    "TAB" 'indent-according-to-mode

    ;; file operations
    "f f" 'find-file
    "f r" 'recentf-open-files
    "f s" 'save-buffer
    "f w" 'write-file
    "f l" 'yank-current-line-position
    "f y" 'yank-buffer-name
    )

  ;; (general-define-key
   ;; :states '(normal visual motion)
   ;; :keymaps 'override
   ;; "SPC" 'hydra-space/body)

  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   :prefix "SPC"
   "SPC" 'counsel-M-x
   )

  (general-define-key
   :states 'normal
   :keymaps 'diff-mode-map
   "C-j" 'diff-hunk-next
   "C-k" 'diff-hunk-prev
   "q" 'evil-window-delete)
  )

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  )

(use-package evil-collection
  :custom (evil-collection-setup-minibuffer t)
  :after evil
  :config
  (setq evil-collection-mode-list
  	'(arc-mode
	  calendar-mode))
  (evil-collection-init))

(use-package smartparens
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (sp-local-pair #'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair #'scheme-mode "'" nil :actions nil)
  (setq sp-escape-quotes-after-insert nil)
  )

 (use-package evil-commentary
  :config
  (evil-commentary-mode)
  (general-define-key
   :states '(normal visual)
    ";" 'evil-commentary-line)
  )

(use-package projectile
  :config
  (projectile-mode +1)
  (general-define-key
   :keymaps 'projectile-mode-map
   :states 'normal
   :prefix "SPC"
   "p" 'projectile-command-map)

  (setq projectile-git-submodule-command "")
  )

 (use-package treemacs
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "t t" 'treemacs
   "t a" 'treemacs-add-project-to-workspace
   "t r" 'treemacs-remove-project-from-workspace
   "t f" 'treemacs-create-file
   "t n" 'treemacs-create-dir
   "t y" 'treemacs-copy-file
   "t d" 'treemacs-delete
   "t /" 'treemacs-find-file
    )
  )

 (general-define-key
 ;;; gud settings
 :states '(normal visual)
 :prefix "SPC"
 "g n" 'gud-next
 "g s" 'gud-step
 "g f" 'gud-finish
 "g b" 'gud-break
 "g c" 'gud-cont
 "g r" 'gud-remove
 "g w" 'gud-watch
 "g h" 'gud-until
 )

(use-package matlab
  :ensure matlab-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))
  (add-hook 'matlab-mode-hook #'display-line-numbers-mode)
  (add-hook 'after-save-hook (lambda () (if (eq major-mode 'matlab-mode)
					    (sbt/rehash buffer-file-truename)
					  )
			       ))
  )

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cc-mode-hook #'rainbow-delimiters-mode)
  )

(use-package counsel
  :config
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
    "SPC" 'counsel-M-x
    )
  (general-define-key
   :states 'insert
   :prefix "<C>"
   "SPC" 'counsel-M-x
   )
  )

(use-package swiper
  :config
  (general-define-key
   :states '(normal visual)
    "/" 'swiper
    )
  )

(use-package doom-themes
  :config
  (setq doom-dark+-blue-modeline t)
  ;; (setq doom-monokai-classic-brighter-comments t)
  ;; (setq doom-monokai-classic-padded-modeline nil)
  (load-theme 'doom-dark+ t)
   )


(require 'doom-themes)
(use-package ivy
:bind (:map ivy-minibuffer-map
	    (("C-j" . ivy-next-line)
	    ("C-k" . ivy-previous-line)
	    ("C-p" . evil-paste-after))

	    :map ivy-switch-buffer-map
	      (("C-j" . ivy-next-line)
	       ("C-k" . ivy-previous-line))
	      )

  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  ;; (set-face-attribute 'ivy-highlight-face t :background "white" :foreground "white")
  (ivy-mode 1)
  )

(use-package powerline
  :config
  (powerline-default-theme)
  )

(use-package which-key
  :config
  (which-key-mode)
  )

(use-package company
  :config
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.0)
  (general-define-key
   :keymap company-active-map
   :states 'insert
   "C-j" 'company-select-next
   "C-k" 'company-select-previous
   )
  (add-hook 'emacs-lisp-mode-hook 'company-mode)

  ;; Do not downcase suggestions
  (setq company-dabbrev-downcase nil)
  )

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode)
  )

;; (require 'org-tempo)
(use-package org
  :config
  (setq org-todo-keywords
	'((sequence "TODO" "IN_PROGRESS" "|" "DONE")
	  ))

  (setq org-todo-keyword-faces
	'(("TODO" . (:foreground "#ff39a3" :weight bold))
	  ("IN_PROGRESS" . "orange")
	  ))
  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   "g l" 'org-open-at-point
   "g b" 'org-mark-ring-goto
   ))
(use-package evil-org)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5)
  )

(use-package mwim
  :config
  (general-define-key
   :states '(normal visual)
   "0" 'mwim-beginning
   "$" 'mwim-end
   )
  )

;; Code formatting
(use-package clang-format
  :config
  (general-define-key
   :prefix "SPC"
   :states '(normal visual)
   :keymaps 'c++-mode-map
   "f r" 'clang-format-region
   "f b" 'clang-format-buffer)
  (setq clang-format-executable "/usr/bin/clang-format")
  )

(use-package scalariform
  :config
  (general-define-key
   :prefix "SPC"
   :states '(normal visual)
   :keymaps 'scala-mode-map
   "f r" 'scalariform-format-region
   "f b" 'scalariform-format-region-or-buffer)
  )

(use-package rust-mode)

;; Configure lsp-mode
(use-package lsp-mode
  :commands lsp
  :hook ((scala-mode . lsp)
	 (rust-mode . lsp)
	 (matlab-mode . lsp-deferred)
	 (c++-mode . lsp)
	 (c-mode . lsp)
	 ;; (lsp-mode . lsp-lens-mode) ;; inline references
	 (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-file-watchers nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (setq lsp-disabled-clients '(clangd))
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  (make-overlay (point-at-bol) (1+ (point-at-eol)))
  (general-define-key
   :keymaps 'xref--xref-buffer-mode-map
   :states 'normal
   "RET" 'xref-goto-xref
   )

  ;; redefine the lsp mode keymap
  (general-define-key
   :prefix "SPC"
   :states 'normal
   "l" '(:keymap lsp-command-map :package lsp-mode))

  ;; use matlabls - this will not work remotely at the moment
  (let ((matlabls-file "//mathworks/inside/labs/dev/matlab_coder_tools//matlabls/matlabls/editors/emacs/lsp-matlab.el"))
    (when (file-exists-p matlabls-file)
      (load-file matlabls-file)
      ))
  (require 'mlint)
  (defun mlint-buffer () "Noop override for mlint")
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(no-accept-focus . t))
  (setq lsp-ui-doc-enable nil
	lsp-ui-flycheck-enable t
	lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-show-hover t
	lsp-ui-peek-enable t
	)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )

(use-package lsp-ivy)
(use-package flycheck
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enable)))

(use-package lsp-treemacs
  :config
  (setq lsp-metals-treeview-show-when-views-received t)
  )

;; lsp backends
(use-package lsp-metals)
(use-package ccls
  ;;:hook ((c-mode c++-mode cc-mode) . (lambda () (require 'ccls) (my/ccls-connect-server)))
  :config
  (setq ccls-executable "ccls")
  (setq ccls-code-lens-mode nil)
  ;; (setq ccls-args '("--log-file=/tmp/ccls.log" "-v=1" ;; uncomment to enable logging
  ;; 		    ;; without this a lot of std headers are not compiled correctly
  ;; 		    "--init={\"clang\": {\"resourceDir\": \"/usr/local/Cellar/llvm/10.0.0_3/lib/clang/10.0.0\"}, \"index\":{\"threads\":4,\"trackDependency\":0}}"
  ;; 		    ))
  )


(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

(use-package plantuml-mode
  :config
  (setq plantuml-jar-path "/usr/local/bin/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (general-define-key
   :keymap org-mode-map
   "C-c p" 'plantuml-preview-current-block)

  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

  ;; Edit PlantUML sources in org-mode
  ;; #+BEGIN_SRC plantuml
  ;;   <hit C-c ' here to open a plantuml-mode buffer>
  ;; #+END_SRC
  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)
							   (python . t)
							   (shell . t)))
  (add-hook 'org-mode-hook #'dubcaps-mode)
  )

(use-package yasnippet
  :bind (:map yas-minor-mode-map
  	      (("<backtab>" . yas-expand)))
  :hook ((c++-mode rust-mode) . yas-minor-mode)
  :config
  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map)
  (general-define-key
   :prefix "SPC"
   :states 'normal
   "y i" 'yas-insert-snippet)
  )

(use-package scala-mode)

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
	'(("TODO" . "#FF0000")
	  ("NOTE" . "#FF0080")
	  ("FIXME". "#FF0000")
	  ("XXX"  . "#FF0000")
	  ("TLDR" . "#FFFF00")))
  (setq global-hl-todo-mode t)
  )

(setq eshell-scroll-to-bottom-on-output t)
