;; Define several classes of keywords.
(defvar zml-types '("void" "string" "boolean" "double" "uint64" "int64"
		    "uint32" "int32" "int16" "uint16" "uint8" "int8"))
(defvar zml-keywords '("class" "struct" "static" "package" "override"
			"mutating" "extends" "enum" "contains" "opposite"
			"qualifiedby" "transient" "import" "derived" "readonly"
			"unordered" "private" "abstract" "default"
			"@.+"))
(defvar zml-constants '("true" "false" "null"))

;; Create a proper regex string for each keyword group.
(defvar zml-types-regexp (regexp-opt zml-types 'words))
(defvar zml-keywords-regexp (regexp-opt zml-keywords 'words))
(defvar zml-constants-regexp (regexp-opt zml-constants 'words))
(defvar zml-annotations-regexp ())

;; Create the list for font-lock.
;; Each class of keyword is given a particular face.
(defvar zml-font-lock-defaults
  `((,zml-types-regexp . font-lock-type-face)
    (,zml-constants-regexp . font-lock-constant-face)
    (,zml-keywords-regexp . font-lock-keyword-face)))

(defvar zml-tab-width 4 "Width of a tab for ZML mode.")



(define-derived-mode zml-mode c-mode "ZML"
  "A major mode to edit .zml files."

  (set (make-local-variable 'font-lock-defaults) '(zml-font-lock-defaults))

  ;; When there's an override, use it.
  ;; Otherwise it gets the default value.
  (when zml-tab-width
    (set (make-local-variable 'tab-width) zml-tab-width))

  (use-local-map zml-mode-map)

  (run-hooks 'zml-mode-hook))

(add-to-list 'auto-mode-alist '("\\.zml\\'" . zml-mode))

(provide 'zml-mode)
