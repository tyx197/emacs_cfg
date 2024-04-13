(set-language-environment "UTF-8")

(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
			 ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
			 ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

;(setq package-install-upgrade-built-in t)

(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
	auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
	auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
	tramp-backup-directory-alist `((".*" . ,backup-dir))
	tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;(load-theme 'gruvbox t)
(load-theme 'catppuccin t)

;(global-display-line-numbers-mode)
(use-package display-line-numbers-mode
  :ensure nil
  :hook
  (verilog-ts-mode emacs-lisp-mode scala-mode))

(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font Mono 18"))
(defun my/set-font-faces ()
  (message "Setting faces!")
  (set-fontset-font "fontset-default" 'han "sarasa-gothic")
  (set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font Mono" :height 180)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Nerd Font Mono" :height 180 :weight 'regular))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook
	      (lambda () (my/set-font-faces)))
    (my/set-font-faces))

;(ivy-mode)
;(setq ivy-use-virtual-buffers t)
;(setq enable-recursive-minibuffers t)

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
	 ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package consult
  :ensure t
  )

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (delete 'embark-mixed-indicator embark-indicators)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package ggtags
  :ensure t
  :hook
  (verilog-ts-mode . ggtags-mode))

(use-package imenu-list
  :ensure t)

(require 'centaur-tabs)
(centaur-tabs-mode t)
(global-set-key (kbd "C-<prior>")  'centaur-tabs-backward)
(global-set-key (kbd "C-<next>") 'centaur-tabs-forward)
(setq centaur-tabs-style "bar")
(setq centaur-tabs-set-icons t)
;(setq centaur-tabs-plain-icons t)
(setq centaur-tabs-gray-out-icons 'buffer)
(setq centaur-tabs-set-bar 'over)
;(setq centaur-tabs-close-button "X")
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-modified-marker "•")

(require 'pyim)
(require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
(pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
(setq default-input-method "pyim")
(global-set-key (kbd "C-\\") 'toggle-input-method)

(use-package org
  :ensure nil
  :custom
  (org-list-allow-alphabetical t)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-cycle-separator-lines 1)
  (org-startup-with-inline-images t)
  (org-blank-before-new-entry
		    '((heading . nil)
		      (plain-list-item . nil))))

(setq org-roam-directory (file-truename "~/mind"))
(org-roam-db-autosync-mode)
(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)

(use-package auto-fill
  :ensure nil
  :hook
  (org-mode text-mode)
  :custom
  (fill-column 100))

(use-package org-superstar
  :ensure t
  :hook
  (org-mode))

(use-package olivetti
  :ensure t
  :hook
  (org-mode text-mode))

(use-package verilog-ts-mode
  :config
  (set-face-attribute 'verilog-ts-font-lock-grouping-keywords-face nil :foreground "DarkGoldenrod1")
  (set-face-attribute 'verilog-ts-font-lock-punctuation-face nil       :foreground "burlywood")
  (set-face-attribute 'verilog-ts-font-lock-operator-face nil          :foreground "burlywood" :weight 'extra-bold)
  (set-face-attribute 'verilog-ts-font-lock-brackets-face nil          :foreground "goldenrod")
  (set-face-attribute 'verilog-ts-font-lock-parenthesis-face nil       :foreground "dark goldenrod")
  (set-face-attribute 'verilog-ts-font-lock-curly-braces-face nil      :foreground "DarkGoldenrod2")
  (set-face-attribute 'verilog-ts-font-lock-port-connection-face nil   :foreground "bisque2")
  (set-face-attribute 'verilog-ts-font-lock-dot-name-face nil          :foreground "gray70")
  (set-face-attribute 'verilog-ts-font-lock-brackets-content-face nil  :foreground "yellow green")
  (set-face-attribute 'verilog-ts-font-lock-width-num-face nil         :foreground "chartreuse2")
  (set-face-attribute 'verilog-ts-font-lock-width-type-face nil        :foreground "sea green" :weight 'bold)
  (set-face-attribute 'verilog-ts-font-lock-module-face nil            :foreground "green1")
  (set-face-attribute 'verilog-ts-font-lock-instance-face nil          :foreground "medium spring green")
  (set-face-attribute 'verilog-ts-font-lock-time-event-face nil        :foreground "dark orange" :weight 'bold)
  (set-face-attribute 'verilog-ts-font-lock-time-unit-face nil         :foreground "light steel blue")
  (set-face-attribute 'verilog-ts-font-lock-preprocessor-face nil      :foreground "pale goldenrod")
  (set-face-attribute 'verilog-ts-font-lock-modport-face nil           :foreground "light blue")
  (set-face-attribute 'verilog-ts-font-lock-direction-face nil         :foreground "RosyBrown3")
  (set-face-attribute 'verilog-ts-font-lock-translate-off-face nil     :background "gray20" :slant 'italic)
  (set-face-attribute 'verilog-ts-font-lock-attribute-face nil         :foreground "orange1")
  (add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-ts-mode)))

(use-package verilog-ext
  :ensure t
  :hook
  ((verilog-mode . verilog-ext-mode))
  :init
  (setq verilog-ext-feature-list
   '(
     ;font-lock
     xref
     ;capf
     ;hierarchy
     ;eglot
     ;lsp
     ;lsp-bridge
     ;flycheck
     ;beautify
     ;navigation
     ;template
     ;formatter
     ;compilation
     imenu
     which-func
     ;hideshow
     ;typedefs
     ;time-stamp
     ;block-end-comments
     ;ports
     ))
  (setq verilog-ext-tags-backend 'tree-sitter)
  :config
  (verilog-ext-mode-setup) (which-function-mode 1))

(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (verilog-ts-mode emacs-lisp-mode))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
