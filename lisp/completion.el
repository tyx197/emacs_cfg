;;; completion.el --- Modern completion UIs -*- lexical-binding: t -*-

;; ── Vertico + Marginalia ──
(add-hook 'after-init-hook #'vertico-mode)
(add-hook 'after-init-hook #'marginalia-mode)
(setq completion-styles             '(orderless basic)
      completion-category-defaults  nil
      completion-category-overrides '((file (styles . (basic partial-completion)))))

;; ── Consult key bindings ──
(global-set-key (kbd "C-s")     'consult-line)
(global-set-key (kbd "M-y")     'consult-yank-pop)
(global-set-key (kbd "C-x b")   'consult-buffer)
(global-set-key (kbd "C-x r b") 'consult-bookmark)
(global-set-key (kbd "M-g g")   'consult-goto-line)
(global-set-key (kbd "M-g m")   'consult-mark)
(global-set-key (kbd "M-s d")   'consult-find)
(global-set-key (kbd "M-s g")   'consult-ripgrep)
(global-set-key (kbd "M-s l")   'consult-line-multi)
(global-set-key [remap switch-to-buffer] 'consult-buffer)
(global-set-key [remap imenu]            'consult-imenu)

;; ── Completion Preview ──
(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map "C-i"   #'completion-preview-insert)
  (keymap-set completion-preview-active-mode-map "<tab>" #'completion-preview-insert)
  (keymap-set completion-preview-active-mode-map "TAB"   #'completion-preview-insert)
  (keymap-set completion-preview-active-mode-map "C-n"   #'completion-at-point)
  (keymap-set completion-preview-active-mode-map "C-p"   #'completion-preview-prev-candidate))
(add-hook 'after-init-hook #'global-completion-preview-mode)

;; ── Eshell ──
(add-hook 'eshell-mode-hook
          (lambda () (setq-local completion-cycle-threshold t)))

;; ── Corfu ──
(with-eval-after-load 'corfu
  (keymap-set corfu-map "C-n" #'corfu-next)
  (keymap-set corfu-map "C-p" #'corfu-previous))
(setq corfu-auto          nil
      corfu-quit-no-match t)
(add-hook 'after-init-hook #'global-corfu-mode)

;; ── Eglot（Verilog） ──
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(verilog-mode . ("slang-server" "--stdio"))))
(add-hook 'verilog-mode-hook #'eglot-ensure)

(provide 'completion)
;;; completion.el ends here
