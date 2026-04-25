;;; core-settings.el --- Language, backups, indentation, UI  -*- lexical-binding: t -*-

;; Setting coding
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Setting backup and auto-save directory
(let ((backup-dir      (expand-file-name "tmp/emacs/backups"    "~"))
      (auto-saves-dir  (expand-file-name "tmp/emacs/auto-saves" "~")))
  (dolist (dir (list backup-dir auto-saves-dir))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (setq backup-directory-alist           `(("." . ,backup-dir))
        auto-save-file-name-transforms   `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix       (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist     `((".*" . ,backup-dir))
        tramp-auto-save-directory         auto-saves-dir))

;; Disable backup and auto-save
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Indent
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq indent-line-function 'insert-tab)
(global-set-key (kbd "C-c i") 'indent-relative)

;; Theme load path
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; Enable global line numbers
(global-display-line-numbers-mode)

;; Disable ring bell
(setq ring-bell-function 'ignore)

(provide 'core-settings)
;;; core-settings.el ends here
