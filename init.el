; Load slime
(add-to-list 'load-path "~/.emacs.d/third/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/bin/sbcl")

; Load evil
(setq evil-want-C-u-scroll t)
(add-to-list 'load-path "~/.emacs.d/third/evil")
(require 'evil)
(evil-mode 1)

; Load vmodule
(add-to-list 'load-path "~/.emacs.d/my/vmodule")
(require 'vmodule)

; Set frame
(add-to-list 'default-frame-alist '(foreground-color . "#c0c0c0"))
(add-to-list 'default-frame-alist '(background-color . "#303030"))
;(add-to-list 'default-frame-alist '(cursor-color . "coral"))
(blink-cursor-mode 0)
(setq fancy-splash-image "~/.emacs.d/theme/2.png")
(set-frame-font "DejaVu Sans Mono 18" nil t)
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

; Disable bars
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

; Line no wrap
(set-default 'truncate-lines t)

; Indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
;(setq indent-line-function 'insert-tab)

; Auto-save files directory
;(setq make-backup-files nil)
(let ((backup-dir "~/.emacs.d/backups")
      (auto-saves-dir "~/.emacs.d/autosaves/"))
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

; No symolic link lockfiles in current directory
(setq create-lockfiles nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(verilog-auto-indent-on-newline nil)
 '(verilog-auto-lineup 'all)
 '(verilog-auto-newline nil)
 '(verilog-case-indent 0)
 '(verilog-cexp-indent 0)
 '(verilog-indent-begin-after-if nil)
 '(verilog-indent-level 0)
 '(verilog-indent-level-behavioral 0)
 '(verilog-indent-level-declaration 0)
 '(verilog-indent-level-directive 0)
 '(verilog-indent-level-module 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:foreground "yellow")))))

; Set font-lock faces
(set-face-bold 'font-lock-keyword-face "bold")
(set-face-foreground 'font-lock-comment-face "#6495ED")
;(add-to-list 'face-remapping-alist '(font-lock-comment-face . ((t (:foreground "blue")))))

