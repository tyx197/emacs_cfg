;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t -*-

;; Global default font variable
(defvar my-default-font "UnifontExMono:pixelsize=32"
  "Default font for fixed and variable pitch, CJK, etc.")

;; Package repo
(setq package-archives '(("gnu"    . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa"  . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

;; Frame width/height and position.
(push '(width  .  120) default-frame-alist)
(push '(height .   50) default-frame-alist)
(push '(top    .   50) default-frame-alist)
(push '(left   . 1000) default-frame-alist)

;; Disable GUI miscs.
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0)   default-frame-alist)
(setq inhibit-startup-screen t)

;; Setting frame font
(push `(font . ,my-default-font) default-frame-alist)

;; Add load path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;(setq package-enable-at-startup nil)

(provide 'early-init)
