;;; init.el --- Modular Emacs configuration entrance -*- lexical-binding: t -*-

;; Add submodule lisp direcotry to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Basic core settings
(require 'core-settings)

;; Font settings
(require 'fonts)

;; Verilog
(require 'verilog)

;; Completion
(require 'completion)

;; Meow
(add-hook 'after-init-hook #'(lambda ()
                              (require 'my-meow)
                              (meow-global-mode 1)))

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
