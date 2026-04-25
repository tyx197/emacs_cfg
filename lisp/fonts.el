;;; fonts.el --- Font faces & line spacing  -*- lexical-binding: t -*-

(defun my/set-font-faces ()
  "Set font faces for han, cjk-misc, fixed and variable pitch."
  (message "Setting faces!")
  (set-fontset-font "fontset-default" 'han        my-default-font)
  (set-fontset-font "fontset-default" 'cjk-misc   my-default-font)
  (set-face-attribute 'fixed-pitch nil    :font my-default-font)
  (set-face-attribute 'variable-pitch nil :font my-default-font :weight 'regular))

;; Line spacing
(setq-default line-spacing 0.2)

;; Setting fonts
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my/set-font-faces)
  (my/set-font-faces))

(provide 'fonts)
;;; fonts.el ends here
