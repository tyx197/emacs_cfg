;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t -*-

(let ((elpa-dir (expand-file-name "elpa/" user-emacs-directory)))
  ;; Find dir start with benchmark-init-
  (let ((match (directory-files elpa-dir t "^benchmark-init-[0-9.]+" t)))
    (when match
      ;; Get the first mached
      (add-to-list 'load-path (car match))
      (require 'benchmark-init nil t)
      (when (fboundp 'benchmark-init/activate)
        (benchmark-init/activate)))))

;; Global default font variable
(defvar my-default-font (getenv "EMACS_FONT")
  "Default font for fixed and variable pitch, CJK, etc.")

(defun my-parse-number (str)
  "Try convert string to number. Return nil if failed."
  (when (and (stringp str) (not (string-empty-p str)))
    (string-to-number str)))

(defvar my-default-frame-width (my-parse-number (getenv "EMACS_WIDTH"))
  "Default frame width.")

(defvar my-default-frame-height (my-parse-number (getenv "EMACS_HEIGHT"))
  "Default frame height.")

(defvar my-default-frame-top (my-parse-number (getenv "EMACS_TOP"))
  "Default frame top.")

(defvar my-default-frame-left (my-parse-number (getenv "EMACS_LEFT"))
  "Default frame left.")

(defvar my-http-proxy (getenv "http_proxy")
  "HTTP proxy URL.")

(defvar my-https-proxy (getenv "https_proxy")
  "HTTPS proxy URL.")

(defvar my-no-proxy (getenv "no_proxy")
  "No proxy URL.")

(defun my-convert-no-proxy-to-regexp (no-proxy-str)
  "Convert host splited by comma to emacs regexp."
  (when (and (stringp no-proxy-str) (not (string-empty-p no-proxy-str)))
    (let ((items (mapcar (lambda (s) (string-remove-prefix "." s))
                         (split-string no-proxy-str "," t))))
      (format "\\(?:^\\|\\.\\)%s$" (regexp-opt items)))))

(defun my-proxy-strip-protocol (proxy-str)
  "Trim http:// or https:// in string."
  (replace-regexp-in-string "^https?://\\|/" "" proxy-str))

(defvar my-emacs-repo (getenv "EMACS_REPO")
  "Custom Emacs repo URL.")

(defun my-repo-ensure-slash (url)
  "Make sure repo URL is ended with slash."
  (when (and (stringp url) (not (string-empty-p url)))
    (if (string-suffix-p "/" url) url (concat url "/"))))

;; Proxy
(setq url-proxy-services nil)

(when my-http-proxy
  (let ((host (my-proxy-strip-protocol my-http-proxy)))
    (when host (push `("http_proxy" . ,host) url-proxy-services))))

(when my-https-proxy
  (let ((host (my-proxy-strip-protocol my-https-proxy)))
    (when host (push `("https_proxy" . ,host) url-proxy-services))))

(when my-no-proxy
  (let ((host (my-convert-no-proxy-to-regexp my-no-proxy)))
    (when host (push `("no_proxy" . ,host) url-proxy-services))))

;; Package repo
(let ((my-base-url (my-repo-ensure-slash my-emacs-repo)))
  (when my-base-url
    (setq package-archives
          `(("gnu"    . ,(concat my-base-url "gnu/"))
            ("nongnu" . ,(concat my-base-url "nongnu/"))
            ("melpa"  . ,(concat my-base-url "melpa/"))))))

;; Frame width/height and position.
(when my-default-frame-width
  (push `(width . ,my-default-frame-width) default-frame-alist))
(when my-default-frame-height
  (push `(height . ,my-default-frame-height) default-frame-alist))
(when my-default-frame-top
  (push `(top . ,my-default-frame-top) default-frame-alist))
(when my-default-frame-left
  (push `(left . ,my-default-frame-left) default-frame-alist))

;; Disable GUI miscs.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq-default blink-cursor-mode nil)
(setq-default frame-inhibit-implied-resize t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Setting frame font
(when my-default-font
  (push `(font . ,my-default-font) default-frame-alist))

;; Add load path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
