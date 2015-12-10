;;; init --- my init file
;;;                                                                                               last change 09.12.2015

;;; Commentary:

;;  Requires at least Emacs 24.3 to work properly.
;;  It's better to use latest stable release:
;;  I'm trying to keep all my emacs installations up to date.


;;; Code:
;; Do not show useless buffers on startup
(setq inhibit-splash-screen t
    inhibit-startup-echo-area-message t)


;;
(setq show-paren-style 'expression)
(show-paren-mode 2)


;; Hide all useless stuff
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)


;; And maximize window
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; delete trailing spaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving


;; dark theme
(load-theme 'tsdh-dark)
;; fix for default dark theme in linux
(set-face-attribute 'font-lock-string-face nil :background "#333333")
(set-face-attribute 'font-lock-constant-face nil :background "#333333")
;;(set-face-attribute 'default nil :height 80)
(setq-default cursor-type 'bar)
(set-cursor-color "#ffffff")


;; built-in
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)


;; built-in
(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))

(global-set-key (kbd "<f2>") 'bs-show)


(add-to-list 'load-path "~/.emacs.d/plugins")


;; http://code.google.com/p/dea/source/browse/trunk/my-lisps/linum%2B.el
(require 'linum+)
(setq linum-format "%d ")
(global-linum-mode 1)


;; http://www.emacswiki.org/emacs/SrSpeedbar
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)


(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.


(add-to-list 'load-path "~/.emacs.d/plugins/ergoemacs-mode")
(require 'ergoemacs-mode)

(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
(ergoemacs-mode 1)

