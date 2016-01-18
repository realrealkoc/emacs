;;; init --- my init file
;;;                                                                                               last change 18.01.2016

;;; Commentary:

;;  Requires at least Emacs 24.3 to work properly.
;;  It's better to use latest stable release:
;;  I'm trying to keep all my emacs installations up to date.


;;; Code:
;; Do not show useless buffers on startup
(setq inhibit-splash-screen t
    inhibit-startup-echo-area-message t)


;; Hide all useless stuff
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; And maximize window
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;;
;; package.el
;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-directory-list (expand-file-name "elpa-to-submit" user-emacs-directory))
(when (not package-archive-contents)
    (package-refresh-contents))
(package-initialize)

(defvar eor/required-packages
  (list 'ag
	'anaconda-mode
	'autopair
	'company
	'company-anaconda
	'company-flx
	'diminish
	'f
	'flx
	'flx-ido
	'flycheck
	'magit
	'haskell-mode
	'header2
	'ido-ubiquitous
	'org-jira
	'projectile
	'pyenv-mode
	'python-mode
	's
	'smex
	'wgrep
	'wgrep-ag
	'yasnippet
	'ergoemacs-mode
    'linum
	'sr-speedbar
	'ahg
	'minimap
	'yaml-mode)
  "Libraries that should be installed by default.")

(dolist (package eor/required-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package)))


;;
;; Custom settings file
;;
(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
;; Load custom file only if one is exists
(if (file-exists-p custom-file)
    (load custom-file))


;;
;; auto-save and auto-backup
;;
(require 'desktop)
(desktop-save-mode t)

(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving


;;
;; Start server to allow connections from emacsclient
;;
(server-start)


;;
;; Other useful stuff
;;

;; log recent files (recentf-open-files will list them all)
(recentf-mode t)
;; highlight current line
(global-hl-line-mode t)
;; save minibuffer history
(savehist-mode 1)
;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4)
;; Show column numbers
(column-number-mode t)
;; And matching parens
(show-paren-mode t)
(require 'paren)
(setq blink-matching-paren-distance nil)
(setq show-paren-style 'mixed)


;; delete trailing spaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; dark theme
(load-theme 'tsdh-dark)
;; smaller font
(set-frame-font "Terminus-10")
;;(set-face-attribute 'default nil :height 100)


;;
(require 'ergoemacs-mode)
(setq ergoemacs-theme nil)
(setq ergoemacs-keyboard-layout "us")
(ergoemacs-mode 1)


;; built-in
(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))

(global-set-key (kbd "<f2>") 'bs-show)
(add-to-list 'load-path "~/.emacs.d/plugins")


;;
(require 'linum)
(setq linum-format "%d ")
;;(global-linum-mode 1)
(defun toggle-global-linum-mode () (interactive) (if global-linum-mode (global-linum-mode -1) (global-linum-mode 1)))
(global-set-key (kbd "C-M-S-l") 'toggle-global-linum-mode)


;;
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)


;;
(require 'minimap)
(setq minimap-window-location 'right)

(global-set-key (kbd "s-M") 'minimap-mode)


;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("Projects"
         ;; My Projects
         ("Crawler" (filename . ".*/Projects/Crawler/.*"))
         ("github" (filename . ".*/Projects/github/.*"))
         ("sandbox" (filename . ".*/Projects/sandbox/.*"))
         ("Other projects" (filename . ".*/Projects/.*"))
         ("emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")))
         ;; Emacs configuration
         ("emacs.d" (or (filename . ".emacs.d")
                        (filename . "emacs-config")
                        (filename . ".*/.*.el$")))
         ;; Other stuff
         ("dired" (mode . dired-mode))
         ("magit" (name . "\*magit"))
         ("logs" (filename . ".*\.log$"))
         ("*..*" (name . "\*.*\*"))
         )))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "Projects")
             (add-to-list 'ibuffer-never-show-predicates "TAGS")
             (ibuffer-auto-mode 1)))


;;
;; Ido
;;
(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)

(ido-everywhere 1)
;; flx completion
;;(require 'flx-ido)
;;(flx-ido-mode 1)
;; disable ido faces to see flx highlights.

;(setq ido-use-faces nil)
;; Ido ubiquitous
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)


;; Mode-line
(setq-default mode-line-format
 (list
  "   "
  ;; is this buffer read-only?
  '(:eval (if buffer-read-only
            (propertize "✎ "
                        'face '(:foreground "red" :height 70)
                        'help-echo "Buffer is read-only")
            (propertize "✎ "
                        'face '(:foreground "green" :height 70)
                        'help-echo "Buffer is writable")))
  ;; was this buffer modified since the last save?
  '(:eval (if (buffer-modified-p)
            (propertize "★ "
                        'face '(:height 70 :foreground "red")
                        'help-echo "Buffer has been modified")
            (propertize "★ "
                        'face '(:height 70 :foreground "green")
                        'help-echo "Buffer is saved")))
  "    "
  ;; the buffer name; the file name as a tool tip
  '(:eval (propertize "%b "
                      'face '(:weight bold)
                      'help-echo (buffer-file-name)))

  "    "

  mode-line-position

  "    "

  '(vc-mode vc-mode)
  ))

;;
;; aHg
;;
(require 'ahg)
(global-set-key (kbd "s-h") 'ahg-status)

;;
;; Magit
;;
(require 'magit)
(global-set-key (kbd "s-g") 'magit-status)
(setq
 ;; use ido to look for branches
 magit-completing-read-function 'magit-ido-completing-read
 )


;;
;; Flycheck
;;
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load "flycheck" '(diminish 'flycheck-mode))


;;
;; Yasnippet
;;
(yas-global-mode 1)
(diminish 'yas-minor-mode)


;;
;; Python
;;
(require 'python)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)


;;
;; Company mode
;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load "company"
 '(progn
   (add-to-list 'company-backends 'company-anaconda)))
(with-eval-after-load 'company
  (company-flx-mode +1)
  (diminish 'company-mode))


;;
;; Autopair
;;
(require 'autopair)
(autopair-global-mode t)
(diminish 'autopair-mode)
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))


;;
;;; View tags other window
;;
(defun view-tag-other-window (tagname &optional next-p regexp-p)
  "Same as `find-tag-other-window' but doesn't move the point"
  (interactive (find-tag-interactive "View tag other window: "))
  (let ((window (get-buffer-window)))
    (find-tag-other-window tagname next-p regexp-p)
    (recenter 0)
    (select-window window)))

;;(global-set-key (kbd "M-g") 'find-tag-other-window)
;;(global-set-key (kbd "M-G") 'view-tag-other-window)
;;(global-set-key (kbd "M-r") 'tags-query-replace)


;;
;; Switching layout when entering commands
;; http://ru-emacs.livejournal.com/82428.html
;;
(require 'quail)
(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method 'russian-computer)

(defadvice read-passwd (around my-read-passwd act)
  (let ((local-function-key-map nil))
    ad-do-it))

(provide 'init)
;;; init.el ends here
