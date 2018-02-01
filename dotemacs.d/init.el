;;  Requires at least Emacs 24.3 to work properly.
;;  It's better to use latest stable release:
;;  I'm trying to keep all my emacs installations up to date.

;;; Code:
;; Do not show useless buffers on startup

;; (setq debug-on-error nil)

(package-initialize)

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

(defvar kc/required-packages
  (list
    'ag
    'wgrep
    'ivy
    'swiper
    'counsel
    'spacemacs-theme
    'spaceline
    'smex
    'ergoemacs-mode
    'irony
    'irony-eldoc
    'cmake-mode
    'company
    'company-irony
    'company-irony-c-headers
    'company-flx
    'diminish
    'elpy
    'f
    'flx
    'flycheck
    'flycheck-irony
    'python-mode
    'p4
    's
    'sr-speedbar
    'yasnippet
    'yasnippet-snippets
    'lua-mode
    'nginx-mode)
  "Libraries that should be installed by default.")

(dolist (package kc/required-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package)))

;; Generally useful packages
(require 'f)

;; Custom settings file
(setq custom-file (f-join user-emacs-directory "emacs-custom.el"))
(if (f-exists? custom-file)
    (load custom-file))

;; Plugins
(defconst kc/plugins-directory (f-join user-emacs-directory "plugins")
  "Plugins Directory.")

(if (f-dir? kc/plugins-directory)
    (add-to-list 'load-path kc/plugins-directory))

;; Data directory
(defconst kc/emacs-persistence-directory (f-join user-emacs-directory "data")
  "Directory for Emacs data files and other garbage.")

(unless (f-dir? kc/emacs-persistence-directory)
  (f-mkdir kc/emacs-persistence-directory))

;; auto-save and auto-backup
(require 'desktop)
(setq-default desktop-dirname (f-join kc/emacs-persistence-directory "desktop"))
(unless (f-dir? desktop-dirname)
  (f-mkdir desktop-dirname))
(setq-default desktop-base-file-name "emacs.desktop")
(setq-default desktop-base-lock-name "emacs.desktop.lock")
(setq desktop-path (list desktop-dirname))
(desktop-save-mode t)

(setq auto-save-list-file-prefix (f-join kc/emacs-persistence-directory "auto-save-list/saves-"))
(setq make-backup-files nil)
(defconst autosave-directory (f-join kc/emacs-persistence-directory "autosave"))
(unless (f-dir? autosave-directory)
  (f-mkdir autosave-directory))
(setq auto-save-file-name-transforms
      `((".*" ,autosave-directory t)))

;; log recent files (recentf-open-files will list them all)
(recentf-mode t)
(setq-default recentf-save-file (f-join kc/emacs-persistence-directory "recentf"))
;; highlight current line
(global-hl-line-mode t)
(setq-default savehist-file (f-join kc/emacs-persistence-directory "minibuffer.history"))
;; save minibuffer history
(savehist-mode 1)
;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode t)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4)
;;
(setq-default c-basic-offset 4)
;; display tab symbol
(standard-display-ascii ?\t "→\t")
;; (standard-display-ascii ?\r "^M")
;; (standard-display-ascii ?\n "^J\n")

;; Show column numbers
(column-number-mode t)
;; And matching parens
(show-paren-mode t)
(require 'paren)
(setq blink-matching-paren-distance nil)
(setq show-paren-style 'mixed)

;; delete trailing spaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)



;;
;; dark theme
;;
(load-theme 'spacemacs-dark t)
(setq spacemacs-theme-org-agenda-height nil)
(setq spacemacs-theme-org-height nil)

(setq powerline-default-separator 'arrow-fade)
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; (set-frame-font "-xos4-terminus-medium-r-normal--20-*-*-*-*-*-*-*" nil t)
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 130
                    :weight 'normal
                    :width 'normal)


(setq ergoemacs-theme nil)
(setq ergoemacs-keyboard-layout "us")
(require 'ergoemacs-mode)
(ergoemacs-mode 1)


;;
;; Common keys
;;
(global-set-key (kbd "C-S-r") 'revert-buffer)     ;; C-r

(global-unset-key (kbd "M-'"))
(global-set-key (kbd "M-[") 'ergoemacs-toggle-camel-case)
(global-set-key (kbd "M-]") 'ergoemacs-toggle-letter-case)

;; comment block (for normal people)
(defun comment-eclipse ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)
    ))

(global-set-key (kbd "M-/") 'comment-eclipse)


;; built-in
(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))

(global-set-key (kbd "<f2>") 'bs-show)


;;
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)
(setq speedbar-show-unknown-files t) ; show all files
(setq speedbar-use-images nil) ; use text for buttons
(setq sr-speedbar-right-side nil) ; put on left side
(setq sr-speedbar-auto-refresh nil)


;;
(require 'ibuffer)
;; (defalias 'list-buffers 'ibuffer)  ;; make ibuffer default
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("projects"

         ("p4"          (name . "^\\*P4"))

         ;; source
         ("development/*"  (filename . "/projects/development/"))
         ("depot/*"  (filename . "/projects/depot/"))
         ("coretech/*"  (filename . "/projects/coretech/"))
         ("sandbox/*"     (filename . "/local/sandbox/"))
         ("local/*"           (filename . "/local/"))
         ("projects/*"        (filename . "/projects/"))

         ("emacs" (or
                        (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")))
         ("emacs.d/*" (or
                        (filename . ".emacs.d")
                        (filename . "emacs-config")
                        (filename . ".*/.*.el$")))
         ;; other stuff
         ("ag"          (name . "^\\*ag"))
         ("dired"       (mode . dired-mode))
         ("log$"        (filename . ".*\.log$"))
         ("*.*"         (name . "\*.*\*"))
         )))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "projects")
             (add-to-list 'ibuffer-never-show-predicates "TAGS")
             (ibuffer-auto-mode 1)))


;;
(require 'ag)
;; Rename ag buffers for easier access
(defun ag/buffer-name (search-string directory regexp)
  "Return a buffer name formatted according to ag.el conventions."
  (format "*ag: %s (%s)*" search-string directory))


;;
;; Yasnippet
;;
(require 'yasnippet)
(yas-global-mode 1)
(diminish 'yas-minor-mode)


;;
;; Ivy
;;
(ivy-mode 1)

(setq ivy-use-virtual-buffers t ; treat recentf, bookmarks as virtual buffers.
        ivy-extra-directories nil ; remove . and .. directory.
        ivy-count-format "")

(setq ivy-ignore-buffers '("^\\*scratch\\*$"
                  "^\\*Messages\\*$"
                  "^\\*Ibuffer\\*$"
                  "^\\*buffer-selection\\*$"
                  "^\\*Flycheck error messages\\*$"
                  "^\\*Quail Completions\\*$"))

(global-set-key (kbd "C-o") 'counsel-find-file)
(global-set-key (kbd "C-b") 'ivy-switch-buffer)
(global-set-key (kbd "M-y") 'swiper)
(global-set-key (kbd "M-Y") 'swiper)
(global-set-key (kbd "C-f") 'swiper)
(global-set-key (kbd "M-a") 'counsel-M-x)
(global-set-key (kbd "C-r") 'ivy-resume)
(global-set-key (kbd "C-c C-o") 'ivy-occur)

(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(define-key ivy-minibuffer-map (kbd "M-i") nil)
(define-key ivy-minibuffer-map (kbd "M-k") nil)
(define-key ivy-minibuffer-map (kbd "C-z") nil)
(define-key ivy-minibuffer-map (kbd "C-x") nil)
(define-key ivy-minibuffer-map (kbd "C-c") nil)
(define-key ivy-minibuffer-map (kbd "C-v") nil)


;;
;; Diminish
;;
(require 'diminish)
(defun diminished-modes ()
  "Echo all active diminished or minor modes as if they were minor.
The display goes in the echo area; if it's too long even for that,
you can see the whole thing in the *Messages* buffer.
This doesn't change the status of any modes; it just lets you see
what diminished modes would be on the mode-line if they were still minor."
  (interactive)
  (let ((minor-modes minor-mode-alist)
        message)
    (while minor-modes
      (when (symbol-value (caar minor-modes))
        ;; This minor mode is active in this buffer
        (let* ((mode-pair (car minor-modes))
               (mode (car mode-pair))
               (minor-pair (or (assq mode diminished-mode-alist) mode-pair))
               (minor-name (cadr minor-pair)))
          (when (symbolp minor-name)
            ;; This minor mode uses symbol indirection in the cdr
            (let ((symbols-seen (list minor-name)))
              (while (and (symbolp (callf symbol-value minor-name))
                          (not (memq minor-name symbols-seen)))
                (push minor-name symbols-seen))))
          (push minor-name message)))
      (callf cdr minor-modes))
    ;; Handle :eval forms
    (setq message (mapconcat
                   (lambda (form)
                     (if (and (listp form) (eq (car form) :eval))
                         (apply 'eval (cdr form))
                       form))
                   (nreverse message) ""))
    (when (= (string-to-char message) ?\ )
      (callf substring message 1))
    (message "%s" message)))


;;
;; Flycheck
;;
(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(progn
	 (require 'flycheck-google-cpplint)
	 (diminish 'flycheck-mode)
	 (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
	 (setq-default flycheck-c/c++-googlelint-executable (f-join kc/plugins-directory "cpplint.py")
				   flycheck-python-flake8-executable (f-full "~/.local/bin/flake8")
				   flycheck-python-pylint-executable (f-full "~/.local/bin/pylint")
				   flycheck-disabled-checkers '(c/c++-clang)
				   flycheck-highlighting-mode 'sexps
				   flycheck-googlelint-verbose "3"
				   flycheck-googlelint-filter "-legal/copyright,-whitespace/braces,-whitespace/parens,-whitespace/newline,-whitespace/tab,-whitespace/indent,-build/header_guard,-whitespace/blank_line,-build/include"
				   flycheck-googlelint-linelength "120"
				   flycheck-gcc-language-standard "c++14"
				   flycheck-python-pycompile-executable "python3"
				   )
	 (flycheck-add-next-checker 'c/c++-cppcheck
								'(warning . c/c++-googlelint))
	 )
  )


;;
;; Company mode
;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load "company"
 '(progn
   (require 'company-irony)
   (require 'company-irony-c-headers)
   (require 'company-yasnippet)
   (setq company-backends (remove 'company-semantic company-backends))
   (setq company-backends (remove 'company-clang company-backends))
   (add-to-list 'company-backends '(company-irony company-irony-c-headers company-yasnippet))
))

(with-eval-after-load 'company
  (setq-default company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (company-flx-mode +1)
  (diminish 'company-mode)
)

(define-key company-active-map [escape] 'company-abort)
(define-key company-active-map (kbd "M-i") 'company-select-previous)
(define-key company-active-map (kbd "M-k") 'company-select-next)


;;
;; Dired
;;
(add-hook 'dired-mode-hook
 (lambda ()
  (local-unset-key (kbd "C-o"))
 ))


;;
;; Isearch
;;
(add-hook 'isearch-mode-hook
 (lambda ()
  (local-unset-key (kbd "M-y"))
 ))



;;
;; C++
;;
(add-hook 'c++-mode-hook
 (lambda ()
  (irony-mode t)
  (add-to-list 'flycheck-gcc-include-path "/usr/include/c++/5.4.0")
  (add-to-list 'flycheck-gcc-include-path "/usr/include/boost")

  (local-unset-key (kbd "M-a"))
  (local-unset-key (kbd "M-e"))
  (local-unset-key (kbd "M-j"))
  (local-unset-key (kbd "M-q"))
))

(defun kc-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async)
)

(add-hook 'irony-mode-hook 'kc-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(add-hook 'irony-mode-hook #'irony-eldoc)

;; Tabs Instead of Spaces
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "C-<f5>") 'compile)

(defun kc-convenient-compile ()
 (interactive)
 (call-interactively 'compile)
 (global-set-key (kbd "<f5>") 'recompile)
 )

(global-set-key (kbd "<f5>") 'kc-convenient-compile)

;; usefull compilance view
(setq compilation-scroll-output t)
(setq-default display-buffer-reuse-frames t)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))

; Add cmake listfile names to the mode list.
(setq auto-mode-alist
  (append
  '(("CMakeLists\\.txt\\'" . cmake-mode))
  '(("\\.cmake\\'" . cmake-mode))
  auto-mode-alist))



;;
;; Python
;;
(require 'python)
(require 'elpy)
(add-hook 'python-mode-hook
 '(lambda ()
  (elpy-enable)
  (elpy-use-cpython "/usr/bin/python3")
  (set (make-local-variable 'company-backends) '(elpy-company-backend company-yasnippet))
 ))


;;
;; Perl
;;

;; use cperl-mode instead of perl-mode
(setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.\\(p\\([lm]\\)\\)\\'" . cperl-mode))

(setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))


;;
;; p4
;;
(require 'p4)

;;
(require 'nginx-mode)

;;
;; Robot mode (https://github.com/sakari/robot-mode)
;;
(load-file (f-join kc/plugins-directory "robot-mode.el"))
(add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))



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

;;
;; undisabled commands
;;
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
