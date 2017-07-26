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

(defvar kc/required-packages
  (list 'ag
    'anaconda-mode
    'company
    'company-c-headers
    'company-anaconda
    'company-flx
    'company-quickhelp
    'diminish
    'f
    'flx
    'flx-ido
    'flycheck
    'flycheck-color-mode-line
    'flycheck-pos-tip
    'ido-ubiquitous
    'python-mode
    'p4
    's
    'smex
    'yasnippet
    'ergoemacs-mode
    'linum
    'sr-speedbar
    'minimap)
  "Libraries that should be installed by default.")

(dolist (package kc/required-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package)))

;;
;; Generally useful packages
;;
(require 'f)

;;
;; Custom settings file
;;
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

;;
;; auto-save and auto-backup
;;
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

;;
;; Start server to allow connections from emacsclient
;;
(server-start)

;;
;; Other useful stuff
;;
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
;; comment block (for normal people)
;;
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

(global-unset-key (kbd "M-'"))

(global-set-key (kbd "M-[") 'ergoemacs-toggle-camel-case)
(global-set-key (kbd "M-]") 'ergoemacs-toggle-letter-case)


;;
;; dark theme
;;
(load-theme 'tsdh-dark)

;; smaller font
(set-face-attribute 'default nil :font "Terminus-10" )
(set-frame-font "Terminus-10" nil t)

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

;;
(require 'linum)
(setq linum-format "%d ")
;;(global-linum-mode 1)
(defun toggle-global-linum-mode () (interactive) (if global-linum-mode (global-linum-mode -1) (global-linum-mode 1)))
(global-set-key (kbd "s-L") 'toggle-global-linum-mode)

;;
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)
(setq speedbar-show-unknown-files t) ; show all files
(setq speedbar-use-images nil) ; use text for buttons
(setq sr-speedbar-right-side nil) ; put on left side
(setq sr-speedbar-auto-refresh nil)

;;
(require 'minimap)
(setq minimap-window-location 'right)
(global-set-key (kbd "s-M") 'minimap-mode)

;; ibuffer
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
	 (require 'flycheck-color-mode-line)
	 (require 'flycheck-google-cpplint)
	 (diminish 'flycheck-mode)
	 (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
	 (flycheck-pos-tip-mode)
	 (setq-default flycheck-c/c++-googlelint-executable (f-join kc/plugins-directory "cpplint.py")
				   flycheck-disabled-checkers '(c/c++-clang)
				   flycheck-highlighting-mode 'sexps
				   flycheck-googlelint-verbose "3"
				   flycheck-googlelint-root (f-full "~/projects/development/licenser/source")
				   flycheck-googlelint-filter "-legal/copyright,-whitespace/braces,-whitespace/parens,-whitespace/newline,-whitespace/tab,-whitespace/indent,-build/header_guard,-whitespace/blank_line,-build/include"
				   flycheck-googlelint-linelength "120"
				   flycheck-gcc-language-standard "c++14"
				   )
	 (flycheck-add-next-checker 'c/c++-cppcheck
								'(warning . c/c++-googlelint))
	 )
  )

;; add include folders
(add-hook 'c++-mode-hook
 (lambda ()
  (add-to-list 'flycheck-gcc-include-path (f-full "~/projects/local/sandbox/sence/obj/libs/contrib/boost"))
  (add-to-list 'flycheck-gcc-include-path (f-full "~/projects/local/sandbox/sence/obj/libs/contrib/cascade/include"))
  (add-to-list 'flycheck-gcc-include-path (f-full "~/projects/development/licenser/obj/libs/contrib/cascade/include"))
  ))

;; Flycheck Python Flake8
(setq-default flycheck-flake8-maximum-line-length 120)

;;
;; The silver searcher
;;
(require 'ag)
;; Rename ag buffers for easier access
(defun ag/buffer-name (search-string directory regexp)
  "Return a buffer name formatted according to ag.el conventions."
  (format "*ag: %s (%s)*" search-string directory))

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
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python2" . python-mode))

;;
;; Company mode
;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load "company"
 '(progn
   (add-to-list 'company-backends 'company-c-headers)
   ;; (add-to-list 'company-backends 'company-anaconda)
   ))
(with-eval-after-load 'company
  (company-flx-mode +1)
  (diminish 'company-mode))

(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0)

(company-quickhelp-mode 1)

;;
;; Fix <TAB> for YASnippet + Company
;; http://thrownforaloop.com/posts/emacs-configuration/
;;
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(defun bind-tab-properly ()
  "Binds tab to tab-indent-or-complete, overwritting yas and company bindings"
  (interactive)
  ;;overwrite yas and company tab mappings
  (define-key yas-minor-mode-map (kbd "<tab>") 'tab-indent-or-complete)
  (define-key yas-minor-mode-map (kbd "TAB") 'tab-indent-or-complete)
  (define-key company-active-map [tab] 'tab-indent-or-complete)
  (define-key company-active-map (kbd "TAB") 'tab-indent-or-complete)
)

(add-hook 'company-mode-hook 'bind-tab-properly)

;;
;; C++
;;
(defun company-c-header-init ()
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/5.2.1/")
  (add-to-list 'company-c-headers-path-user (f-full "~/projects/local/sandbox/sence/obj/libs/contrib/boost"))
  (add-to-list 'company-c-headers-path-user (f-full "~/projects/local/sandbox/sence/obj/libs/contrib/cascade/include"))
  )

(add-hook 'c-mode-hook 'company-c-header-init)
(add-hook 'c++-mode-hook 'company-c-header-init)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))


(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "C-<f5>") 'compile)

(defun kc-convenient-compile ()
 (interactive)
 (call-interactively 'compile)
 (global-set-key (kbd "<f5>") 'recompile))
(global-set-key (kbd "<f5>") 'kc-convenient-compile)

;; usefull compilance view
(setq compilation-scroll-output t)
(setq-default display-buffer-reuse-frames t)


;; Tabs Instead of Spaces
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;
;; Debugging
;;
;; (http://kousik.blogspot.ru/2005/10/highlight-current-line-in-gdbemacs.html)
;; + (https://www.emacswiki.org/emacs/DebuggingWithEmacs)
(defvar gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")

(defadvice gud-display-line (after my-gud-highlight act)
  "Highlight current line."
  (let* ((ov gud-overlay)
         (bf (gud-find-file true-file)))
    (with-current-buffer bf
      (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                    ;;(move-overlay ov (line-beginning-position) (line-end-position)
                    (current-buffer)))))

(defun gud-kill-buffer ()
  (if (derived-mode-p 'gud-mode)
      (delete-overlay gud-overlay)))

(add-hook 'kill-buffer-hook 'gud-kill-buffer)

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

;; (global-set-key (kbd "s-F") 'find-tag-other-window)
;; (global-set-key (kbd "s-G") 'view-tag-other-window)
;; (global-set-key (kbd "s-R") 'tags-query-replace)

;;
;; p4
;;
(require 'p4)

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
