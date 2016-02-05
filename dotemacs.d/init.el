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
    'company-c-headers
	'company-anaconda
	'company-flx
    'company-quickhelp
	'diminish
	'f
	'flx
	'flx-ido
	'flycheck
    'flycheck-pos-tip
    'flycheck-color-mode-line
    'flycheck-google-cpplint
    'fill-column-indicator
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
    'neotree
	'yaml-mode
    'auto-complete
    'auto-complete-c-headers
    'iedit
    'google-c-style
    'cedet)
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
    (comment-or-uncomment-region start end)))

;;(global-set-key (kbd "M-'") 'comment-dwim)
(global-set-key (kbd "M-'") 'comment-eclipse)


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
(global-set-key (kbd "s-L") 'toggle-global-linum-mode)


;;
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)


;;
(require 'minimap)
(setq minimap-window-location 'right)

(global-set-key (kbd "s-M") 'minimap-mode)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


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
         ("tlib/*"      (filename . ".*/Projects/tlib/.*"))
         ("Crawler/*"   (filename . ".*/Projects/python/Crawler/.*"))
         ("github/*"    (filename . ".*/Projects/github/.*"))
         ("bitbucke/*t" (filename . ".*/Projects/bitbucket/.*"))
         ("sandbox/*"   (filename . ".*/Projects/sandbox/.*"))
         ("python/*"    (filename . ".*/Projects/python/.*"))
         ("Projects/*"  (filename . ".*/Projects/.*"))
         ("emacs" (or
                        (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")))
         ("emacs.d/*" (or
                        (filename . ".emacs.d")
                        (filename . "emacs-config")
                        (filename . ".*/.*.el$")))
         ;; other stuff
         ("dired"       (mode . dired-mode))
         ("log$"        (filename . ".*\.log$"))
         ("*.*"         (name . "\*.*\*"))
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


;;
;; Projectile
;;
(require 'projectile)
(setq projectile-keymap-prefix (kbd "s-P"))
(projectile-global-mode)
(eval-after-load "projectile" '(diminish 'projectile-mode))

(defun eor/project/file-name ()
  "Return file name relative to project root."
  (when (projectile-project-p)
    (s-chop-prefix (projectile-project-root) buffer-file-name)))

(defun eor/project/copy-file-name ()
  "Copy file name relative to project root."
  (interactive)
  (when (projectile-project-p)
    (kill-new (eor/project/file-name))))

(put 'eor/project/repo-browser-url-pattern 'safe-local-variable 'stringp)
(defun eor/project/get-repo-file-url ()
  "Open current file in browser (for github, gitlab, stash and so on)."
  (when (and (boundp 'eor/project/repo-browser-url-pattern)
	     (projectile-project-p))

    (let ((filename (eor/project/file-name))
	  (revision (vc-working-revision (buffer-file-name))))
      (s-replace "{filename}" filename
		 (s-replace "{revision}" revision
			    eor/project/repo-browser-url-pattern)))))

(defun eor/project/open-repo-file-in-browser ()
  "Open current file in repo browser."
  (interactive)
  (let ((url (eor/project/get-repo-file-url)))
    (when url
      (browse-url url))))


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
;; aHg
;;
(require 'ahg)
(global-set-key (kbd "s-H") 'ahg-status)


;;
;; Git
;;
(add-to-list 'load-path "~/.emacs.d/plugins/git/contrib/emacs")
(require 'git)
(global-set-key (kbd "s-G") 'git-status)

;;
;; Magit
;;
;;(require 'magit)
;;(global-set-key (kbd "s-g") 'magit-status)
;;(setq
;; ;; use ido to look for branches
;; magit-completing-read-function 'magit-ido-completing-read
;; )


;;
;; Flycheck
;;
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load "flycheck" '(diminish 'flycheck-mode))

(with-eval-after-load 'flycheck
 (flycheck-pos-tip-mode))

(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(setq flycheck-highlighting-mode 'lines)

;; Flycheck C++
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(c/c++-clang)))

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

;; (eval-after-load 'flycheck
;;   '(progn
;;      (require 'flycheck-google-cpplint)
;;      ;; Add Google C++ Style checker.
;;      ;; In default, syntax checked by Clang and Cppcheck.
;;      (flycheck-add-next-checker 'c/c++-gcc
;;                                 '(warnings-only . c/c++-googlelint))))
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-clang
                                'c/c++-googlelint 'append)))

(custom-set-variables
 '(flycheck-c/c++-googlelint-executable "/usr/bin/cpplint")
 '(flycheck-googlelint-verbose "3")
 '(flycheck-googlelint-filter "-legal/copyright")  ;; -readability/streams,-whitespace/operators,-legal/copyright,-whitespace,+whitespace/braces
 '(flycheck-googlelint-linelength "120"))

;;
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Flycheck Python Flake8
(setq-default flycheck-flake8-maximum-line-length 120)

(setq
 flycheck-python-flake8-executable "/usr/bin/flake8-python2"
 python-check-command "/usr/bin/pyflakes-python2"
 python-shell-interpreter "/usr/bin/python2")

;; for virtualenv
;; (setq
;;  flycheck-python-flake8-executable "~/Projects/Crawler/env/bin/flake8"
;;  python-check-command "~/Projects/Crawler/env/bin/pyflakes"
;;  python-shell-interpreter "~/Projects/Crawler/env/bin/python")


;;;;
;;;; FillColumnIndicator
;;;;
;;(require 'fill-column-indicator)
;;(setq fci-rule-column 120)
;;(setq fci-rule-width 2)
;;(setq fci-rule-color "darkgrey")
;;(define-globalized-minor-mode global-fci-mode fci-mode
;;  (lambda ()
;;    (if (and
;;         (not (string-match "^\*.*\*$" (buffer-name)))
;;         (not (eq major-mode 'dired-mode)))
;;        (fci-mode 1))))
;;(global-fci-mode 1)


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
(add-to-list 'interpreter-mode-alist '("python2" . python-mode))


;;
;; Company mode
;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load "company"
 '(progn
   (add-to-list 'company-backends 'company-c-headers)
   (add-to-list 'company-backends 'company-anaconda)))
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
  (define-key company-active-map (kbd "TAB") 'tab-indent-or-complete))

(add-hook 'company-mode-hook 'bind-tab-properly)


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
;; iedit
;;
(define-key global-map (kbd "s-R") 'iedit-mode)


;;
;; C++
;;
(defun style-c-indent-init ()
  (setq c-default-style "linux"
        c-basic-offset 4))
(add-hook 'c++-mode-hook 'style-c-indent-init)

(defun company-c-header-init ()
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/5.3.0/"))

(add-hook 'c++-mode-hook 'company-c-header-init)
(add-hook 'c-mode-hook 'company-c-header-init)

;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)

;; (defun ac-c-header-init ()
;;   (require 'auto-complete-c-headers)
;;   (add-to-list 'ac-sources 'ac-source-c-headers)
;;   (add-to-list 'achead:include-directories '"/usr/include/c++/5.3.0"))

;; (add-hook 'c++-mode-hook 'ac-c-header-init)
;; (add-hook 'c-mode-hook 'ac-c-header-init)

;; (semantic-mode 1)

;; (defun add-semantic-to-autocomplete()
;;   (add-to-list 'ac-sources 'ac-source-semantic))

;; (add-hook 'c-mode-common-hook 'add-semantic-to-autocomplete)

;; (global-ede-mode 1)

;; (ede-cpp-root-project "my project" :file "~/Projects/sandbox/cpp/binsearch/main.cpp"
;;                       :include-path '("/../includes"))

;; (global-semantic-idle-scheduler-mode 1)

(defun run-compile-command ()
 (interactive)
 (setq-local compilation-read-command nil)
 (call-interactively 'compile))

(global-set-key (kbd "<f5>") 'run-compile-command)


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
