;;; init --- my init file

;;; Commentary:

;;  Requires at least Emacs 24.3 to work properly.
;;  It's better to use latest stable release:
;;  I'm trying to keep all my emacs installations up to date.

;;; Code:
;; Do not show useless buffers on startup
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t)

;; Hide all useless stuff
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
;; And maximize window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Extend path variable and exec-path
(when (eq system-type 'darwin)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  ; TODO add as a last element
  (add-to-list 'exec-path "/usr/local/bin"))

(defun eor/concat-list (lst separator)
  "A non-recursive function that concatenates a list of strings."
  (if (listp lst)
      (let ((result ""))
        (dolist (item lst)
          (if (stringp item)
	      (if (and (stringp separator) (not (eq result "")))
		  (setq result (concat result separator item))
		(setq result (concat result item)))
	    (prin1 item)))
        result)))

(defconst eor/python-path
  (list
   "/Users/vprotasov/dev/web/Sources/LicensingService/Backend/lib/python"
   "/Users/vprotasov/dev/web/Sources/Common/Backend/myaccount_client/python"
   "/Users/vprotasov/dev/web/Sources/MyAccount"
   "/Users/vprotasov/dev/web/Sources/Common/Backend/pax_client/python"
   "/Users/vprotasov/dev/web/Sources/Common/Backend/pd_private_api_client"
   "/Users/vprotasov/dev/web/Sources/Desktop/Backend/python"
   "/Users/vprotasov/dev/web/Sources/Portal"
   "/Users/vprotasov/dev/web/Sources/Common/Backend"
   "/Users/vprotasov/dev/web/Sources/RAS/Backend/lib/python"
   "/Users/vprotasov/dev/web/Sources/SWP/PrivateAPIClient/lib/python"
   "/Users/vprotasov/dev/web/Sources/Common/Backend/www_private_api_client"
   ""))

(setenv "PYTHONPATH" (eor/concat-list eor/python-path ":"))

;;
;; package.el
;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar eor/required-packages
  (list 'ag
	'anaconda-mode
	'autopair
	'company
	'company-anaconda
	'company-flx
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
	'yasnippet)
  "Libraries that should be installed by default.")

(dolist (package eor/required-packages)
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
(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
;; Load custom file only if one is exists
(if (file-exists-p custom-file)
    (load custom-file))

;; Data directory
(defconst emacs-persistence-directory (concat user-emacs-directory "data/")
  "Directory for emacs data files and other garbage")
(unless (file-exists-p emacs-persistence-directory)
    (make-directory emacs-persistence-directory t))

;;
;; auto-save and auto-backup
;;
(desktop-save-mode t)
(setq make-backup-files nil)


;;
;; Start server to allow connections from emacsclient
;;
(server-start)

;;
;; Other useful stuff
;;


;; Show column numbers
(column-number-mode t)
;; And matching parens
(show-paren-mode t)
(require 'paren)
(setq blink-matching-paren-distance nil)
(setq show-paren-style 'mixed)

;; Which function mode
;; http://www.masteringemacs.org/articles/2011/11/19/which-function-mode/
(which-function-mode t)
;; Do not blink the buffer!!!!11111
(setq visible-bell nil)

;; Make eshell store its files in .emacs.d
(require 'eshell)
(setq eshell-directory-name (concat emacs-persistence-directory "eshell"))
;; Explicitly set temporary folder
(setq temporary-file-directory (expand-file-name "~/.tmp"))
(unless (file-accessible-directory-p temporary-file-directory)
  (make-directory temporary-file-directory))

;; delete trailing spaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; dark theme
(load-theme 'tsdh-dark)
;; smaller font
(set-face-attribute 'default nil :height 100)


;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(global-set-key (kbd "s-b") 'ido-switch-buffer)
(setq ibuffer-expert t)
(require 'ibuf-ext)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("work"
	 ;; Parallels web
	 ("Parallels Desktop" (filename . ".*/web/Sources/Desktop/Backend/python/pd_server/.*"))
	 ("License Service" (filename . ".*/web/Sources/LicensingService/Backend/lib/python/ls_server/.*"))
	 ("Remote Access Service" (filename . ".*/web/Sources/RAS/Backend/lib/python/ras_server/.*"))
	 ("Web Common" (filename . ".*/web/Sources/Common/Backend/.*"))
	 ("Other web" (filename . ".*/web/Sources/.*"))
	 ;; Emacs configuration
	 ("emacs.d" (or (filename . ".emacs.d")
                             (filename . "emacs-config")
                             (filename . ".*/.*.el$")))
	 ;; Org-mode files
         ("org" (mode . org-mode))
	 ;; Other stuff
         ("dired" (mode . dired-mode))
         ("magit" (name . "\*magit"))
	 ("ag" (name . "\*ag"))
         ("logs" (filename . ".*\.log$"))
	 ("*..*" (name . "\*.*\*"))

         )))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "work")
             (add-to-list 'ibuffer-never-show-predicates "TAGS")
             (add-to-list 'ibuffer-never-show-predicates ".*org_archive")
	     (ibuffer-auto-mode 1)))


;;
;; Ido
;;
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
;; flx completion
(require 'flx-ido)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
;; Ido ubiquitous
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "s-x") 'smex-major-mode-commands)
;; annoying prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
;; move ido.last to data directory
(setq ido-save-directory-list-file (concat emacs-persistence-directory "ido.last"))
;;
;; Projectile
;;
(require 'projectile)
(defconst projectile-data-dir (concat emacs-persistence-directory "projectile/"))
(setq projectile-known-projects-file (expand-file-name "bookmarks.eld" projectile-data-dir))
(setq projectile-cache-file (expand-file-name "projectile.cache" projectile-data-dir))
(setq projectile-keymap-prefix (kbd "s-p"))
(projectile-global-mode)


;;
;; Magit
;;
(require 'magit)
(global-set-key (kbd "s-m") 'magit-status)
(setq
 ;; use ido to look for branches
 magit-completing-read-function 'magit-ido-completing-read
 ;; don't put "origin-" in front of new branch names by default
 magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
 ;; highlight word/letter changes in hunk diffs
 magit-diff-refine-hunk t
 ;; ask me if I want to include a revision when rewriting
 magit-rewrite-inclusive 'ask
 ;; ask me to save buffers
 magit-save-some-buffers t
 ;; pop the process buffer if we're taking a while to complete
 magit-process-popup-time 10
 ;; ask me if I want a tracking upstream
 magit-set-upstream-on-push t
 ;; don't show " MRev" in modeline
 magit-auto-revert-mode-lighter ""
 )
;;
;; Flycheck
;;
(add-hook 'after-init-hook #'global-flycheck-mode)

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

;;
;; Python
;;
(require 'python)
(defun projectile-venv-activate ()
  (interactive "P")
  (when (projectile-project-p)
    (let ((project-venv (f-join (projectile-project-root) "venv")))
      (when (f-dir? project-venv)
	(setq python-shell-virtualenv-path project-venv)))))

(add-hook 'python-mode-hook 'projectile-venv-activate)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
;;(pyenv-mode)

;;
;; Auto-complete
;;
;;(require 'auto-complete-config)
;;(setq ac-comphist-file "~/.emacs.d/data/ac-comphist.dat")
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;(ac-config-default)
;;(add-hook 'python-mode-hook 'ac-anaconda-setup)

;;
;; Company mode
;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load "company"
 '(progn
   (add-to-list 'company-backends 'company-anaconda)))
(with-eval-after-load 'company
  (company-flx-mode +1))
;;
;; Autopair
;;
(require 'autopair)
(autopair-global-mode t)
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))



;;
;; header2
;;
(require 'header2)
(add-to-list 'python-mode-hook 'auto-make-header)
;; should be set in .dir-locals file in project root
(put 'eor/prl/project 'safe-local-variable 'stringp)

(defsubst eor/header-triple-quotes ()
  "Insert triple quotes."
  (insert "\"\"\"\n"))

(defsubst eor/header-python-no-prefix-string ()
  "Disable comment prefix for python files."
  (setq header-prefix-string ""))

(defsubst eor/prl/header-file-name ()
  "Insert \"File: \" line, using buffer's file name."
  (insert "File: "
	  (if (buffer-file-name)
              (file-name-nondirectory (buffer-file-name))
	    (buffer-name))
	  "\n"))
(defsubst eor/prl/header-copyright ()
  "Insert copyright."
  (insert "Copyright:\n"
	  "Copyright " (format-time-string "%Y-%Y. ") "Parallels IP Holdings GmbH. All Rights Reserved.\n"))

(defun eor/prl/header-python ()
  "Insert docstring with copyright for python files in parallels projects."
  (when (and (boundp 'eor/prl/project) (eq major-mode 'python-mode))
    (eor/header-no-prefix-string)
    (eor/header-triple-quotes)
    (eor/prl/header-file-name)
    (header-blank)
    (eor/prl/header-copyright)
    (eor/header-triple-quotes)))

(setq make-header-hook '(eor/prl/header-python))


(defun eor/prl/header-update-copyright ()
  "Update copyright."
  (when (boundp 'eor/prl/project)
    (move-beginning-of-line nil)
    (forward-char 15)
    (delete-region (point) (progn (forward-word nil) (point)))
    (insert (format-time-string "%Y"))))
(register-file-header-action "^Copyright [0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]" 'eor/prl/header-update-copyright)

(add-hook 'write-file-hooks 'auto-update-file-header)


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
