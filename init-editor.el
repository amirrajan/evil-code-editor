;; editor configuration
(use-package editorconfig
  :ensure t
  :config

  ;; remember cursor position in each buffer
  (save-place-mode 1)

  ;; disable extrenous menu items
  (menu-bar-mode -1)
  (setq inhibit-splash-screen t)

  ;; don't wrap lines
  (setq-default truncate-lines t)
  (setq create-lock-files nil)

  ;; set your backup directory so your working dir doesn't get polluted with ~ files
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

  ;; keep backups of the last 10 edits for every file
  (setq backup-by-copying t     ; don't clobber symlinks
        kept-new-versions 10    ; keep 10 latest versions
        kept-old-versions 10    ; don't bother with old versions
        delete-old-versions t   ; don't ask about deleting old versions
        version-control t       ; number backups
        vc-make-backup-files t) ; backup version controlled files

  (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
  (setq large-file-warning-threshold 20000000)

  ;; display line numbers in the gutter
  (global-display-line-numbers-mode)

  ;; keep track of most recently accessed files
  ;; so they show up higher in lists
  (recentf-mode 1)

  ;; auto load reverted files
  (setq global-auto-revert-non-file-buffers t)

  ;; show trailing whitespace
  (setq show-trailing-whitespace t)

  ;; remove extrenous info in file explorer
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (dired-hide-details-mode))))

;; fancy themes for emacs
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (load-theme 'doom-one t)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; wire up terminal clipboard to osx clipboard
(use-package osx-clipboard
  :ensure t
  :config
  (osx-clipboard-mode 1))

;; package trims white space, but only for lines you changed
(use-package ws-butler
  :ensure t
  :init
  (ws-butler-global-mode))

;; file bread crumbs
(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode 1))

;; note taking that you'll never use
(use-package org-roam
  :ensure t
  :init
  (when (not (file-directory-p "~/.notes"))
    (make-directory "~/.notes"))
  (setq org-roam-directory (file-truename "~/.notes"))
  (setq org-defaults-notes-file (file-truename "~/.notes"))
  (setq org-roam-dailies-directory "daily/")
  (setq org-agenda-files (list org-roam-directory))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))
  :config
  (add-hook 'org-roam-post-find-hook
            (lambda (node)
              (org-roam-update-node-prop node :last-visited (current-time))))
  (setq org-return-follows-link t)
  (org-roam-db-autosync-mode))

;; file explorer
(use-package treemacs
  :hook  (emacs-startup . treemacs)
  :ensure t
  :config
  (define-key treemacs-mode-map (kbd "C-h") 'tmux-navigate-left)
  (define-key treemacs-mode-map (kbd "C-l") 'tmux-navigate-right))

(use-package treemacs-evil :ensure t)


;; file navigation: ido, projectile, flx-ido, ido-vertical-mode, ivy
(use-package ido
  :defer 2
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere 1)

  (defun vim-like-ido-keys ()
    "Add vim like keybindings for ido."
    (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
    (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
    (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))

  (add-hook 'ido-setup-hook 'vim-like-ido-keys))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-enable-caching t)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-enable-caching 'persistent)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-files "*.png"))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces t)
  (setq ido-enable-flex-matching t))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (define-key ivy-minibuffer-map (kbd "C-n") nil)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-p") nil)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))

  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")

  (defun my-action-1 (x)
    "action one was invoked")

  (defun my-action-2 (x)
    (message "action-2: %s" x))

  (defun my-action-3 (x)
    (message "action-3: %s" x))

  ;; This is a sample function that shows the power of
  ;; Ivy. This method can be invoked via M-x / :.
  ;; Once invoked, it will present an auto complete list
  ;; with three values: foo bar baz.
  ;; On selecting a value, the respective `action(x)` callback
  ;; will be invoked with the string value they entered.
  (defun my-command-with-3-actions ()
    (interactive)
    (ivy-read "test: "
              '("foo" "bar" "baz")
              :action
              (lambda (x)
                (cond
                 ((string= x "foo") (message "foo was called"))
                 ((string= x "bar") (message "bar was called"))
                 ((string= x "baz") (message "baz was called"))
                 (t (message "no match")))))))

;; some packages to help you with editing lisp
;; BuT tHE PaRENtHeSiS!!!
(use-package lispyville
  :ensure t
  :hook (emacs-lisp-mode . lispyville-mode))

(use-package prism
  :ensure t
  :hook (emacs-lisp-mode . prism-mode))

(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode . paredit-mode))

;; contextual help
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))
