;; -*- lexical-binding: t; -*-

(use-package editorconfig
  :config
  ;; remove tool bar, menu bar, and start up screen
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (setq inhibit-splash-screen t)

  ;; line configurations
  (setq-default truncate-lines t)
  (setq long-line-threshold nil)

  ;; expand tab to spaces
  (setq-default indent-tabs-mode nil)

  ;; show trailing white spaces
  (setq show-trailing-whitespace t)

  ;; display line numbers
  (global-display-line-numbers-mode)

  ;; auto refresh files
  (setq global-auto-revert-non-file-buffers t)
  (setq large-file-warning-threshold 20000000)

  ;; backup files
  (setq create-lock-files nil)
  (setq auto-save-file-name-transforms (list (list ".*" (expand-file-name "auto-save-list" user-emacs-directory) t)))
  (setq backup-directory-alist '(("." . (expand-file-name "backups" user-emacs-directory))))
  (setq backup-by-copying t     ; don't clobber symlinks
        kept-new-versions 10    ; keep 10 latest versions
        kept-old-versions 10    ; don't bother with old versions
        delete-old-versions t   ; don't ask about deleting old versions
        version-control t       ; number backups
        vc-make-backup-files t))

;; ================================
;; BEGIN: make emacs look pretty
;; ================================
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :ensure t
  :init
  (defvar nerd-icons/pragmata-alist '(("nf-md-image" . "")))
  (nerd-icons-define-icon pragmata nerd-icons/pragmata-alist nerd-icons-font-family "PragmataPro Mono Liga")
  (add-to-list 'nerd-icons-extension-icon-alist
               '("png" nerd-icons-pragmata "nf-md-image" :face nerd-icons-blue))
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;; ================================
;; END: make emacs look pretty
;; ================================

;; ================================
;; BEGIN: VIM
;; ================================

;; file jumping and fuzzy find
(use-package projectile
  :defer t
  :ensure t
  :custom
  (projectile-globally-ignored-directories
   '("node_modules" "^\\.idea$" "^\\.vscode$" "^\\.ensime_cache$"
     "^\\.eunit$" "^\\.git$" "^\\.hg$" "^\\.fslckout$" "^_FOSSIL_$"
     "^\\.bzr$" "^_darcs$" "^\\.pijul$" "^\\.tox$" "^\\.svn$"
     "^\\.stack-work$" "^\\.ccls-cache$" "^\\.cache$" "^\\.clangd$"
     "*.vs"))
  (projectile-mode t nil (projectile))
  :config
  (projectile-global-mode)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-enable-caching t)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-enable-caching 'persistent)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-sort-order 'recently-active)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "backups")
  (add-to-list 'projectile-globally-ignored-files "*.png"))

(use-package ivy
  :ensure t
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package consult
  :ensure t
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  (ivy-mode 1))

(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-expand-substring 'substring)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode))

;; folding package
(use-package origami
  :ensure t
  :config
  (global-origami-mode))

;; tree based undo system
(use-package undo-fu
  :ensure t
  :custom
  (undo-fu-allow-undo-in-region t)
  (undo-limit 12000000))

(use-package undo-fu-session
  :ensure t
  :custom
  (undo-fu-session-temp-directories '("~/.emacs.d/meow" "/tmp" "/dev/shm"))
  :config
  (global-undo-fu-session-mode))

(use-package vundo
  :ensure t
  :custom
  (vundo-compact-display t)
  (vundo-glyph-alist
   '((selected-node . 9679) (node . 9675) (horizontal-stem . 9472)
     (vertical-stem . 9474) (branch . 9500) (last-branch . 9492))))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-default-cursor nil)
  (evil-want-fine-undo t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-fu)

  ;; Make movement keys work like they should
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  ;; (setq-default evil-cross-lines t)
  (define-key evil-normal-state-map (kbd "za") 'origami-toggle-node)
  (define-key evil-normal-state-map (kbd "zR") 'origami-open-all-nodes)
  (define-key evil-normal-state-map (kbd "zM") 'origami-close-all-nodes)
  (define-key evil-normal-state-map (kbd "zr") 'origami-open-node-recursively))

;; like vim matchit
(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode t))

;; cursor change in terminal
(use-package evil-terminal-cursor-changer
  :vc (:url "https://github.com/7696122/evil-terminal-cursor-changer"
       :rev :newest
       :branch "master")
  :ensure t
  :init
  (evil-terminal-cursor-changer-activate)
  (setq evil-motion-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-emacs-state-cursor  'hbar))

;; make all of emacs with vim idioms
(use-package evil-collection
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :custom
  (global-evil-collection-unimpaired-mode t)
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;; like vim-surround
(use-package evil-surround
  :ensure t
  :custom
  (global-evil-surround-mode t)
  :config
  (global-evil-surround-mode 1))

;; like vim-exchange
(use-package evil-exchange
  :ensure t
  :config
  (evil-exchange-install))

;; map backslash to secondary leader key
(use-package evil-leader
  :ensure t
  :vc (:url "https://github.com/cofi/evil-leader"
       :rev :newest
       :branch "master")
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "\\"))

;; easy motion package analogs
(use-package avy
  :defer t
  :ensure t
  :config
  (setq avy-styles-alist '((avy-goto-word-0 . at-full) (avy-goto-line . at-full)))
  (setq avy-all-windows nil))

;; easy motion package analogs
(use-package flash :ensure t)

;; map primary leader key to space
(use-package general
  :ensure t
  :config
  (general-evil-setup t)

  (general-create-definer backslash-leader :prefix "SPC")

  (defun amir/next-code-buffer ()
      (interactive)
      (let (( bread-crumb (buffer-name)))
        (next-buffer)
        (while (and (not string-match-p "TAGS" (buffer-name))
                    (string-match-p "^\*" (buffer-name))
                    (not (equal bread-crumb (buffer-name))))
          (next-buffer))))

  (defun amir/previous-code-buffer ()
    (interactive)
    (let (( bread-crumb (buffer-name)))
      (previous-buffer)
      (while (and (not string-match-p "TAGS" (buffer-name))
                  (string-match-p "^\*" (buffer-name))
                  (not (equal bread-crumb (buffer-name))))
        (previous-buffer))))

  (backslash-leader
   :states '(normal visual)
   :keymaps 'override
   "b" '("goto buffer" . consult-buffer)
   "g" '("find file" . consult-project-buffer)
   "o" '("outline current document" . consult-outline)
   "p" '("go to next buffer" . amir/next-code-buffer)
   "q" '("go to previous buffer" . amir/previous-code-buffer)
   "w" 'flash-evil-jump
   "v" 'consult-imenu
   "W" 'avy-goto-word-1-above
   "/" 'consult-recent-file
   ";" 'projectile-switch-project))
