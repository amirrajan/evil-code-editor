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

