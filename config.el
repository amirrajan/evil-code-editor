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

