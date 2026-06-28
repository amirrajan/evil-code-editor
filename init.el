;; -*- lexical-binding: t; -*-

(require 'package)
(setq package-check-signature nil)
(setq gc-cons-threshold (* 50 1000 1000))
(setq custom-file (concat user-emacs-directory "init-custom.el"))
(load custom-file)

(push '("melpa" . "http://melpa.org/packages/") package-archives)
(push '("org" . "http://orgmode.org/elpa/") package-archives)
(push '("melpa-stable" . "https://stable.melpa.org/packages/") package-archives)

(package-initialize)

(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(unless (package-installed-p 'evil) (package-install 'evil))

(load (concat user-emacs-directory "config.el"))

(setq gc-cons-threshold (* 2 1000 1000))
(put 'dired-find-alternate-file 'disabled nil)
