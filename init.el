;; import the package manager
(require 'package)

(setq warning-minimum-level :error)
(setq byte-compile-warnings nil)

;; set package sources
(push '("melpa" . "http://melpa.org/packages/") package-archives)
(push '("org" . "http://orgmode.org/elpa/") package-archives)
(push '("melpa-stable" . "https://stable.melpa.org/packages/") package-archives)

;; initialize the package manager
(package-initialize)

;; check to see if the package repos have been downloaded, if not then do so
(unless package-archive-contents (package-refresh-contents))

;; install use-package (which helps keep package definitions organized/idempotent)
(unless (package-installed-p 'use-package) (package-install 'use-package))

(use-package load-relative :ensure t)
(when (package-installed-p 'evil-mode) (evil-mode 1))

;; load the rest of the config wrapped in a try catch
(condition-case
    err
    (progn
      (load-relative "./init-functions.el")
      (load-relative "./init-editor.el")
      (load-relative "./init-evil.el")
      (load-relative "./init-evil-leader.el")
      (load-relative "./init-code.el"))
  (error
   (let ((buffer (get-buffer-create "*init error*")))
     (with-current-buffer buffer
       (insert (format "Error while loading (start emacs with --debug-init to debug): %s" err)))
     (display-buffer buffer))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(gptel evil-leader evil-collection undo-tree origami evil-visual-mark-mode)))
