;; source control
(use-package magit :ensure t)

(use-package lsp-mode :ensure t)

;; auto completion
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'org-mode-hook (lambda () (company-mode 0)))
  (setq company-backends '(company-bbdb
                           company-semantic company-cmake company-capf company-files
                           (company-dabbrev-code company-gtags company-etags company-yasnippet company-keywords)
                           company-oddmuse company-dabbrev))

  (setq company-transformers '(delete-consecutive-dups
                               company-sort-by-occurrence))

  (setq company-idle-delay 0.03)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers 't)
  (setq gc-cons-threshold 20000000)
  (delq 'company-preview-if-just-one-frontend company-frontends)
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))

  ;; (define-key company-active-map "C-r" 'company-complete)

  (setq company-dabbrev-downcase nil)

  ;; unmap ctrl-n so that it doesn't do anything
  (define-key company-active-map (kbd "C-n") nil)
  (define-key company-active-map (kbd "C-p") nil)
  (define-key company-active-map (kbd "C-f") 'company-search-candidates)

  (define-key company-active-map (kbd "<backtab>") 'company-select-next)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

;; chat gpt
(use-package gptel
  :ensure t
  :defer
  :config
  (add-hook 'gptel-mode-hook 'visual-line-mode))

(use-package terraform-mode
  :ensure t)
