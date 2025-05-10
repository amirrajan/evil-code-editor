;; install origami (used to control folding of all the things)
(use-package origami
  :ensure t
  :config
  (global-origami-mode))

;; package gives you multi level undo
;; going into normal mode and typing `:undo-tree-visualize`
;; will present you a tree with visual history of your changes
;; if you find that you ever need this
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; install evil which gives you modal editing
(use-package evil
  :ensure t
  :after undo-tree
  :init
  ;; methods within this section are invoked before the package loads
  ;; this registers evil so that it overrides all the bullshit
  ;; shortcut keys across emacs
  ;; if you ever want to know what something does, go into normal mode
  ;; and type `:describe-variable`, press enter, and type what variable you
  ;; want documentation on.

  ;; there's also:
  ;; - `:describe-function`:  gets docs on a function
  ;; - `:describe-char`: describes the char under the cursor (in cause you want to change it's color theme)
  ;; - `:describe-key`: after pressing a key you will be provided with the function that was invoked
  ;; - `:describe-mode`: gives you information about all the plugins that are currently loaded for the current file
  (setq evil-want-C-i-jump nil)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  ;; methods within this section are invoked after the plugin has been loaded
  ;; enable evil mode immediately
  (evil-mode 1)

  ;; set the undo system for evil to the fancy undo/redo package
  (evil-set-undo-system 'undo-tree)

  ;; this function allows you to spam the escape key to quit whatever
  ;; window, minibuffer, dialog, etc that you're in
  (defun minibuffer-keyboard-quit ()
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; with the function defined, make sure that the pressing escape gets out
  ;; of "all the things"
  ;; you can view all keyboard shortcuts by using `:describe-keymap`
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  ;; override the built in folding mechanism to use origami (which is fancier)
  ;; you can view all keyboard shortcuts by using `:describe-keymap`
  (define-key evil-normal-state-map (kbd "za") 'origami-toggle-node)
  (define-key evil-normal-state-map (kbd "zR") 'origami-open-all-nodes)
  (define-key evil-normal-state-map (kbd "zM") 'origami-close-all-nodes)
  (define-key evil-normal-state-map (kbd "zr") 'origami-open-node-recursively)
  (define-key evil-normal-state-map (kbd "zm") 'origami-close-node-recursively)
  (define-key evil-motion-state-map (kbd "za") 'origami-toggle-node)
  (define-key evil-motion-state-map (kbd "zR") 'origami-open-all-nodes)
  (define-key evil-motion-state-map (kbd "zM") 'origami-close-all-nodes)
  (define-key evil-motion-state-map (kbd "zr") 'origami-open-node-recursively)
  (define-key evil-motion-state-map (kbd "zm") 'origami-close-node-recursively)

  (define-key evil-visual-state-map (kbd "ESC") 'evil-normal-state)

  ;; the following customization allows you to move accross buffer splits and tmux splits
  ;; with ctrl+hjkl
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings))

  (define-key evil-normal-state-map
              (kbd "C-h")
              'tmux-navigate-left)

  (define-key evil-normal-state-map
              (kbd "C-j")
              'tmux-navigate-down)

  (define-key evil-normal-state-map
              (kbd "C-k")
              'tmux-navigate-up)

  (define-key evil-normal-state-map
              (kbd "C-l")
              'tmux-navigate-right))

  ;; change the cursor in terminal based on mode
(use-package evil-terminal-cursor-changer
  :ensure t
  :init
  (evil-terminal-cursor-changer-activate)
  (setq evil-motion-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-emacs-state-cursor  'hbar))

;; install the evil collection which adds vim bindings to popular packages (like magit)
(use-package evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-exchange
  :ensure t
  :config
  (evil-exchange-install))
