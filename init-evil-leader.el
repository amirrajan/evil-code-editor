;; overrides the leader key
(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode)

  ;; set your leader key to "SPC"
  (evil-leader/set-leader "SPC")

  ;; you can wire up functions to be invoked
  ;; with leader key combinations

  ;; here is an example, you can define a function
  ;; that logs a message

  ;; function definition
  ;; this function can be called by going into normal mode and typing
  ;; `:justin/hello-world` followed by enter
  (defun justin/hello-world ()
    ;; mark the method as "public" (can be called interactively)
    (interactive)

    ;; do stuff here
    (message "hello world"))

  ;; here is how you would wire this function up to a leader key
  (evil-leader/set-key
    "h" '("git history this file/directory" .  magit-log-buffer-file)
    "." '("goto directory of this file". dired)
    "s" '("git status" . magit-status)
    "r" '("create or find note" . org-roam-node-find)
    "m" '("vsplit and find file" . amir/split-and-find)
    "g" '("find file" . projectile-find-file)
    "p" '("switch to next buffer" . amir/next-code-buffer)
    "q" '("switch to previous buffer" . amir/previous-code-buffer)
    ";" '("switch project" . projectile-switch-project)))
