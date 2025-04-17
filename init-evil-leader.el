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
    ;; magit status (git status)
    "s" 'magit-status
    ;; find or create note
    "r" 'org-roam-node-find
    ;; this will map <leader>h to invoke justin/hello-world
    "h" 'justin/hello-world
    ;; search for file in a new split
    "m" 'amir/split-and-find
    ;; search for file
    "g" 'projectile-find-file))
