(defmacro alias (from to)
  `(defun ,from ()
     (interactive)
     (call-interactively #',to)))

(defmacro foreach (i list &rest body)
  `(dolist (,i ,list)
     ,@body))

(defmacro call-i (name)
  `(call-interactively #',name))

(alias df describe-function)
(alias dm describe-mode)
(alias ef eval-defun)
(alias eb eval-buffer)
(alias er eval-region)
(alias dk describe-key)
(alias dv describe-variable)
(alias ee eval-expression)
(alias ic projectile-invalidate-cache)

(defun tmux-navigate (direction)
  (let ((cmd (concat "windmove-" direction)))
    (condition-case nil
     (progn
       (funcall (read cmd))
       (when (string= "*Messages*" (buffer-name))
         (evil-leader-mode 1)))
     (error
      (tmux-command direction)))))

(defun tmux-command (direction)
  (shell-command-to-string
   (concat "tmux select-pane -" (tmux-direction direction))))

(defun tmux-direction (direction)
  (upcase (substring direction 0 1)))

(defun tmux-navigate-left ()
  (interactive)
  (tmux-navigate "left"))

(defun tmux-navigate-right ()
  (interactive)
  (tmux-navigate "right"))

(defun tmux-navigate-up ()
  (interactive)
  (tmux-navigate "up"))

(defun tmux-navigate-down ()
  (interactive)
  (tmux-navigate "down"))

(defun amir/split-and-find ()
  "Opens a new split window and brings up dired so I can search."
  (interactive)
  (progn
    (evil-window-vsplit)
    (dired ".")
    (projectile-find-file)))

(defun amir/next-code-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name)))
    (next-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name))))
      (next-buffer))))

(defun amir/previous-code-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name)))
    (previous-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name))))
      (previous-buffer))))

(defun amir/zoom-buffer ()
  (interactive)
  (progn
    (evil-window-set-width (frame-width))
    (evil-window-set-height (frame-height))
    (redraw-display)))

(defun amir/resize-equal ()
  (interactive)
  (balance-windows)
  (redraw-display))

(defun amir/touch ()
  "Run touch command on current file."
  (interactive)
  (when buffer-file-name
    (shell-command (concat "touch " (shell-quote-argument buffer-file-name)))
    (clear-visited-file-modtime)))
