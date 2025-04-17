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
