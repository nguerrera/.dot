;; -*- lexical-binding: t; -*-

(defun ng-home (&optional arg)
  "Move back to indentation. If already there, move to beginning
of line. If a region is selected, create multiple cursors in the
selected lines before moving."
  (interactive "p")
  (setq arg (or arg 1))
  (if (/= arg 1)
      (move-beginning-of-line arg)
    (progn
      (if (use-region-p)
          (ng-mc-edit-lines))
      (if (ng-at-indentation-p)
          (move-beginning-of-line 1)
        (back-to-indentation)))))

(defun ng-end (&optional arg)
  "Move to end of line. If a region is selected, create multiple
cursors in the selected lines before moving."
  (interactive "p")
  (setq arg (or arg 1))
  (if (/= arg 1)
      (move-end-of-line arg)
    (if (use-region-p)
        (ng-mc-edit-lines))
    (move-end-of-line 1)))

(defun ng-at-indentation-p ()
  "Check if the cursor is already back to indentation."
  (interactive)
  (let ((old-column (current-column)))    
    (save-excursion
      (back-to-indentation)
      (= old-column (current-column)))))

(defvar-local ng-enable-multiple-cursor-keys t
  "Set to nil in modes where we don't want the multiple cursor
behaviors of certain keybindings.")

(defun ng-add-cursor-on-click (event)
  "Add another cursor on click."
  (interactive "e")
  (if (ng-use-multiple-cursor-keys)
      (mc/add-cursor-on-click event)))

(defun ng-use-multiple-cursor-keys ()
  "Determines if multiple cursor behaviors should be used."
  (and (fboundp 'mc/edit-lines) ng-enable-multiple-cursor-keys))

(defun ng-rectangle-self-insert ()
  "Hook this to self-insert-command in rectangle-mark-mode. It
deletes the current rectangle, then switches to multiple
cursors. This achieves typing over rectangular selection as in
modern editors."
  (interactive)
  (when (ng-use-multiple-cursor-keys)
    (command-execute 'delete-rectangle)
    (mc/edit-lines))
  (command-execute 'self-insert-command))

(defun ng-mc-edit-lines (&optional arg)
  "Add cursors to lines in region."
  (interactive "p")
  (if (ng-use-multiple-cursor-keys)
      (mc/edit-lines)))

(defun ng-rectangle-mark-down (&optional arg)
  "Extend rectangle down. Start rectangle-mark-mode if not already started."
  (interactive "p")
  (when (ng-use-multiple-cursor-keys)
    (unless rectangle-mark-mode (rectangle-mark-mode 1))
    (rectangle-next-line arg)))
                             
(defun ng-rectangle-mark-up (&optional arg)
  "Extend rectangle up. Start rectangle-mark-mode if not already started."
  (interactive "p")
  (when (ng-use-multiple-cursor-keys)
    (unless rectangle-mark-mode (rectangle-mark-mode 1))
    (rectangle-previous-line arg)))

(defun ng-rectangle-mark-left (&optional arg)
  "Extend rectangle left. Start rectangle-mark-mode if not already started."
  (interactive "p")
  (when (ng-use-multiple-cursor-keys)
    (unless rectangle-mark-mode (rectangle-mark-mode 1))
    (rectangle-left-char arg)))

(defun ng-rectangle-mark-right (&optional arg)
  "Extend rectangle rig. Start rectangle-mark-mode if not already started."
  (interactive "p")
  (when (ng-use-multiple-cursor-keys)
    (unless rectangle-mark-mode (rectangle-mark-mode 1))
    (rectangle-right-char arg)))

(defun ng-C-w (&optional arg)
  "If there is an active region, kill it. Otherwise, kill the
preceding word. Allows C-w to backward-kill-word like it does on
the terminal, but still kill-region when something is selected."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun ng-C-k (&optional arg)
  "If there is an active region, kill it. Otherwise, kill the
following line. Adds symmetry to the mnemonic C-k=kill:C-y==yank."
  (interactive "P")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-line arg)))

(defun ng-done ()
  "Save the current buffer and kill it. Notify client that we're
done if applicable."
  (interactive)
  (save-buffer)
  (cond ((and (boundp 'with-editor-mode) with-editor-mode)
         (with-editor-finish nil))
        (server-buffer-clients
          (server-done))
        (t
         (kill-current-buffer))))

(defmacro ng-unbind-keys (&rest keys)
  "Unbind multiple keys."
  `(progn
     ,@(mapcar (lambda (k) `(unbind-key ,k))
               keys)))

(defun ng-mac-activate (&rest r)
  "Bring Emacs into focus"
  (ns-do-applescript
   "tell application \"Emacs\" to activate"))

(defun ng-unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single
line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(provide 'ng-lib)
