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

(defvar-local ng-enable-multiple-cursor-keys t
  "Whether various personal keybindings are overloaded to use
multiple cursors or rectangles.")
(defun ng-mc-edit-lines ()
  (interactive)
  (if (and (fboundp 'mc/edit-lines)
           ng-enable-multiple-cursor-keys)
      (mc/edit-lines)))

(defun ng-add-cursor-on-click (event)
  (interactive "e")
  (if (and (fboundp 'mc/add-cursor-on-click)
           ng-enable-multiple-cursor-keys)
      (mc/add-cursor-on-click event)))

(defun ng-at-indentation-p ()
  "Check if the cursor is already back to indentation."
  (interactive)
  (let ((old-column (current-column)))    
    (save-excursion
      (back-to-indentation)
      (= old-column (current-column)))))

(defun ng-rectangle-mark-down (&optional arg)
  (interactive "p")
  (when ng-enable-multiple-cursor-keys
    (unless cua--rectangle (cua-set-rectangle-mark))
    (cua-resize-rectangle-down arg)))
                             
(defun ng-rectangle-mark-up (&optional arg)
  (interactive "p")
  (when ng-enable-multiple-cursor-keys
    (unless cua--rectangle (cua-set-rectangle-mark))
    (cua-resize-rectangle-up arg)))

(defun ng-rectangle-mark-left (&optional arg)
  (interactive "p")
  (when ng-enable-multiple-cursor-keys
    (unless cua--rectangle (cua-set-rectangle-mark))
    (cua-resize-rectangle-left arg)))

(defun ng-rectangle-mark-right (&optional arg)
  (interactive "p")
  (when ng-enable-multiple-cursor-keys
    (unless cua--rectangle (cua-set-rectangle-mark))
    (cua-resize-rectangle-right arg)))

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
  "Save the current buffer, kill it, and delete its
window. Notify client that we're done."
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

(provide 'ng-lib)
